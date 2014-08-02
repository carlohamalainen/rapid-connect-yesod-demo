{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.AuthJwt where

import Import
import Network.HTTP.Types (status200, status403)
import Control.Monad (join, when, mzero)
import Data.Maybe
import Data.Aeson

import qualified Web.JWT as JWT

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

import Data.Text.Encoding

configIss :: T.Text
configIss = "https://rapid.test.aaf.edu.au" -- or maybe "https://rapid.aaf.edu.au"

configAudience :: T.Text
configAudience = "https://example.com/rc"

secret :: T.Text
secret = "SECRET"

-- For a description of these attributes, see https://rapid.aaf.edu.au/developers
data AAFAttributes = AAFAttributes
    { aafEdupersontargetedid        :: T.Text
    , aafDisplayname                :: T.Text
    , aafCn                         :: T.Text
    , aafEdupersonscopedaffiliation :: T.Text
    , aafEdupersonprincipalname     :: T.Text
    , aafMail                       :: T.Text
    , aafSurname                    :: T.Text
    , aafGivenname                  :: T.Text
    }
    deriving (Show, Eq)

instance FromJSON AAFAttributes where
    parseJSON (Object v) = AAFAttributes <$>
                           v .: "edupersontargetedid" <*>
                           v .: "displayname" <*>
                           v .: "cn" <*>
                           v .: "edupersonscopedaffiliation" <*>
                           v .: "edupersonprincipalname" <*>
                           v .: "mail" <*>
                           v .: "surname" <*>
                           v .: "givenname"

    parseJSON _          = mzero

postAuthJwtR :: Handler ()
postAuthJwtR = do
    jwtInput <- lookupPostParam "assertion"

    when (isNothing jwtInput) $ permissionDenied "No assertion found in POST."

    let decodedAndVerified  = join $ JWT.decodeAndVerifySignature (JWT.secret secret) <$> jwtInput
        claimset            = JWT.claims <$> decodedAndVerified
        audience            = join $ JWT.aud <$> claimset
        attributes          = join $ Map.lookup "https://aaf.edu.au/attributes" <$> JWT.unregisteredClaims <$> claimset
        iss                 = join $ JWT.iss <$> claimset

        -- In a production system, store the jti
        -- values to guard against a replay attack.
        jti                 = join $ JWT.jti <$> claimset

    when (audience /= JWT.stringOrURI configAudience) $
        permissionDenied "Not for this audience."

    when (iss /= JWT.stringOrURI configIss) $
         permissionDenied "Issuer does not match."

    when (isNothing claimset) $
        permissionDenied "Could not decode claimset."

    -- JWT looks good. Store the attributes in the session.

    case (fromJSON $ fromJust attributes :: Result AAFAttributes) of
        Success attributes' -> do
            -- Can set other attributes here, as required, or do auth stuff.
            setSession "aaf_cn"   $ aafCn   attributes'
            setSession "aaf_mail" $ aafMail attributes'
        Error e             -> permissionDenied $ "Error: JSON error when decoding attributes: " `T.append` (T.pack e)

    -- Set claimset. Beware that decodeUtf8 can explode; use decodeUtf8' or similar.
    setSession "jwt" (decodeUtf8 $ BSL.toStrict $ encode $ fromJust claimset)

    redirect WelcomeR
