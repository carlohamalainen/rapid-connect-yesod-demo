{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.AuthJwt where

import Import
import Network.HTTP.Types (status200, status403)
import Control.Monad (join, when, mzero)
import Data.Maybe
import Data.Aeson

-- import qualified Web.JWT as JWT

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

import Data.Text.Encoding

import qualified Data.Text as T
import qualified Crypto.JWT as JWT

import qualified Crypto.JOSE as JOSE
import qualified Data.HashMap.Lazy as HM

import Crypto.JOSE.JWA.JWK
import Crypto.JOSE.Types

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.URI (parseURI)

-- for string to char8
import Data.ByteString.Internal (unpackBytes)
import Data.ByteString.Char8 (pack)
import GHC.Word (Word8)

getRight :: Either a b -> b
getRight (Right b) = b
getRight _ = error "derp"

configIss :: T.Text
configIss = "https://rapid.test.aaf.edu.au" -- or maybe "https://rapid.aaf.edu.au"

configAudience :: T.Text
configAudience = "https://example.com/rc"

secret :: B.ByteString
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

strToWord8s :: String -> [Word8]
strToWord8s = unpackBytes . pack

decodeJWT :: T.Text -> Either JOSE.Error JWT.JWT
decodeJWT jwtInput = JOSE.decodeCompact $ BL.pack $ strToWord8s $ T.unpack jwtInput

encodeJWK :: B.ByteString -> JOSE.JWK
encodeJWK s = JOSE.JWK k z z z z z z z z
  where
    z = Nothing
    k = OctKeyMaterial $ OctKeyParameters Oct $ Base64Octets s

postAuthJwtR :: Handler ()
postAuthJwtR = do
    jwtInput <- lookupPostParam "assertion"

    when (isNothing jwtInput) $ permissionDenied "No assertion found in POST."

    case decodeJWT (fromJust jwtInput) of
         Left  _   -> permissionDenied "Could not decode JWT."
         Right jwt -> do
            let
                jwk = encodeJWK secret

                -- Things that I have to look up:
                claimset = JWT.jwtClaimsSet jwt
                iss      = JWT.claimIss claimset
                audience = JWT.claimAud claimset

                -- In the unregistered claims I will find things about
                -- the user's institutional email, staff/student affiliation, etc.
                unregisteredClaims = JWT.unregisteredClaims claimset
                attributes         = HM.lookup "https://aaf.edu.au/attributes" unregisteredClaims

                isvalid = JWT.validateJWSJWT jwk jwt

            when (audience /= (Just $ JWT.Special $ JWT.OrURI $ URI $ fromJust $ parseURI $ T.unpack configAudience)) $
                permissionDenied $ "Not for this audience: "

            when (iss /= (Just $ JWT.OrURI $ URI $ fromJust $ parseURI $ T.unpack configIss)) $
                 permissionDenied "Issuer does not match."

            when (isNothing attributes) $
                permissionDenied "Could not lookup AAF attributes."

            when (not isvalid) $
                permissionDenied "Could not validate JWT."

            case (fromJSON $ fromJust attributes :: Result AAFAttributes) of
                Success attributes' -> do
                    -- Can set other attributes here, as required, or do auth stuff.
                    setSession "aaf_cn"   $ aafCn   attributes'
                    setSession "aaf_mail" $ aafMail attributes'
                Error e             -> permissionDenied $ "Error: JSON error when decoding attributes: " `T.append` (T.pack e)

            -- Set claimset. Beware that decodeUtf8 can explode; use decodeUtf8' or similar.
            setSession "jwt" (decodeUtf8 $ BSL.toStrict $ encode claimset)

            redirect WelcomeR
