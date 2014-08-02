{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Welcome where

import Import

import qualified Data.Text as T
import qualified Data.Map.Lazy as M

import qualified Data.ByteString.Char8 as C

bsToText :: C.ByteString -> Text
bsToText = T.pack . C.unpack

getWelcomeR :: Handler Html
getWelcomeR = do
    s <- getSession

    let
        cn   = bsToText <$> M.lookup "aaf_cn"   s
        mail = bsToText <$> M.lookup "aaf_mail" s

    case (cn, mail) of (Just cn', Just mail') ->defaultLayout [whamlet|
        <h1>Welcome
        <br>
        <p> Hello #{cn'}, you are identified as #{mail'}.
        |]
                       _ -> permissionDenied $ T.pack $ show s

