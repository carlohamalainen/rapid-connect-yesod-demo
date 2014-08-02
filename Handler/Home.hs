{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Data.Text as T

rapidConnectAuthURL :: T.Text
rapidConnectAuthURL = "https://rapid.test.aaf.edu.au/jwt/authnrequest/research/CHANGE_ME"

getHomeR :: Handler Html
getHomeR = do
    defaultLayout [whamlet|<h1>Rapid Connect Yesod Demo</h1>
    <br>
    <br>
    <p>Click <a href=#{rapidConnectAuthURL}>here</a> to log in using AAF Rapid Connect.
|]
