{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Onibus
import Handlers.Viagem
import Handlers.Pessoa
import Handlers.Login
import Handlers.Log

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
         $maybe _ <- sess
             <form action=@{LoginR} method=post>
                 <input type="submit" value="Logout">
     |]
