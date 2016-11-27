{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Onibus
import Handlers.Viagem
import Handlers.Pessoa
import Handlers.Login
import Handlers.Reserva

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
        <h1>Sistema de reserva de onibus
        <ul>
            <li><a href=@{LoginR}>Login
            <li><a href=@{CadastroR}>Primeiro Acesso
            <li><a href=@{OnibusR}>Cadastro da Frota
            <li><a href=@{ViagemR}>Cadastro de Viagem
            <li><a href=@{ReservaR}>Reserva de poltrona
        $maybe _ <- sess
            <form action=@{LoginR} method=post>
            <input type="submit" value="Logout">
     |]
