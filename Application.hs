{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Onibus
import Handlers.Viagem
import Handlers.Pessoa
import Handlers.Login
import Handlers.Cidade

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
        <center>
        <menu>
        <h1>Sistema de reserva de Viagens
            <ul>
                <li><a href=@{LoginR}>Login
                <li><a href=@{CadastroR}>Primeiro Acesso
                <li><a href=@{MenuOnibusR}>Frota
                <li><a href=@{MenuCidadeR}>Cidades
                <li><a href=@{ViagemR}>Reserva de Viagem
                
        $maybe _ <- sess
            <br><form action=@{LogoutR} method=post>
            <input type="submit" value="Logout">
     |]
