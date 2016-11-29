{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Onibus where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formOnibus :: Form Onibus
formOnibus = renderDivs $ Onibus <$>
            areq textField "Marca:     " Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Placa:     ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","7")]} Nothing

getOnibusR :: Handler Html
getOnibusR = do
             (widget, enctype) <- generateFormPost formOnibus
             defaultLayout $ widgetForm OnibusR enctype widget "Cadastro de Onibus" "Cadastrar"

postOnibusR :: Handler Html
postOnibusR = do
                ((result, _), _) <- runFormPost formOnibus
                case result of
                    FormSuccess onibus -> do
                       runDB $ insert onibus
                       defaultLayout [whamlet|
                           <h1>Onibus Inserido com sucesso.
                           <h3><a href=@{HomeR}>Página principal   <h3><a href=@{MenuOnibusR}>Voltar
                       |]
                    _ -> redirect OnibusR
                    
getListarOnibusR :: Handler Html
getListarOnibusR = do
                listaP <- runDB $ selectList [] [Asc OnibusMarca]
                defaultLayout $ do
                    [whamlet|
                        <center>
                        <h1>Frota cadastradas:
                        $forall Entity pid onibus <- listaP
                            #{onibusMarca onibus} - 
                            #{onibusPlaca onibus} <br>
                        <br><form action=@{MenuOnibusR} method=get>
                        <input type="submit" value="Menu">
                        |]
                    
getMenuOnibusR :: Handler Html
getMenuOnibusR = do
        defaultLayout $ do
            [whamlet|
                Menu de Onibus
                <ul>
                    <li><a href=@{OnibusR}>Cadastro de Onibus
                    <li><a href=@{ListarOnibusR}>Lista de Onibus
                    
                    <br><form action=@{HomeR} method=get>
                    <input type="submit" value="Home">
            |]