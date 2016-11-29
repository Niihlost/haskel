{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Viagem where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formViagem :: Form Viagem
formViagem = renderDivs $ Viagem <$>
            areq (selectField cidade) "Origem:      " Nothing <*>
            areq (selectField cidade) "Destino:      " Nothing <*>
            areq (selectField bus) "Onibus:      " Nothing <*>
            areq doubleField "Preço:      " Nothing

cidade = do
    entidades <- runDB $ selectList [] [Asc CidadeNome] 
    optionsPairs $ fmap (\ent -> (cidadeNome $ entityVal ent, entityKey ent)) entidades

bus = do
    entidades <- runDB $ selectList [] [Asc OnibusMarca] 
    optionsPairs $ fmap (\ent -> (onibusMarca $ entityVal ent, entityKey ent)) entidades

getViagemR :: Handler Html
getViagemR = do
             (widget, enctype) <- generateFormPost formViagem
             defaultLayout $ widgetForm ViagemR enctype widget "Cadastro de Viagem" "Cadastrar"

postViagemR :: Handler Html
postViagemR = do
                ((result, _), _) <- runFormPost formViagem
                case result of
                    FormSuccess viagem -> do
                       runDB $ insert viagem
                       defaultLayout [whamlet|
                           <h1>Viagem Inserido com sucesso.
                           <h2><a href=@{HomeR}>Voltar
                       |]
                    _ -> redirect ViagemR
                    
{-getListarViagemR :: Handler Html
getListarViagemR = do
                listaP <- runDB $ selectList [] [Asc ViagemOrigem]
                defaultLayout $ do
                    [whamlet|
                        <h1>Frota cadastradas:
                        $forall Entity pid viagem <- listaP
                            #{viagemOrigem listaP} - 
                            #{viagemDestino listaP} - 
                            #{viagemOnibusid listaP} - 
                            #{viagemPreco listaP}<br>
                        |]-}