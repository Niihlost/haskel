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
            areq textField "Origem" Nothing <*>
            areq textField "Destino" Nothing <*>
            areq (selectField bus) "Onibus" Nothing <*>
            areq doubleField "Preço" Nothing

bus = do
    entidades <- runDB $ selectList [] [Asc OnibusMarca] 
    optionsPairs $ fmap (\ent -> (onibusMarca $ entityVal ent, entityKey ent)) entidades

getViagemR :: Handler Html
getViagemR = do
             (widget, enctype) <- generateFormPost formViagem
             defaultLayout $ widgetForm ViagemR enctype widget "Cadastro de Viagem"

postViagemR :: Handler Html
postViagemR = do
                ((result, _), _) <- runFormPost formViagem
                case result of
                    FormSuccess viagem -> do
                       runDB $ insert viagem
                       defaultLayout [whamlet|
                           <h1>Viagem Inserido com sucesso. 
                       |]
                    _ -> redirect ViagemR