{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Log where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formReserva :: Form Viagem
formReserva = renderDivs $ areq (selectField viagens) "Viagem" Nothing

viagens = do
    entidades <- runDB $ selectList [] [Asc ViagemDestino] 
    optionsPairs $ Prelude.map (\v -> (mconcat [viagemOrigem $ entityVal v, " - ", viagemDestino $ entityVal v], entityKey v)) entidades

getLogR :: Handler Html
getLogR = do
             (widget, enctype) <- generateFormPost formReserva
             defaultLayout $ widgetForm LogR enctype widget "Cadastro de Reserva"

postLogR :: Handler Html
postLogR = do
                ((result, _), _) <- runFormPost formReserva
                case result of
                    FormSuccess reserva -> do
                        userId <- lookupSession "_ID"
                        case userId of
                            Nothing -> redirect LoginR
                            Just userStr -> do
                                pid <- (return $ read $ unpack userStr) :: Handler PessoaId
                                sequence $ fmap (\vid -> runDB $ insert $ Log pid vid) reserva
                                defaultLayout [whamlet| <h1> Reservas cadastradas com sucesso! |]
                    _ -> redirect ViagemR