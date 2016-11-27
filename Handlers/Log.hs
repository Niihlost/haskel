{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Log where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formReserva :: Form [ViagemId]
formReserva = renderDivs $ areq (multiSelectField viagensLista) "Lista de viagens" Nothing
              where
                viagensLista = do
                    viagensLista <- runDB $ selectList [] [Asc ViagemPreco]
                    optionsPairs $ Prelude.map (\v -> (mconcat [viagemOrigem $ entityVal v, " - ", viagemDestino $ entityVal v, " - ", pack $ show $ viagemPreco $ entityVal v], entityKey v)) viagensLista


getReservaR :: Handler Html
getReservaR = do
    (widget,enctype) <- generateFormPost formReserva
    defaultLayout $ do
        [whamlet|
            <form action=@{ReservaR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postReservaR :: Handler Html
postReservaR = do
        ((result,_),_)<- runFormPost formReserva
        case result of
            FormSuccess reservas -> do
                userId <- lookupSession "_ID"
                case userId of
                    Nothing -> redirect HomeR
                    Just userStr -> do
                        pid <- (return $ read $ unpack userStr) :: Handler PessoaId
                        sequence $ fmap (\vid -> runDB $ insert $ Log pid vid) reservas
                        defaultLayout [whamlet| <h1> Reservas cadastradas com sucesso! |]
            _ -> redirect ReservaR