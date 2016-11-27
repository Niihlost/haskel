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
            areq textField "Marca" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Placa",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","7")]} Nothing

getOnibusR :: Handler Html
getOnibusR = do
             (widget, enctype) <- generateFormPost formOnibus
             defaultLayout $ widgetForm OnibusR enctype widget "Cadastro de Onibus"

postOnibusR :: Handler Html
postOnibusR = do
                ((result, _), _) <- runFormPost formOnibus
                case result of
                    FormSuccess onibus -> do
                       runDB $ insert onibus
                       defaultLayout [whamlet|
                           <h1> Onibus n° #{OnibusId onibus} Inserido com sucesso. 
                       |]
                    _ -> redirect OnibusR