{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Cidade where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formCidade :: Form Cidade
formCidade = renderDivs $ Cidade <$>
            areq textField "Nome da Cidade:     " Nothing <*>
            areq textField "Nome do Estado:     " Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla do Estado:      ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing

getCidadeR :: Handler Html
getCidadeR = do
             (widget, enctype) <- generateFormPost formCidade
             defaultLayout $ widgetForm CidadeR enctype widget "Cadastro de Cidades" "Cadastrar"

postCidadeR :: Handler Html
postCidadeR = do
                ((result, _), _) <- runFormPost formCidade
                case result of
                    FormSuccess cidade -> do
                       runDB $ insert cidade
                       defaultLayout [whamlet|
                           <h1>Cidade cadastrada com sucesso. 
                       |]
                    _ -> redirect CidadeR