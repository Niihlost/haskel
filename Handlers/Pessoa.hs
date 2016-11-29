{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Pessoa where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome:     " Nothing <*>
             areq intField "Idade:     " Nothing <*>
             areq emailField "E-mail:     " Nothing <*>
             areq passwordField "Password:     " Nothing

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroR enctype widget "Cadastro de Pessoas" "Cadastrar"

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout $ do
                 $(whamletFile "templates/lista.hamlet") 
                 toWidget $(luciusFile "templates/lista.lucius")

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       unicoEmail <- runDB $ getBy $ UniqueEmail (pessoaEmail pessoa)
                       case unicoEmail of
                           Just _ -> redirect CadastroR
                           Nothing -> do 
                              pid <- runDB $ insert pessoa 
                              redirect HomeR
                    _ -> redirect CadastroR
