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
                           <h3><a href=@{HomeR}>Página principal   <a href=@{MenuCidadeR}>Voltar
                       |]
                    _ -> redirect CidadeR
            
getListarCidadeR :: Handler Html
getListarCidadeR = do
             listaP <- runDB $ selectList [] [Asc CidadeNome]
             defaultLayout $ do
                [whamlet|
                        <center>
                        <h1>Cidades cadastradas:
                        $forall Entity pid cidade <- listaP
                            #{cidadeNome cidade} - 
                            #{cidadeEstado cidade} - 
                            #{cidadeSigla_estado cidade}<br>
                        <br><form action=@{MenuCidadeR} method=get>
                        <input type="submit" value="Menu">
                |]
                
getMenuCidadeR :: Handler Html
getMenuCidadeR = do
        defaultLayout $ do
            [whamlet|
                <h1>Menu de Cidades:
                <ul>
                    <li><a href=@{CidadeR}>Cadastro de Cidades
                    <li><a href=@{ListarCidadeR}>Lista de Cidades
                    
                    <br><form action=@{HomeR} method=get>
                    <input type="submit" value="Home">
            |]  