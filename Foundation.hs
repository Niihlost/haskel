{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Pessoa
   nome    Text
   idade   Int
   email   Text
   senha   Text
   UniqueEmail email
   deriving Show

Onibus
    marca Text
    placa Text sqltype=varchar(7)
    deriving Show
    
Cidade
    nome Text
    estado Text
    sigla_estado Text sqltype=varchar(2)
    deriving Show
    
Viagem
    origem CidadeId
    destino CidadeId
    onibusid OnibusId
    preco Double
    --UniqueOrigemDestino origem destino
    deriving Show
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "routes")

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized CadastroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized _ _ = estaAutenticado

estaAutenticado :: Handler AuthResult
estaAutenticado = do
   msu <- lookupSession "_ID"
   case msu of
       Just _ -> return Authorized
       Nothing -> return AuthenticationRequired

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y z = $(whamletFile "templates/form.hamlet")