{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Yesod
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Aeson
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import GHC.Generics

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/json/#Int JsonR GET PUT DELETE
/jsonl JsonlR GET
|]

instance Yesod HelloWorld

data Person = Person {
    personId :: Int,
    name :: String,
    age :: Int
} deriving (Show,Generic)

instance FromJSON Person
instance ToJSON Person

instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field

instance ToRow Person where
    toRow d = [ toField (personId d), toField (name d), toField (age d)]

getConnectionString = do
	cnn <- connect defaultConnectInfo {
        	connectHost = "127.0.0.1"
        	, connectPort = 5432
        	, connectUser = "postgres"
        	, connectPassword = "123456"
        	, connectDatabase = "tst"
        	}
	return (cnn)

getPerson id = do
    cnn <- getConnectionString
    (x:xs) <- query cnn "select \"PersonId\", \"Name\", \"Age\" from \"Person\" where \"PersonId\" = ?" (Only (id :: Int))  :: IO [Person]
    return x

getPersonList = do
	cnn <- getConnectionString
	xs <- query cnn "select \"PersonId\", \"Name\", \"Age\" from \"Person\" where \"PersonId\" > ? order by \"Name\"" (Only (0 :: Int)) :: IO [Person]
	return xs

savePerson person = do
	cnn <- getConnectionString
	if (personId person) == 0
		then execute cnn "insert into \"Person\" (\"PersonId\", \"Name\", \"Age\") values((select max(\"PersonId\") + 1 from \"Person\"), ?, ?)" (name person :: String, age person :: Int)
		else execute cnn "update \"Person\" set \"Name\" = ?, \"Age\" = ? where \"PersonId\" = ?" (name person :: String, age person :: Int, personId person :: Int)

deletePerson id = do
	cnn <- getConnectionString
	execute cnn "delete from \"Person\" where \"PersonId\" = ?" (Only id)
	
getHomeR :: Handler ()
getHomeR = sendFile typeHtml "staticpage.html"

getJsonR :: Int -> Handler Value
getJsonR personId = do
	render <- getUrlRenderParams
	--liftIO $ print render
	person <- liftIO $ getPerson personId
	returnJson $ person

putJsonR :: Int -> Handler Value
putJsonR personId = do
	person <- parseJsonBody_ :: Handler Person
	liftIO $ savePerson person
	returnJson $ person

deleteJsonR :: Int -> Handler Value
deleteJsonR personId = do
	liftIO $ deletePerson personId
	returnJson $ personId
	
getJsonlR :: Handler Value
getJsonlR = do
	persons <- liftIO $ getPersonList
	returnJson $ persons

main :: IO ()
main = warp 3000 HelloWorld
