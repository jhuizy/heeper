{-# LANGUAGE OverloadedStrings #-}

module User.Database where

import           Control.Monad.State
import           Data.IORef
import           Data.Maybe
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data User = User
  { id       :: Int
  , email    :: String
  , password :: String
  , age      :: Int
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id e p a) = toRow (id, e, p, a)

type DB = Connection

init :: IO DB
init = do
  conn <- open "test.db"
  up conn
  return conn

up :: DB -> IO () 
up conn = execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, email TEXT NOT NULL, password TEXT NOT NULL, age INTEGER NOT NULL)"

list :: DB -> IO [User]
list conn = query_ conn "SELECT * FROM users" 

create :: DB -> String -> String -> Int -> IO User
create conn email pass age = do
  execute conn "INSERT INTO users (email, password, age) VALUES (?, ?, ?)" (email, pass, age)
  ident <- lastInsertRowId conn
  return $ User (fromIntegral ident) email pass age

find :: DB -> Int -> IO (Maybe User)
find conn ident = do
  user <- query conn "SELECT * FROM users WHERE id = ?" $ Only ident
  return $ listToMaybe user