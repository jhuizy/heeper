{-# LANGUAGE OverloadedStrings #-}

module User.Database where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.BCrypt
import qualified Data.ByteString.Char8          as C
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
  hashPassword <- maybe (error "hashing failed") C.unpack <$> hashPasswordUsingPolicy slowerBcryptHashingPolicy (C.pack pass)
  execute conn "INSERT INTO users (email, password, age) VALUES (?, ?, ?)" (email, hashPassword, age)
  ident <- lastInsertRowId conn
  return $ User (fromIntegral ident) email hashPassword age

find :: DB -> Int -> IO (Maybe User)
find conn ident = do
  user <- query conn "SELECT * FROM users WHERE id = ?" $ Only ident
  return $ listToMaybe user

findByEmail :: DB -> String -> IO (Maybe User)
findByEmail conn email = do
  user <- query conn "SELECT * FROM users WHERE email = ?" $ Only email
  return $ listToMaybe user

checkCredentials :: DB -> String -> String -> IO Bool
checkCredentials conn email pass = do
  let p = C.pack
  maybeUser <- findByEmail conn email
  return $ case maybeUser of
    (Just u) -> validatePassword (p $ password u) (p pass)
    Nothing  -> False
