module Test where

import qualified Data.Text               as T
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple

data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

main :: IO ()
main = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  execute conn "INSERT INTO test VALUES (?, ?)" (TestField 0 "hello world")
  r <- query_ conn "SELECT * FROM test" :: IO [TestField]
  mapM_ print r
  execute_ conn "DROP TABLE test"
  close conn