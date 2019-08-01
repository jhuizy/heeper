{-# LANGUAGE OverloadedStrings #-}

module Heeper where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State                  (lift, liftM)
import           Data.String                          (fromString)
import qualified Data.Text.Lazy                       as L
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import qualified User.Api                             as Api
import qualified User.Database                        as DB
import           Web.Scotty.Trans

newtype ScottyM a = ScottyM { runApp :: ScottyT APIError IO a } deriving (Functor, Applicative, Monad)
type ActionM = ActionT APIError IO

data APIError =
  NotFound Int
  | BadRequest String
  | UnhandledError String
  deriving (Show, Eq)

instance ScottyError APIError where
  stringError = UnhandledError
  showError = fromString . show

errorHandler :: APIError -> ActionM ()
errorHandler (NotFound i) = do
  status status404
  text "Not Found"
errorHandler (BadRequest s) = do
  status status400
  text $ L.pack s
errorHandler (UnhandledError s) = do
  liftIO $ putStrLn s
  status status500
  text "Something went wrong..."

main :: IO ()
main = scottyT 3000 id (runApp app)

app :: ScottyM ()
app = ScottyM $ do

  let db = liftIO DB.init

  defaultHandler errorHandler

  middleware logStdout

  post "/login" $ do
    (Api.LoginRequest email pass) <- rescue jsonData (raise . BadRequest . L.unpack . showError)
    db' <- db
    success <- liftIO $ DB.checkCredentials db' email pass
    text . L.pack . show $ success

  get "/users/:id" $ do
    id <- param "id"
    db' <- db
    user <- liftIO $ DB.find db' id
    maybe (raise $ NotFound id) (json . Api.fromUser) user

  get "/users" $ do
    users <- runDB db DB.list
    json $ fmap Api.fromUser users

  post "/users" $ do
    (Api.CreateUserRequest email pass age) <- jsonData
    db' <- db
    user <- liftIO $ DB.create db' email pass age
    json $ Api.fromUser user

runDB :: (Monad m, MonadIO m) => m DB.DB -> (DB.DB -> IO a) -> m a
runDB db f = db >>= (liftIO . f)

