{-# LANGUAGE OverloadedStrings #-}

module Heeper where

import           Control.Monad.State (lift, liftM)
import           Control.Monad.IO.Class
import           Data.String                          (fromString)
import qualified Data.Text.Lazy                       as L
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import qualified User.Api                             as Api
import qualified User.Database                        as DB
import           Web.Scotty.Trans

type AppM = ScottyT APIError IO
type AppActionM = ActionT APIError IO

data APIError =
  NotFound Int
  | BadRequest
  | UnhandledError
  deriving (Show, Eq)

instance ScottyError APIError where
  stringError _ = UnhandledError
  showError = fromString . show

errorHandler :: APIError -> AppActionM ()
errorHandler (NotFound i) = do
  status status404
  text "Not Found"
errorHandler BadRequest = do
  status status400
  text "Bad Request"
errorHandler _ = do
  status status500
  text "Something went wrong..."

main :: IO ()
main = scottyT 3000 id $ do

  let db = liftIO DB.init

  defaultHandler errorHandler

  middleware logStdout
  get "/hello" $ text "hello world"

  get "/hello/:id" $ do
    id <- param "id"
    text $ "hello " <> id

  get "/users/:id" $ do
    id <- param "id"
    user <- liftIO $ db >>= flip DB.find id
    maybe (raise $ NotFound id) (json . Api.fromUser) user

  get "/users" $ do
    users <- liftIO $ db >>= DB.list
    json $ fmap Api.fromUser users

  post "/users" $ do
    userReq <- jsonData :: AppActionM Api.CreateUserRequest
    user <- liftIO $ db >>= (\db' -> DB.create db' (Api._curEmail userReq) (Api._curPassword userReq) (Api._curAge userReq))
    json $ Api.fromUser user




