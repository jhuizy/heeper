{-# LANGUAGE OverloadedStrings #-}

module User.Helper where

import           Data.Text
import qualified User.Database as DB
import           Web.JWT

data JWTInfo = JWTInfo
  { _id    :: Int
  , _email :: String
  } deriving (Show)

jwtInfo :: DB.User -> JWTInfo
jwtInfo (DB.User id e p a) = JWTInfo id e

parseClaims :: JWTClaimsSet -> JWTInfo
parseClaims claims = undefined

mkToken :: DB.User -> JSON
mkToken u = encodeSigned HS256 (secret "mysecret") $ mkClaims u

mkClaims :: DB.User -> JWTClaimsSet
mkClaims = def

verifyToken :: Text -> (Maybe JWTInfo)
verifyToken = undefined
