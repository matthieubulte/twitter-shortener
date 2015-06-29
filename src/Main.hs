{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Credentials            as C
import           Data.Aeson
import           Data.ByteString        as B
import           Data.ByteString.Lazy   as BL
import           Data.Text              as T
import           Data.Text.Lazy         as TL
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Web.Authenticate.OAuth
import           Web.Scotty             (body, html, post, scotty, status)


oath :: OAuth
oath = newOAuth { oauthServerName     = "api.twitter.com"
                , oauthConsumerKey    = C.consumerKey
                , oauthConsumerSecret = C.consumerSecret
                }

credentials :: Credential
credentials = newCredential C.accessToken C.accessTokenSecret

data Tweet = Tweet { text :: !T.Text } deriving (Show, Generic)
instance FromJSON Tweet
instance ToJSON Tweet

authRequest :: Request -> IO BL.ByteString
authRequest req = do
  res <- withManager $ \m -> do
    signedReq <- signOAuth oath credentials req
    httpLbs signedReq m

  return $ responseBody res


tweet :: B.ByteString -> IO (Either String Tweet)
tweet t = do
  initReq      <- parseUrl "https://api.twitter.com/1.1/statuses/update.json?status=test%20status"
  let postReq  = initReq { method = "POST" }
  let req      = urlEncodedBody [("status", t)] postReq
  res          <- authRequest req

  return $ eitherDecode res


main :: IO ()
main = scotty 3000 $ post "/" $ do
  url <- body
  res <- liftIO $ tweet $ BL.toStrict url

  case res of
    (Left e) -> do
      liftIO $ print e
      status status500

    (Right (Tweet t)) -> html $ TL.fromStrict t
