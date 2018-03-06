{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Web.Authenticate.OAuth
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Network.HTTP.Conduit

filePath = "./twitterConfig"

myOAuth :: BS.ByteString -> BS.ByteString -> OAuth
myOAuth key secret =
  newOAuth { oauthServerName = "api.twitter.com"
           , oauthConsumerKey = key
           , oauthConsumerSecret = secret
           }

tweet :: OAuth -> Credential -> BS.ByteString -> IO ()
tweet oauth cred tw = do
  req <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let postReq = urlEncodedBody [("status", tw)] req
  m <- newManager tlsManagerSettings
  res <- do
    signedreq <- signOAuth oauth cred postReq
    httpLbs signedreq m
  return ()

readConfig :: IO [BS8.ByteString]
readConfig = map (BS8.pack) . lines <$> readFile filePath

main :: IO ()
main = do
  config <- readConfig
  let !oauth = myOAuth (config !! 0) (config !! 1)
      !cred = newCredential (config !! 2) (config !! 3)
  content <- getLine :: IO String
  tweet oauth cred $ encodeUtf8 $ T.pack content
