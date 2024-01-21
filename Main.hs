{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (join, when)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe
import Database.SQLite.Simple qualified as SQL
import GHC.Generics
import Network.HTTP.Simple qualified as HTTP
import System.IO

newtype Response = Response {rdata :: Data} deriving (Generic, Show)

newtype Data = Data {products :: Products} deriving (Generic, Show)

data Products = Products {items :: [Item], total_count :: Int} deriving (Generic, Show)

data Item = Item
  { retail_price :: String,
    name :: String,
    item_title :: String,
    sku :: String,
    created_at :: String,
    updated_at :: String,
    url_key :: String
  }
  deriving (Generic, Show)

instance FromJSON Response where
  parseJSON (Object v) = do
    d <- v .: "data"
    return Response {rdata = d}
  parseJSON _ = fail "invalid response"

instance FromJSON Data

instance FromJSON Products

instance FromJSON Item

data Request = Request
  { operationName :: String,
    variables :: Variables,
    query :: String
  }
  deriving (Generic, Show)

data Variables = Variables
  { storeCode :: String,
    published :: String,
    currentPage :: Int,
    pageSize :: Int
  }
  deriving (Generic, Show)

instance ToJSON Request

instance ToJSON Variables

main :: IO ()
main = do
  hPutStrLn stderr "running"
  conn <- openDB
  fetchAll conn
  SQL.close conn
  hPutStrLn stderr "done"

fetchAll :: SQL.Connection -> IO ()
fetchAll conn = do
  fetches <- mapM fetch [1 .. 18]
  let items = join $ catMaybes fetches
  mapM_ (insert conn) items

fetch :: Int -> IO (Maybe [Item])
fetch page = do
  query <- readFile "./query.graphql"
  let request =
        Request
          { operationName = "SearchProduct",
            variables =
              Variables
                { storeCode = "701",
                  published = "1",
                  currentPage = page,
                  pageSize = 100
                },
            query = query
          }
  hPutStrLn stderr $ "requesting page: " ++ show page
  result <- sendQuery request
  let resp :: Maybe Response = decode $ LBS.pack result
  case resp of
    Nothing -> return Nothing
    Just (Response (Data (Products {items}))) -> return $ Just items

graphqlUrl :: String
graphqlUrl = "https://www.traderjoes.com/api/graphql"

sendQuery :: Request -> IO String
sendQuery query = do
  let encoded = encode query
  rawReq <- HTTP.parseRequest graphqlUrl
  let req = HTTP.setRequestMethod "POST" . HTTP.setRequestBodyLBS encoded . HTTP.setRequestHeaders headers $ rawReq
  resp <- HTTP.httpBS req
  let statusCode = HTTP.getResponseStatusCode resp
  when (statusCode /= 200) . fail $ show req ++ "\nrequest failed:\n" ++ show resp
  return $ BS.unpack $ HTTP.getResponseBody resp

headers :: [HTTP.Header]
headers =
  [ ("accept", "*/*"),
    ("accept-language", "en-US,en;q=0.9"),
    ("cache-control", "no-cache"),
    ("content-type", "application/json"),
    ("pragma", "no-cache"),
    ("accept-encoding", "gzip, deflate, br"),
    ("referrer", "https://www.traderjoes.com/home/products/pdp/organic-ground-beef-8515-092558"),
    ("sec-ch-ua", "\"Not_A Brand\";v=\"8\", \"Chromium\";v=\"120\", \"Google Chrome\";v=\"120\""),
    ("sec-ch-ua-mobile", "?0"),
    ("sec-ch-ua-platform", "\"macOS\""),
    ("sec-fetch-dest", "empty"),
    ("sec-fetch-mode", "cors"),
    ("sec-fetch-site", "same-origin"),
    ("referrer", "https://www.traderjoes.com/home/products/pdp/organic-ground-beef-8515-092558"),
    ("referrerPolicy", "strict-origin-when-cross-origin"),
    ("method", "POST"),
    ("mode", "cors"),
    ("credentials", "include")
  ]

openDB :: IO SQL.Connection
openDB = do
  conn <- SQL.open "traderjoes.db"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS items (sku text, retail_price text, item_title text, inserted_at integer);"
  return conn

insert :: SQL.Connection -> Item -> IO ()
insert conn (Item {sku, item_title, retail_price}) =
  SQL.execute conn "INSERT INTO items (sku, retail_price, item_title, inserted_at) VALUES (?, ?, ?, DATETIME('now'))" (sku, retail_price, item_title)
