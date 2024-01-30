{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (join, when)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.FileEmbed (embedStringFile)
import Data.Maybe
import Database.SQLite.Simple qualified as SQL
import GHC.Generics
import Network.HTTP.Simple qualified as HTTP
import System.Environment (getArgs)
import System.IO
import TraderJoesCom (renderPage)

newtype Response = Response {rdata :: Data} deriving (Generic, Show)

newtype Data = Data {products :: Products} deriving (Generic, Show)

data Products = Products {items :: [Item], total_count :: Int} deriving (Generic, Show)

data Item = Item
  { retail_price :: String,
    item_title :: String,
    sku :: String,
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
  args <- getArgs
  when (length args /= 1) $ fail "provide exactly 1 argument ('fetch' | 'gen')"
  conn <- openDB
  case listToMaybe args of
    Just "gen" -> do
      prices <- latestPrices conn
      SQL.close conn
      let html = renderPage $ display <$> prices
      L.putStr html
    Just "fetch" -> do
      hPutStrLn stderr "running"
      fetchAll conn
      SQL.close conn
      hPutStrLn stderr "done"
    _ -> putStrLn "run with 'fetch' or 'gen'"

display :: DBItem -> [String]
display (DBItem {dsku, ditem_title, dretail_price}) = [dsku, ditem_title, dretail_price]

data DBItem = DBItem
  { dsku :: String,
    ditem_title :: String,
    dretail_price :: String,
    dinserted_at :: String
  }
  deriving (Generic, Show)

instance SQL.FromRow DBItem

instance SQL.ToRow DBItem

latestPrices :: SQL.Connection -> IO [DBItem]
latestPrices conn = SQL.query conn $(embedStringFile "./latest-prices.sql") ()

fetchAll :: SQL.Connection -> IO ()
fetchAll conn = do
  items <- mapM fetch [1 .. 25]
  mapM_ (insert conn) . join . catMaybes $ items

fetch :: Int -> IO (Maybe [Item])
fetch page = do
  hPutStrLn stderr $ "requesting page: " ++ show page
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
            query = $(embedStringFile "./query.graphql")
          }
  result <- sendQuery request
  return $ items . products . rdata <$> decode result

openDB :: IO SQL.Connection
openDB = do
  conn <- SQL.open "traderjoes.db"
  SQL.execute_ conn $(embedStringFile "./schema.sql")
  return conn

insert :: SQL.Connection -> Item -> IO ()
insert conn (Item {sku, item_title, retail_price}) =
  SQL.execute conn "INSERT INTO items (sku, retail_price, item_title, inserted_at) VALUES (?, ?, ?, DATETIME('now'))" (sku, retail_price, item_title)

sendQuery :: Request -> IO ByteString
sendQuery query = do
  url <- HTTP.parseRequest "https://www.traderjoes.com/api/graphql"
  let encoded = encode query
  let req = HTTP.setRequestMethod "POST" . HTTP.setRequestBodyLBS encoded . HTTP.setRequestHeaders headers $ url
  resp <- HTTP.httpLBS req
  let statusCode = HTTP.getResponseStatusCode resp
  when (statusCode /= 200) . fail $ show req ++ "\nrequest failed:\n" ++ show resp
  return $ HTTP.getResponseBody resp

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
