module Prices (allItemsByStore, Item (..)) where

import Control.Monad (join, when)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.FileEmbed (embedStringFile)
import Data.Maybe
import GHC.Generics
import Network.HTTP.Simple qualified as HTTP

newtype Response = Response {rdata :: Data} deriving (Generic, Show)

newtype Data = Data {products :: Products} deriving (Generic, Show)

data Products = Products {items :: [Item], total_count :: Int} deriving (Generic, Show)

data Item = Item
  { retail_price :: String
  , item_title :: String
  , sku :: String
  , url_key :: String
  , availability :: String
  }
  deriving (Generic, Show)

instance FromJSON Response where
  parseJSON (Object v) = do
    d <- v .: "data"
    return Response{rdata = d}
  parseJSON _ = fail "invalid response"

instance FromJSON Data

instance FromJSON Products

instance FromJSON Item

data Request = Request
  { operationName :: String
  , variables :: Variables
  , query :: String
  }
  deriving (Generic, Show)

data Variables = Variables
  { storeCode :: String
  , published :: String
  , currentPage :: Int
  , pageSize :: Int
  }
  deriving (Generic, Show)

instance ToJSON Request

instance ToJSON Variables

allItemsByStore :: String -> IO [Item]
allItemsByStore store = do
  items <- mapM (itemsByStore store) [1 .. 25]
  return . join . catMaybes $ items

itemsByStore :: String -> Int -> IO (Maybe [Item])
itemsByStore storeCode page = do
  let request =
        Request
          { operationName = "SearchProduct"
          , variables =
              Variables
                { storeCode = storeCode
                , published = "1"
                , currentPage = page
                , pageSize = 100
                }
          , query = $(embedStringFile "./query.graphql")
          }
  result <- sendQuery request
  return $ items . products . rdata <$> decode result

sendQuery :: Request -> IO ByteString
sendQuery query = do
  url <- HTTP.parseRequest "https://www.traderjoes.com/api/graphql"
  let encoded = encode query
  let req = HTTP.setRequestMethod "POST" . HTTP.setRequestBodyLBS encoded . HTTP.setRequestHeaders headers $ url
  resp <- HTTP.httpLBS req
  let statusCode = HTTP.getResponseStatusCode resp
  when (statusCode /= 200) . fail $ show req <> "\nrequest failed:\n" <> show resp
  return $ HTTP.getResponseBody resp

headers :: [HTTP.Header]
headers =
  [ ("accept", "*/*")
  , ("accept-language", "en-US,en;q=0.9")
  , ("cache-control", "no-cache")
  , ("content-type", "application/json")
  , ("pragma", "no-cache")
  , ("accept-encoding", "gzip, deflate, br")
  , ("sec-ch-ua", "\"Not_A Brand\";v=\"8\", \"Chromium\";v=\"120\", \"Google Chrome\";v=\"120\"")
  , ("sec-ch-ua-mobile", "?0")
  , ("sec-ch-ua-platform", "\"macOS\"")
  , ("sec-fetch-dest", "empty")
  , ("sec-fetch-mode", "cors")
  , ("sec-fetch-site", "same-origin")
  , ("referrer", "https://www.traderjoes.com/home/products/pdp/organic-ground-beef-8515-092558")
  , ("referrerPolicy", "strict-origin-when-cross-origin")
  , ("method", "POST")
  , ("mode", "cors")
  , ("credentials", "include")
  ]
