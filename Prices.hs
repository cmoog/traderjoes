module Prices (allItemsByStore, Item (..)) where

import Control.Exception (bracket)
import Control.Monad (join)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.FileEmbed (embedStringFile)
import Data.Maybe
import GHC.Generics
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Process (proc, readCreateProcess)
import Text.Read (readMaybe)

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
    pure Response{rdata = d}
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
  pure . join . catMaybes $ items

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
  pure $ items . products . rdata <$> decode result

sendQuery :: Request -> IO ByteString
sendQuery req = do
  tmpDir <- getTemporaryDirectory
  bracket
    (openTempFile tmpDir "traderjoes-response.json")
    (\(fp, h) -> hClose h >> removeFile fp)
    ( \(fp, h) -> do
        hClose h
        let args =
              [ "-s"
              , "-X"
              , "POST"
              , "-H"
              , "content-type: application/json"
              , "-H"
              , "origin: https://www.traderjoes.com"
              , "-H"
              , "referer: https://www.traderjoes.com/home/products/pdp/organic-ground-beef-8515-092558"
              , "--data-binary"
              , "@-"
              , "-o"
              , fp
              , "-w"
              , "%{http_code}"
              , "https://www.traderjoes.com/api/graphql"
              ]
        statusOut <- readCreateProcess (proc curlBin args) (BLC8.unpack (encode req))
        respBody <- BL.readFile fp
        case readMaybe statusOut :: Maybe Int of
          Just 200 -> pure respBody
          _ -> fail $ "curl-impersonate request failed, status=" <> statusOut <> "\nbody:\n" <> BLC8.unpack respBody
    )
 where
  curlBin = "curl_chrome99"
