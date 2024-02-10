{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.FileEmbed (embedStringFile)
import Data.Maybe
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Database.SQLite.Simple qualified as SQL
import GHC.Generics
import Prices (Item (..), allItems)
import System.Directory (createDirectory, doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.IO
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal qualified as A
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ fail "provide exactly 1 argument ('fetch' | 'gen')"
  conn <- openDB
  case listToMaybe args of
    Just "gen" -> do
      prices <- latestPrices conn
      changes <- priceChanges conn
      SQL.close conn
      ts <- showTime
      let html = renderPage $ do
            H.i . H.toMarkup $ "Last updated: " ++ ts
            H.br
            H.a H.! A.class_ "underline" H.! A.href "https://github.com/cmoog/traderjoes" H.! A.target "_blank" $ "Source code"
            H.br
            H.a H.! A.class_ "underline" H.! A.href "https://data.traderjoesprices.com/dump.csv" H.! download "traderjoes-dump.csv" $ "Download full history (.csv)"
            H.h1 "Price Changes"
            H.table H.! A.class_ "table table-striped table-gray" $ do
              H.thead . H.tr . H.toMarkup $ H.th <$> (H.toMarkup <$> ["Date Changed" :: String, "Item Name", "Old Price", "New Price"])
              H.toMarkup $ displayPriceChange <$> changes
            H.h1 "All Items"
            H.table H.! A.class_ "table table-striped table-gray" $ do
              H.thead . H.tr . H.toMarkup $ H.th <$> (H.toMarkup <$> ["Item Name" :: String, "Retail Price"])
              H.toMarkup $ displayDBItem <$> prices
      setupSiteDirectory
      L.writeFile "site/index.html" html
    Just "fetch" -> do
      hPutStrLn stderr "running"
      scrapeAll conn
      SQL.close conn
      hPutStrLn stderr "done"
    _ -> fail "run with 'fetch', 'gen'"

renderPage :: (H.ToMarkup a) => a -> ByteString
renderPage page = renderHtml $ H.html $ do
  H.head $ do
    H.meta H.! A.charset "UTF-8"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.meta H.! A.name "description" H.! A.content "Daily Tracking of Trader Joe's Price Changes"
    H.title "Trader Joe's Prices"
  H.body $ do
    H.style $(embedStringFile "./style.css")
    H.toMarkup page

displayDBItem :: DBItem -> H.Html
displayDBItem (DBItem {ditem_title, dretail_price, dsku}) = H.tr $ do
  H.td $ H.a H.! A.href (productUrl dsku) H.! A.target "_blank" $ H.toHtml ditem_title
  H.td $ H.toHtml dretail_price

displayPriceChange :: PriceChange -> H.Html
displayPriceChange (PriceChange {pitem_title, pbefore_price, pafter_price, pafter_date, psku}) = H.tr $ do
  H.td $ H.toHtml pafter_date
  H.td $ H.a H.! A.href (productUrl psku) H.! A.target "_blank" $ H.toHtml pitem_title
  H.td $ H.toHtml pbefore_price
  H.td H.! A.class_ (H.toValue $ priceChangeClass (pbefore_price, pafter_price)) $ H.toHtml pafter_price

priceChangeClass :: (String, String) -> String
priceChangeClass (before, after) = fromMaybe "" $ do
  beforeNum <- readMaybe before :: Maybe Float
  afterNum <- readMaybe after :: Maybe Float
  return $ if beforeNum > afterNum then "green" else "red"

productUrl :: String -> H.AttributeValue
productUrl sku = H.toValue $ "https://traderjoes.com/home/products/pdp/" ++ sku

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
latestPrices conn = SQL.query conn $(embedStringFile "./sql/latest-prices.sql") ()

data PriceChange = PriceChange
  { psku :: String,
    pitem_title :: String,
    pbefore_price :: String,
    pafter_price :: String,
    pbefore_date :: String,
    pafter_date :: String
  }
  deriving (Generic, Show)

instance SQL.FromRow PriceChange

priceChanges :: SQL.Connection -> IO [PriceChange]
priceChanges c = SQL.query c $(embedStringFile "./sql/price-changes.sql") ()

scrapeAll :: SQL.Connection -> IO ()
scrapeAll conn = do
  items <- allItems
  mapM_ (insert conn) items

openDB :: IO SQL.Connection
openDB = do
  conn <- SQL.open "traderjoes.db"
  SQL.execute_ conn $(embedStringFile "./sql/schema.sql")
  return conn

insert :: SQL.Connection -> Item -> IO ()
insert conn (Item {sku, item_title, retail_price}) =
  SQL.execute conn "INSERT INTO items (sku, retail_price, item_title, inserted_at) VALUES (?, ?, ?, DATETIME('now'))" (sku, retail_price, item_title)

-- | show a timestamp in the current system timezone
showTime :: IO String
showTime = do
  zone <- getCurrentTimeZone
  utc <- getCurrentTime
  return $ show (utcToLocalTime zone utc) <> " " <> show zone

-- | this should be patched upstream in Blaze
download :: H.AttributeValue -> H.Attribute
download = A.attribute "download" " download=\""

-- | create an empty site directory, deleting it beforehand if it already exists
setupSiteDirectory :: IO ()
setupSiteDirectory = do
  siteDirectoryExists <- doesDirectoryExist "site"
  when siteDirectoryExists $ removeDirectoryRecursive "site"
  createDirectory "site"
