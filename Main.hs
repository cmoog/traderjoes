{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad
import Data.ByteString.Lazy qualified as L
import Data.FileEmbed (embedStringFile)
import Data.Maybe
import Database.SQLite.Simple qualified as SQL
import GHC.Generics
import Prices (Item (..), allItems)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.IO
import Text.Blaze.Html5 qualified as H
import TraderJoesCom (renderPage, renderTable)

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
      let html = renderPage [H.h1 "Price Changes", renderTable ["Date Changed", "Item Name" :: String, "Old Price", "New Price"] (displayPriceChange <$> changes), H.h1 "All Items", renderTable ["Item Name" :: String, "Retail Price"] (display <$> prices)]
      removeDirectoryRecursive "site"
      createDirectory "site"
      L.writeFile "site/index.html" html
    Just "fetch" -> do
      hPutStrLn stderr "running"
      scrapeAll conn
      SQL.close conn
      hPutStrLn stderr "done"
    _ -> fail "run with 'fetch', 'gen'"

display :: DBItem -> [String]
display (DBItem {ditem_title, dretail_price}) = [ditem_title, dretail_price]

displayPriceChange :: PriceChange -> [String]
displayPriceChange (PriceChange {pitem_title, pbefore_price, pafter_price, pafter_date}) = [pafter_date, pitem_title, pbefore_price, pafter_price]

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
priceChanges c = SQL.query c $(embedStringFile "./leads.sql") ()

scrapeAll :: SQL.Connection -> IO ()
scrapeAll conn = do
  items <- allItems
  mapM_ (insert conn) items

openDB :: IO SQL.Connection
openDB = do
  conn <- SQL.open "traderjoes.db"
  SQL.execute_ conn $(embedStringFile "./schema.sql")
  return conn

insert :: SQL.Connection -> Item -> IO ()
insert conn (Item {sku, item_title, retail_price}) =
  SQL.execute conn "INSERT INTO items (sku, retail_price, item_title, inserted_at) VALUES (?, ?, ?, DATETIME('now'))" (sku, retail_price, item_title)
