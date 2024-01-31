{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (when)
import Data.ByteString.Lazy qualified as L
import Data.FileEmbed (embedStringFile)
import Data.Maybe
import Database.SQLite.Simple qualified as SQL
import GHC.Generics
import Prices (Item (..), allItems)
import System.Environment (getArgs)
import System.IO
import TraderJoesCom (renderPage)

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
      scrapeAll conn
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
