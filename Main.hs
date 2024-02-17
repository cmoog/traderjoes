{-# LANGUAGE GHC2021 #-}
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
import Prices (Item (..), allItemsByStore)
import System.Directory (createDirectory, doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal qualified as A
import Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= handleArgs

help :: String
help =
  "Usage:\n\
  \  traderjoes fetch\n\
  \  traderjoes gen\
  \"

printlog :: String -> IO ()
printlog = hPutStrLn stderr

handleArgs :: [String] -> IO ()
handleArgs ["gen"] = do
  conn <- openDB
  changes <- priceChanges conn
  allitems <- latestPrices conn
  SQL.close conn
  ts <- showTime
  let html = renderPage $ pageBody changes allitems ts
  setupCleanDirectory "site"
  printlog "writing to ./site"
  L.writeFile "site/index.html" html
handleArgs ["fetch"] = do
  conn <- openDB
  printlog "running..."
  -- Store code 701 is the South Loop Chicago location.
  let stores :: [String] =
        [ "701", -- Chicago South Loop
          "31", -- Los Angeles
          "546", -- East Village
          "452" -- Austin Seaholm
        ]
  mapM_ (processStore conn) stores
  SQL.close conn
handleArgs _ = printlog help >> exitFailure

-- | fetch all items for the store and insert into the database
processStore :: SQL.Connection -> String -> IO ()
processStore conn store = do
  items <- allItemsByStore store
  printlog $ "fetched items (" <> store <> "): " <> (show . length $ items)
  printlog $ "inserting into database (" <> store <> ")..."
  mapM_ (insert conn store) items
  changeCount <- SQL.totalChanges conn
  printlog $ "changed rows (" <> store <> "): " <> show changeCount

-- | generate the home page html body
pageBody :: [PriceChange] -> [DBItem] -> String -> H.Html
pageBody changes items timestamp = do
  H.i . H.toMarkup $ "Last updated: " ++ timestamp
  H.br
  H.a ! A.class_ "underline" ! A.href "https://github.com/cmoog/traderjoes" ! A.target "_blank" $ "Source code"
  H.br
  H.a ! A.class_ "underline" ! A.href "https://data.traderjoesprices.com/dump.csv" ! download "traderjoes-dump.csv" $ "Download full history (.csv)"
  H.br
  H.br
  H.strong ! A.style "font-size: 1.15em; font-family: serif" $ do
    H.i "Disclaimer: This website is not affiliated, associated, authorized, endorsed by, or in any way officially connected with Trader Joe's, or any of its subsidiaries or its affiliates. All prices are sourced from Trader Joe's South Loop in Chicago, IL (store code 701). There may be regional price differences from those listed on this site. This website may include discontinued or unavailable products."
  H.br
  H.h1 "(Unofficial) Trader Joe's Price Tracking"
  H.form ! A.action "signup" ! A.class_ "signup-form" ! A.role "form" $ do
    H.label ! A.for "email" $ "Sign up for a weekly email of price changes."
    H.input ! A.required "" ! A.name "email" ! A.type_ "email" ! A.class_ "formInput input-lg" ! A.placeholder "example@gmail.com"
    H.button ! A.type_ "submit" ! A.class_ "btn primary" ! A.title "Email address" $ "Sign Up"
  H.h2 "Price Changes"
  H.table ! A.class_ "table table-striped table-gray" $ do
    H.thead . H.tr . H.toMarkup $ H.th <$> ["Date Changed" :: H.Html, "Item Name", "Old Price", "New Price"]
    H.tbody . H.toMarkup $ displayPriceChange <$> changes
  H.h2 "All Items"
  H.table ! A.class_ "table table-striped table-gray" $ do
    H.thead . H.tr . H.toMarkup $ H.th <$> ["Item Name" :: H.Html, "Retail Price"]
    H.tbody . H.toMarkup $ displayDBItem <$> items

-- render the given page body with html head/styles/meta
renderPage :: (H.ToMarkup a) => a -> ByteString
renderPage page = renderHtml $ H.html $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.meta ! A.name "description" ! A.content "Daily Tracking of Trader Joe's Price Changes"
    H.title "Trader Joe's Prices"
  H.body $ do
    H.style $(embedStringFile "./style.css")
    H.toMarkup page

-- | display the item as a table row
displayDBItem :: DBItem -> H.Html
displayDBItem (DBItem {ditem_title, dretail_price, dsku}) = H.tr $ do
  H.td $ H.a ! A.href (productUrl dsku) ! A.target "_blank" $ H.toHtml ditem_title
  H.td $ H.toHtml dretail_price

-- | display the price change as a table row
displayPriceChange :: PriceChange -> H.Html
displayPriceChange (PriceChange {pitem_title, pbefore_price, pafter_price, pafter_date, psku}) = H.tr $ do
  H.td $ H.toHtml pafter_date
  H.td $ H.a ! A.href (productUrl psku) ! A.target "_blank" $ H.toHtml pitem_title
  H.td $ H.toHtml pbefore_price
  H.td ! A.class_ (H.toValue $ priceChangeClass (pbefore_price, pafter_price)) $ H.toHtml pafter_price

-- | color the price change table cell based on whether the price increased vs. decreased
priceChangeClass :: (String, String) -> String
priceChangeClass (before, after) = fromMaybe "" $ do
  beforeNum <- readMaybe before :: Maybe Float
  afterNum <- readMaybe after :: Maybe Float
  return $ if beforeNum > afterNum then "green" else "red"

-- link to the product detail page by `sku`
productUrl :: String -> H.AttributeValue
productUrl sku = H.toValue $ "https://traderjoes.com/home/products/pdp/" ++ sku

data DBItem = DBItem
  { dsku :: String,
    ditem_title :: String,
    dretail_price :: String,
    dinserted_at :: String,
    dstore_code :: String
  }
  deriving (Generic, Show)

instance SQL.FromRow DBItem

instance SQL.ToRow DBItem

-- fetch the latest seen price for each `sku`
latestPrices :: SQL.Connection -> IO [DBItem]
latestPrices conn = SQL.query_ conn $(embedStringFile "./sql/latest-prices.sql")

data PriceChange = PriceChange
  { psku :: String,
    pitem_title :: String,
    pbefore_price :: String,
    pafter_price :: String,
    pbefore_date :: String,
    pafter_date :: String,
    pstore_code :: String
  }
  deriving (Generic, Show)

instance SQL.FromRow PriceChange

-- each change in price partitioned by sku and storeid
priceChanges :: SQL.Connection -> IO [PriceChange]
priceChanges conn = SQL.query_ conn $(embedStringFile "./sql/price-changes.sql")

openDB :: IO SQL.Connection
openDB = do
  conn <- SQL.open "traderjoes.db"
  SQL.execute_ conn $(embedStringFile "./sql/schema.sql")
  return conn

insert :: SQL.Connection -> String -> Item -> IO ()
insert conn store (Item {sku, item_title, retail_price, availability}) =
  SQL.execute conn "INSERT INTO items (sku, retail_price, item_title, store_code, availability, inserted_at) VALUES (?, ?, ?, ?, ?, DATETIME('now'))" (sku, retail_price, item_title, store, availability)

-- | show a timestamp in the current system timezone
showTime :: IO String
showTime = do
  zone <- getCurrentTimeZone
  utc <- getCurrentTime
  return $ show (utcToLocalTime zone utc) <> " " <> show zone

-- | this should be patched upstream in Blaze
download :: H.AttributeValue -> H.Attribute
download = A.attribute "download" " download=\""

-- | create an empty directory, deleting it beforehand if it already exists
setupCleanDirectory :: FilePath -> IO ()
setupCleanDirectory dir = do
  siteDirectoryExists <- doesDirectoryExist dir
  when siteDirectoryExists $ removeDirectoryRecursive dir
  createDirectory dir
