module Main where

import Control.Concurrent.Async
import Control.Monad
import Data.Aeson ( encodeFile, ToJSON, toEncoding, defaultOptions, genericToEncoding )
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.FileEmbed (embedStringFile)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Clock.POSIX
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

stores :: [(String, String)]
stores =
  [ ("701", "Chicago South Loop")
  , ("31", "Los Angeles")
  , ("546", "NYC East Village")
  , ("452", "Austin Seaholm")
  ]

handleArgs :: [String] -> IO ()
handleArgs ["gen"] = do
  conn <- openDB
  setupCleanDirectory "site"
  printlog "writing to ./site"
  mapConcurrently_ (priceChangesJson conn) stores
  allitems <- latestPrices conn
  SQL.close conn
  ts <- showTime
  let html = renderPage $ pageBody allitems ts
  L.writeFile "site/index.html" html
handleArgs ["fetch"] = do
  conn <- openDB
  printlog $ "fetching stores: " <> show stores
  SQL.withTransaction conn $
    mapConcurrently_ (scrapeStore conn) stores
  printlog "done"
  changeCount <- SQL.totalChanges conn
  printlog $ "changed rows: " <> show changeCount
  SQL.close conn
handleArgs _ = printlog help >> exitFailure

-- | Fetch all items for the store and insert into the given database.
scrapeStore :: SQL.Connection -> (String, String) -> IO ()
scrapeStore conn store = do
  items <- allItemsByStore (fst store)
  mapM_ (insert conn (fst store)) items

-- | Generate the home page html body.
pageBody :: [DBItem] -> String -> H.Html
pageBody items timestamp = do
  H.i . H.toMarkup $ "Last updated: " ++ timestamp
  H.br
  H.a ! A.class_ "underline" ! A.href "https://github.com/cmoog/traderjoes" ! A.target "_blank" $ "Source code"
  H.br
  H.a ! A.class_ "underline" ! A.href "https://data.traderjoesprices.com/dump.csv" ! download "traderjoes-dump.csv" $ "Download full history (.csv)"
  H.br
  H.br
  H.strong ! A.style "font-size: 1.15em; font-family: serif" $ do
    H.i "Disclaimer: This website is not affiliated, associated, authorized, endorsed by, or in any way officially connected with Trader Joe's, or any of its subsidiaries or its affiliates. There may be regional price differences from those listed on this site. This website may include discontinued or unavailable products."
  H.br
  H.label ! A.for "store-select" $ do
    "Select store"
    H.select ! A.class_ "dropdown" ! A.name "store" ! A.id "store-select" $ do
      H.toMarkup $ displayStoreItemOption <$> stores
  H.h1 "(Unofficial) Trader Joe's Price Tracking"
  H.form ! A.action "signup" ! A.class_ "signup-form" ! A.role "form" $ do
    H.label ! A.for "email" $ "Sign up for a weekly email of price changes."
    H.input ! A.required "" ! A.name "email" ! A.type_ "email" ! A.class_ "formInput input-lg" ! A.placeholder "example@gmail.com"
    H.button ! A.type_ "submit" ! A.class_ "btn primary" ! A.title "Email address" $ "Sign Up"
  H.h2 "Price Chart"
  H.label ! A.for "item-select" $ do
    "Select item(s)"
    H.select ! A.class_ "dropdown" ! A.name "items[]" ! A.id "item-select" ! A.multiple "multiple" $ do
      H.toMarkup $ displayDBItemOption <$> items
  H.div ! A.class_ "chart-container" $ do
    H.canvas ! A.id "chart" $ ""
  H.h2 "Price Changes"
  H.table ! A.class_ "table table-striped table-gray" ! A.id "price-changes" $ do
    H.thead . H.tr . H.toMarkup $ H.th <$> ["Date Changed", "Item Name", "Old Price", "New Price"]
  H.h2 "All Items"
  H.table ! A.class_ "table table-striped table-gray" ! A.id "current-prices" $ do
    H.thead . H.tr . H.toMarkup $ H.th <$> ["Item Name", "Retail Price"]

-- | Render the given page body with html head/styles/meta.
renderPage :: (H.ToMarkup a) => a -> ByteString
renderPage page = renderHtml $ H.html $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.meta ! A.name "description" ! A.content "Daily Tracking of Trader Joe's Price Changes"
    H.meta ! A.name "keywords" ! A.content "trader joes, prices, price tracking"
    H.script ! A.src "https://cdn.jsdelivr.net/npm/jquery@3/dist/jquery.min.js" $ ""
    H.link ! A.href "https://cdn.jsdelivr.net/npm/select2@4/dist/css/select2.min.css" ! A.rel "stylesheet"
    H.script ! A.src "https://cdn.jsdelivr.net/npm/select2@4/dist/js/select2.min.js" $ ""
    H.script ! A.src "https://cdn.jsdelivr.net/npm/chart.js@4/dist/chart.umd.min.js" $ ""
    H.script ! A.src "https://cdn.jsdelivr.net/npm/chartjs-adapter-intl@0.1/dist/chartjs-adapter-intl.umd.min.js" $ ""
    H.title "Trader Joe's Prices"
  H.body $ do
    H.script $(embedStringFile "./script.js")
    H.style $(embedStringFile "./style.css")
    H.toMarkup page

-- | Display item as a dropdown option.
displayDBItemOption :: DBItem -> H.Html
displayDBItemOption (DBItem{ditem_title, dsku}) =
  H.option ! A.value (H.toValue dsku) $ H.toHtml (ditem_title <> " (" <> dsku <> ")")

-- | Display store as a dropdown option.
displayStoreItemOption :: (String, String) -> H.Html
displayStoreItemOption (store_id, name) =
  H.option ! A.value (H.toValue store_id) $ H.toHtml (name <> " (" <> store_id <> ")")

-- | URL to the product detail page by `sku`.
productUrl :: String -> H.AttributeValue
productUrl sku = H.toValue $ "https://traderjoes.com/home/products/pdp/" ++ sku

data DBItem = DBItem
  { dsku :: String
  , ditem_title :: String
  , dretail_price :: String
  , dinserted_at :: String
  , dstore_code :: String
  }
  deriving (Generic, Show)

instance SQL.FromRow DBItem

instance SQL.ToRow DBItem

-- | Fetch the latest seen price for each `sku`.
latestPrices :: SQL.Connection -> IO [DBItem]
latestPrices conn = SQL.query_ conn $(embedStringFile "./sql/latest-prices.sql")

data PriceChange = PriceChange
  { psku :: String
  , pitem_title :: String
  , pbefore_price :: Maybe String
  , pafter_price :: Maybe String
  , pbefore_date :: Maybe String
  , pafter_date :: Maybe String
  , pstore_code :: String
  }
  deriving (Generic, Show)

instance SQL.FromRow PriceChange

instance ToJSON PriceChange where
  toEncoding = genericToEncoding defaultOptions

priceChangesJson :: SQL.Connection -> (String, String) -> IO ()
priceChangesJson conn store = do
  changes <- SQL.query conn $(embedStringFile "./sql/price-changes.sql") [fst store] :: IO[PriceChange]
  encodeFile ("site/prices-" <> fst store <> ".json") changes

openDB :: IO SQL.Connection
openDB = do
  conn <- SQL.open "traderjoes.db"
  SQL.execute_ conn $(embedStringFile "./sql/schema.sql")
  pure conn

insert :: SQL.Connection -> String -> Item -> IO ()
insert conn store (Item{sku, item_title, retail_price, availability}) =
  SQL.execute
    conn
    "INSERT INTO items (sku, retail_price, item_title, store_code, availability, inserted_at) VALUES (?, ?, ?, ?, ?, DATETIME('now'))"
    (sku, retail_price, item_title, store, availability)

-- | Show a timestamp in the current system timezone.
showTime :: IO String
showTime = do
  zone <- getCurrentTimeZone
  utc <- getCurrentTime
  let local = utcToLocalTime zone utc
  pure $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S " local <> show zone

-- this should be patched upstream in Blaze
download :: H.AttributeValue -> H.Attribute
download = A.attribute "download" " download=\""

-- | Create an empty directory, deleting it beforehand if it already exists.
setupCleanDirectory :: FilePath -> IO ()
setupCleanDirectory dir = do
  siteDirectoryExists <- doesDirectoryExist dir
  when siteDirectoryExists $ removeDirectoryRecursive dir
  createDirectory dir
