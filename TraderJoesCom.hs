{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module TraderJoesCom (renderPage) where

import Data.ByteString.Lazy (ByteString)
import Data.FileEmbed (embedStringFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes qualified as A

renderPage :: [[String]] -> ByteString
renderPage rows = renderHtml $ html $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.title "Trader Joe's Prices"
  body $ do
    style $(embedStringFile "./style.css")
    h1 "Latest Prices"
    renderTable rows

renderTable :: [[String]] -> H.Html
renderTable rows = html $ do
  H.table ! A.class_ "table table-striped table-gray" $ do
    H.thead $ H.tr $ do
      H.th "Stock ID"
      H.th "Item Name"
      H.th "Retail Price"
    H.tbody $ toMarkup $ toMarkup . renderRow <$> rows

renderRow :: [String] -> H.Html
renderRow row = H.tr $ toHtml $ renderCell <$> row

renderCell :: String -> Html
renderCell = H.td . H.toMarkup
