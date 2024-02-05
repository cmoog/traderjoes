{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module TraderJoesCom (renderPage, renderTable) where

import Data.ByteString.Lazy (ByteString)
import Data.FileEmbed (embedStringFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes qualified as A

renderPage :: (ToMarkup a) => a -> ByteString
renderPage page = renderHtml $ html $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.title "Trader Joe's Prices"
  body $ do
    style $(embedStringFile "./style.css")
    toMarkup page

renderTable :: (ToMarkup a) => [a] -> [[a]] -> H.Html
renderTable headers rows = html $ do
  H.table ! A.class_ "table table-striped table-gray" $ do
    H.thead $ H.tr $ toMarkup $ H.th <$> (toMarkup <$> headers)
    H.tbody $ toMarkup $ toMarkup . renderRow <$> rows

renderRow :: (ToMarkup a) => [a] -> H.Html
renderRow row = H.tr $ toHtml $ renderCell <$> row

renderCell :: (ToMarkup a) => a -> Html
renderCell = H.td . H.toMarkup
