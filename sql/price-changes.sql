WITH
  PriceChanges AS (
    SELECT
      sku,
      retail_price,
      item_title,
      LEAD (retail_price) OVER (
        PARTITION BY
          sku,
          store_code
        ORDER BY
          inserted_at
      ) AS next_price,
      inserted_at,
      LEAD (inserted_at) OVER (
        PARTITION BY
          sku,
          store_code
        ORDER BY
          inserted_at
      ) AS next_inserted_at,
      store_code
    FROM
      items
  ),
  EarliestPrices AS (
    SELECT
      sku,
      NULL AS retail_price,
      item_title,
      retail_price AS next_price,
      NULL AS inserted_at,
      inserted_at AS next_inserted_at,
      store_code
    FROM
      items
    GROUP BY sku, store_code
    HAVING inserted_at = min(inserted_at)
  ),
  Result AS (
    SELECT * FROM EarliestPrices
    UNION
    SELECT * FROM PriceChanges
    WHERE
      retail_price IS NOT next_price
      AND retail_price != "0.01"
      AND next_price IS NOT "0.01"
  )
SELECT
  sku,
  item_title,
  retail_price AS before_price,
  next_price AS after_price,
  inserted_at AS before_date,
  next_inserted_at AS after_date,
  store_code
FROM
  Result
WHERE
  store_code = ?
ORDER BY
  next_inserted_at DESC;
