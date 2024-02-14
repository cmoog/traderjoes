-- latest prices
SELECT
  sku,
  item_title,
  retail_price,
  inserted_at,
  store_code
FROM
  (
    SELECT
      *,
      ROW_NUMBER() OVER (
        PARTITION BY
          sku,
          store_code
        ORDER BY
          inserted_at DESC
      ) as rn
    FROM
      items
  ) tmp
WHERE
  rn = 1
  AND retail_price != "0.01"
  AND store_code = "701"
  -- AND availability = "1"
ORDER BY
  item_title;
