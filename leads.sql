WITH PriceChanges AS (
  SELECT
    sku,
    retail_price,
    item_title,
    LEAD(retail_price) OVER (PARTITION BY sku ORDER BY inserted_at) AS next_price,
    inserted_at,
    LEAD(inserted_at) OVER (PARTITION BY sku ORDER BY inserted_at) AS next_inserted_at
  FROM
    items
)
SELECT
  sku,
  item_title,
  retail_price AS before_price,
  next_price AS after_price,
  substr ("--JanFebMarAprMayJunJulAugSepOctNovDec", strftime ("%m", inserted_at) * 3, 3) || strftime(' %d, %Y', inserted_at) AS before_date,
  substr ("--JanFebMarAprMayJunJulAugSepOctNovDec", strftime ("%m", next_inserted_at) * 3, 3) || strftime(' %d, %Y', next_inserted_at) AS after_date
FROM
  PriceChanges
WHERE
  retail_price != next_price
  AND next_price IS NOT NULL
ORDER BY
  next_inserted_at
DESC;

