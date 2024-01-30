-- latest prices
SELECT
  sku,
  item_title,
  retail_price,
  inserted_at
FROM (
  SELECT *, ROW_NUMBER() OVER (PARTITION BY sku ORDER BY inserted_at DESC) as rn
  FROM items
) tmp
WHERE rn = 1;
