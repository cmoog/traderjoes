CREATE TABLE
  IF NOT EXISTS items (
    sku text,
    retail_price text,
    item_title text,
    inserted_at integer,
    store_code text,
    availability text
  );

/*
ALTER TABLE items ADD store_code text;
ALTER TABLE items ADD availability text;
*/
