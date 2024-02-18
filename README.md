# Trader Joe's Price Tracking

Updated daily at [traderjoesprices.com](https://traderjoesprices.com).

All product prices are sourced from the Chicago South Loop location, store code `701`.

```console
$ nix run . -- fetch
running...
requesting page 1
requesting page 2
...

$ sqlite3 traderjoes.db -csv -header
sqlite> SELECT * FROM items ORDER BY inserted_at DESC LIMIT 4;
sku,retail_price,item_title,inserted_at,store_code,availability
055691,3.49,"Scandinavian Swimmers","2024-02-15 14:31:29",701,1
063192,3.99,"Cauliflower Pancakes","2024-02-15 14:31:29",701,1
063486,2.99,"Organic Kansas City Style BBQ Sauce","2024-02-15 14:31:29",701,1
063277,1.49,"Red Chili Scalloped Crackers","2024-02-15 14:31:29",701,0

$ nix run . -- gen
# generates site in ./site
```

## Dedication

This project is dedicated to Trader Joe's Hyde Park in Chicago. Store code `706`.

![trader-joes-store](https://github.com/cmoog/traderjoes/assets/7585078/1d984c08-55dc-4686-8d2d-60629bfb77a2)
