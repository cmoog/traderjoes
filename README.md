# Trader Joe's Price Tracking

```console
$ nix run .
runnning
requesting page 1
requesting page 2
...

$ sqlite3 traderjoes.db
> select * from items limit 3
076249|Emerald Insulated Reusable Bag|7.99||2024-01-21 21:05:53
078381|Chocolate Berry Lip Mask Duo|7.99||2024-01-21 21:05:53
078041|Brazil Nut Body Oil|6.99||2024-01-21 21:05:53
```
