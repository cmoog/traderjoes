# Trader Joe's Price Tracking

Updated daily at [traderjoesprices.com](https://traderjoesprices.com).

```console
$ nix run . -- fetch
runnning
requesting page 1
requesting page 2
...

$ sqlite3 traderjoes.db
sqlite> select * from items order by item_title limit 3;
093200|4.49|100% Colombian Instant Coffee|2024-01-21 22:45:30
156933|3.99|100% Mango Juice from Carabao Mangoes|2024-01-21 22:45:33
066569|3.99|100% Orange Juice No Pulp|2024-01-21 22:45:28

$ nix run . -- gen
# generates site in ./site
```
