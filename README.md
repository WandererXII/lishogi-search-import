# lishogi search import

This code streams lishogi games from a mongodb secondary to the lishogi-search service.

## Usage

```
# import since Jan 2020
sbt "run http://172.16.0.8:9822 2020-01"

# reset search index and import since Jan 2020
sbt "run http://172.16.0.8:9822 reset"
```
