# e4ts
[日本語版 README はこちら](https://github.com/tsutomu-nagano/e4ts/blob/main/README-ja.md)

### `e`ngine `for` statistical tables using `t`idy data`s`.

## Overview
e4ts is a library for creating simple statistical tables.
The main functions are as follows.
- Conversion of missing values.
- Total Calculation Settings.
- Multiple aggregation methods.

## Installation
### using devtools
```R
devtools::install_github("tsutomu-nagano/e4ts")
```

## Example
### 1. none missing convert + none weight
```R
library(e4ts)
library(data.table)
df <- data.table(
	F1 = c("A", "A", "B", "B", "B"),
	F2 = c("1", "2", "3", "4", "5")
	)
dimensions <- c("F1")
measure <- measure_sum$new(name = "F2")
st <- stattable(
	df = df,
	dimensions = dimensions,
	measure = measure
	)

print(st)
# A tibble: 2 × 11
  F1    func       value count   sum   min   max  top1  top2  rate added
  <chr> <list>     <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <lgl>
1 A     <measr_sm>     3     2     3     1     2     2     1 0.667 FALSE
2 B     <measr_sm>    12     3    12     3     5     5     4 0.417 FALSE

```

## Other
### GUI
Provides GUI with shiny application.
```R
library(e4ts)
front(host = "0.0.0.0", port = 3000)
```
![screenshot1](/assets/screenshot1.png)
![screenshot2](/assets/screenshot2.png)

### docker
Provides a docker container with a version of R that can run e4ts.
```sh
cd docker
docker-compose up -d

docker exec -it e4ts /bin/bash

$ R
> library(e4ts)
> ........
```
