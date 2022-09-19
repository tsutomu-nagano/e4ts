# e4ts
engine for statistical tables using tidy datas.

## Overview
e4ts は簡易に統計表を作成するためのライブラリです。
主に次の機能があります。
- 欠測値の変換
- 合計算出の設定
- 複数の集計方法

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
shiny アプリケーションによるGUIを提供します。
```R
library(e4ts)
front(host = "0.0.0.0", port = 3000)
```
![screenshot1](/assets/screenshot1.png)
![screenshot2](/assets/screenshot2.png)
