iteration_and_listcols
================
Fenglin Xie
2025-10-30

需要多次使用同一段codes - iteration output object + sequence to iterate
over + the loop body + input object – for loop

使用purrr::map()来进行loop,和 for
loop效果一样(purrr是tidyverse的package)，不要使用lapply()

list()可以用来生成不同类型变量的组合，chr、numeric、matrices、summaries…
data_frame()需要保证each list entry is a vector with the same
length,可以有多个不同类型的变量组合到一起，print as a table.

list可以以column的形式作为dataframe的一部分（只要length一样）—这代表着，input和output可以一起被save到同一个dataframe当中（keep
track of the result）

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

\##Making a list

``` r
l =
  list(
    vec_numeric = 1:23,
    char_vec = c("Lin"),
    mat = matrix(1:8, nrow = 2, ncol = 4),
    summary = summary(rnorm(1000, mean = 4))
  )

l
```

    ## $vec_numeric
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## 
    ## $char_vec
    ## [1] "Lin"
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.024   3.300   3.991   3.971   4.618   7.374

``` r
##召唤某一个variable
##三种方式--序号/name/$

l[[1]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l[["vec_numeric"]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l$vec_numeric
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

\##Make a different list

``` r
list_normals =
  list(
    a = rnorm(30, mean = 3, sd = 1),
    b = rnorm(30, mean = 30, sd = 1),
    c = rnorm(30, mean = 3, sd = 10),
    d = rnorm(30, mean = -3, sd = 4)
  )
```

## Let’s copy and paste the function from last time

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("This input x should be numeric")
  }
  
  if(length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  tibble(
    mean = mean_x, 
    sd = sd_x)
}
```

``` r
mean_and_sd(list_normals[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  1.14

``` r
mean_and_sd(list_normals[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.0  1.25

``` r
mean_and_sd(list_normals[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.98  11.1

``` r
mean_and_sd(list_normals[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.74  3.37

做的工作都是重复性质的–use a loop to iterate
\##i的值是从1到4（1:4），每个i都跑一遍这个代码`mean_and_sd(list_normals[[i]])`,然后储存下来作为这个variable
i的output，一共有四个output

\##先创建一个empty list，用来储存output

``` r
##define an empty list -- type(list) + 所需（list）长度
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_normals[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  1.14
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.0  1.25
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.98  11.1
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.74  3.37

Use ‘map’ to do the same thing \##map(input +
function)–map就是把input依次按照function进行处理，分别得出结果，不需要define
length. save it in a variable(output)

``` r
output = map(list_normals, mean_and_sd)

output = map(list_normals, median)
```

Check out some `map` variants

``` r
##把结果以dataframe的形式展示出来

map_dfr(list_normals, mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  1.14
    ## 2 30.0   1.25
    ## 3  3.98 11.1 
    ## 4 -1.74  3.37

``` r
##给input命名
map_dfr(list_normals, mean_and_sd, .id = "sample")
```

    ## # A tibble: 4 × 3
    ##   sample  mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a       3.30  1.14
    ## 2 b      30.0   1.25
    ## 3 c       3.98 11.1 
    ## 4 d      -1.74  3.37

``` r
##如果你知道你的结果是single number，使用map_dbl,这样出来的结果就不是list of number，就是直接给你对应的几个数字
map_dbl(list_normals, median)
```

    ##         a         b         c         d 
    ##  3.256539 29.754248  4.386190 -1.081754

## LIST COLUMNS

try to put my list into a dataframe

\##name和sample的length一样所以才可以匹配，name中是四个数据，sample中也是四个数据

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    sample = list_normals
  )
```

Did this really work?

``` r
pull(listcol_df, name)
```

    ## [1] "a" "b" "c" "d"

``` r
pull(listcol_df, sample)
```

    ## $a
    ##  [1] 5.589724 3.473972 4.454349 2.247185 4.297522 3.067717 2.991402 3.220414
    ##  [9] 2.661170 3.221015 3.657003 3.709538 3.604742 3.719905 4.994213 4.705785
    ## [17] 2.740843 1.081738 1.005149 2.136438 1.977799 3.001639 4.038362 3.466080
    ## [25] 3.033404 3.292064 4.659121 2.099292 5.129549 1.846097
    ## 
    ## $b
    ##  [1] 33.11041 31.53399 28.54207 31.32445 31.08441 30.00572 28.59412 29.90533
    ##  [9] 28.75961 29.11558 31.48027 31.11215 29.81648 31.95055 29.93958 30.12864
    ## [17] 29.63232 28.27033 30.46241 29.26609 29.00481 28.37135 29.79268 29.71582
    ## [25] 29.40034 29.51929 28.80137 28.69611 29.39873 32.22368
    ## 
    ## $c
    ##  [1]   5.8626361   2.9838635 -19.9130132  -4.8480096  16.9085674  -1.2182408
    ##  [7]   0.3539777  11.8259391  -1.4653929   0.9221814   8.5906713  18.2602701
    ## [13]   4.5108094 -18.8824511  11.7740402  -1.0609854  -5.2041172  -1.1919668
    ## [19]   7.4157880  10.7804187  22.5396700  17.6842123   4.4384798  -7.0592456
    ## [25]   2.3462425  27.1388912   4.3339006   5.8983483 -14.7327864  10.4672834
    ## 
    ## $d
    ##  [1]  4.05156269 -7.30784734  4.64625422 -2.19255256 -3.96787348 -0.44298034
    ##  [7]  1.06545859 -2.92910848  0.25477300 -4.60539008 -0.88064034  0.01788849
    ## [13] -0.04903867 -3.30357237  1.40464698 -1.28286854 -1.51269695  2.31555712
    ## [19]  2.06380870 -5.95545386 -9.66724982 -7.31759507 -0.35817724 -0.75620291
    ## [25] -3.92345929 -0.30551515 -3.82442990 -1.81809335  0.25854514 -5.80938803

can I apply `mean_and_sd`?

``` r
mean_and_sd(pull(listcol_df, sample)[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  1.14

``` r
mean_and_sd(pull(listcol_df, sample)[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.0  1.25

``` r
mean_and_sd(pull(listcol_df, sample)[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.98  11.1

``` r
mean_and_sd(pull(listcol_df, sample)[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.74  3.37

iterate using `map` \##`pull(listcol_df, sample)`–accessing the
input–sample数据

``` r
map(pull(listcol_df, sample), mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  1.14
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.0  1.25
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.98  11.1
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.74  3.37

我希望我的结果最后也是以dataframe的形式呈现的（方便我进行下一步分析）

adding a column(加一个outcome column)

``` r
listcol_df = 
  listcol_df |> 
  mutate(
    summary = map(sample, mean_and_sd)
  )

pull(listcol_df, summary)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  1.14
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.0  1.25
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.98  11.1
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.74  3.37

\##unnest-把一个数据以更好的形式展现出来（后面会讲）
\##去掉sample是因为我后续不再需要他，并且他这列数据看着有点奇怪

``` r
listcol_df |> 
  select(-sample) |> 
  unnest(summary)
```

    ## # A tibble: 4 × 3
    ##   name   mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      3.30  1.14
    ## 2 b     30.0   1.25
    ## 3 c      3.98 11.1 
    ## 4 d     -1.74  3.37

## Revisit NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_import = function(html, table_num) {
  
  data = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

}

data_marj_year = nsduh_import(nsduh_html, table_num = 1)
data_marj_month = nsduh_import(nsduh_html, table_num = 2)
```

Try this with a `for` loop

``` r
output = vector("list", length = 3)

for (i in 1:3) {
  
  output[[i]] = nsduh_import(html = nsduh_html, i)
  
}
```

Do this with `map`
\##`nsduh_import`这个function需要注明的argument有两个，所以这里额外多了一个信息需要写明

``` r
map(1:3, nsduh_import, html = nsduh_html)
```

    ## [[1]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows
    ## 
    ## [[2]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows
    ## 
    ## [[3]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows

Do this all in a dataframe(更tidy) \##加一个outcome
column，并且用`unnest`展开这个table

``` r
nsduh_df =
  tibble(
    name = c("marj year", "marj month", "marj first"),
    number = 1:3
  ) |> 
  mutate(
    table = map(number, nsduh_import, html = nsduh_html)
  ) |> 
  unnest(table)
```

## Look at weather data

``` r
library(p8105.datasets)
data("weather_df")
```

``` r
weather_df |> 
  filter(name == "CentralPark_NY") |> 
  ggplot(aes(x = tmin, y = tmax)) +
  geom_point() +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](iteration_and_listcols_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Let’s do a regression

\##有时候这个function可能not expect 一个dataframe
作为data进入，这个时候需要指明这个data放在哪里，也就是使用这个code–`data = _`即可

``` r
weather_df |> 
  filter(name == "CentralPark_NY") |>
  lm(tmax ~ tmin, data = _)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = filter(weather_df, name == "CentralPark_NY"))
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
weather_df |> 
  filter(name == "Molokai_HI") |>
  lm(tmax ~ tmin, data = _)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = filter(weather_df, name == "Molokai_HI"))
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
weather_df |> 
  filter(name == "Waterhole_WA") |>
  lm(tmax ~ tmin, data = _)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = filter(weather_df, name == "Waterhole_WA"))
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

一直在做重复工作，如果有一百个地址，还这么一个一个code的话会非常麻烦！！

Let’s iterate differently
\##nest–把从date到tmin的那几列都聚合到一个column中，和unnest功能相反

``` r
weather_nest =
  weather_df |> 
  nest(data = date:tmin)
```

``` r
lm(tmax ~ tmin, data = pull(weather_nest, data)[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = pull(weather_nest, data)[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = pull(weather_nest, data)[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = pull(weather_nest, data)[[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = pull(weather_nest, data)[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = pull(weather_nest, data)[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

Do this using `map`

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}
```

``` r
map(pull(weather_nest, data), weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

依旧dataframe–keep track of everything in one place

``` r
weather_nest |> 
  mutate(
    lm_fits = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_fits
    ##   <chr>          <chr>       <list>             <list> 
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>   
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>   
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
