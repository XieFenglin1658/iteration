writing_functions
================
Fenglin Xie
2025-10-26

\##当你用一段code超过两次，写function！

\##argument：输入 \##body：function \##return：输出

\##scoping issue:若function没有找到她需要的input，他会到global
environment中随便找一个匹配的，这会造成很多问题，所以请确保你的input是你的function中定义的，所能操作的，不然他就会自己去global
environment里面找。

\##不要尝试写很复杂的function去包括所有过程，把过程分解成几个简单的小function去处理这个数据会更好，大家都更能看懂你的分析过程，之后自己去回忆也能记起来这一步是为了干嘛。

\##design for simplicity；add complexity only if where you must.

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
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Start small

everyone loves z scores.

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

#z transformation
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.9210459 -0.2080152 -0.0614665  0.2193205 -1.1901990  0.3313575
    ##  [7]  0.8981949 -2.0806242 -1.9637919  1.8682721  0.3126610  0.5155967
    ## [13]  0.3059284  0.9283443 -0.7609738  0.8878484  0.6454027 -0.6850044
    ## [19]  0.3889008  0.5692937

Write a function to compute z scores

\##小括号里面放input，大括号里面定义function（body）

\##z并不是被定义的一个新变量，他只是function中的一部分，不同输入他就是不同输出

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
```

若不写明return，则function的最后一行会被默认为return

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  z
}
```

let’s try our function

``` r
z_scores(x = x_vec)
```

    ##  [1] -0.9210459 -0.2080152 -0.0614665  0.2193205 -1.1901990  0.3313575
    ##  [7]  0.8981949 -2.0806242 -1.9637919  1.8682721  0.3126610  0.5155967
    ## [13]  0.3059284  0.9283443 -0.7609738  0.8878484  0.6454027 -0.6850044
    ## [19]  0.3889008  0.5692937

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  1.9867897512  0.8185241959  2.2964592451 -0.7298969316 -0.4359016374
    ##   [6]  0.3493581351 -1.9360222633  0.7639502171 -0.2815741356  0.9082146697
    ##  [11] -2.1947885694  0.7107071178 -0.1189079452 -0.6966089982  1.2083994242
    ##  [16] -1.6831231630  1.0670332522 -0.2129799395 -0.3019056075 -1.2630103884
    ##  [21]  0.3813302262  0.0899946212 -1.1156906012  1.3040586622  0.3263882752
    ##  [26]  0.0136950174  1.3475083106  0.1942283297  0.5195371413  1.0038118610
    ##  [31]  1.2141213770  0.5634324211  0.2095468991  0.2522268898 -0.2710989777
    ##  [36] -0.2692017200 -1.1826178638  0.0671997069  0.2603538106  0.1058727055
    ##  [41]  2.5279637103 -0.0369592811  0.2763795270 -0.0025260003  1.2306304266
    ##  [46]  1.1146768143  2.0305310501 -0.0001607643 -1.0943926166 -1.4585184268
    ##  [51]  1.3928457975  1.7578635607  0.3284587957 -1.2686363739 -1.2782807895
    ##  [56] -2.6594678338 -0.5072575528  1.9000927025 -0.1988963689 -1.8946510546
    ##  [61] -0.8697155319 -1.6078198799 -0.6015010326 -1.5828551217  0.6510798015
    ##  [66]  1.2128428503  0.0313852855  0.5849225806  0.6245912826  0.7418189045
    ##  [71] -1.1976379119  0.1369841499  1.3828899331 -1.1403862108 -0.4783626929
    ##  [76] -0.8509904100  1.2425263640 -0.8531358570 -0.9393997000 -1.8551614213
    ##  [81] -0.3054273544 -0.1366692070 -0.4522470291 -0.4060370186  0.0199183432
    ##  [86]  0.1536055933  0.7304535366  0.4312859909  0.3567460894  0.4612105664
    ##  [91] -0.8009970947  0.0883457018 -0.6382692934 -1.3655514291 -0.2744064675
    ##  [96] -1.2019418893 -1.3381138627 -0.1544481607  0.6773123065 -0.7866303267
    ## [101] -1.1463343195  0.0635908565 -0.0735727666 -0.0150396713  0.4652104285
    ## [106] -0.1872344644  0.3348893103  0.6471927032  0.9103387937  0.9956304224
    ## [111]  0.1342168977 -1.3978915524  0.3628691827  0.7499921361  1.2137835925
    ## [116]  0.0477144399  1.1210128677 -0.0290387054  0.8798706793 -0.0625761136
    ## [121] -1.6863690769 -0.1644363546 -0.2831425063

Let’s break our function

\##若你的输入不满足设定的function，则function报错

\##knit会报错，但这个error是我们故意犯的，所以确保knit顺利，设定`error = TRUE`

``` r
z_scores(3) ##算sd至少要两个data
```

    ## [1] NA

``` r
z_scores("my name is Jeff") ##input需要是数字
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

\##如果别人使用你的function，你可以添加message，show this is break and
this is why

\##设定的condition是针对这个function的，不会应用到其他地方

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("This input x should be numeric")
  }
  
  if(length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
}
```

## Let’s compute some stuff

Let’s compute and return the mean and sd of a numeric vector

给自己的function设定条件，如果你不想要这个结果，提前设定条件，function
stop，然后报错信息，免得最后一套分析下来是你不想要的结果

\##`c(mean_x, sd_x)`让结果并排呈现

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
  
  c(mean_x, sd_x)
}
```

\##让结果dataframe的形式呈现(nicer way)

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
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.0  3.34

\##Make up data

Let’s *simulate* some data

\##summarize只是取一个sample进行计算吗？？？？

``` r
sim_df = 
  tibble(
    x = rnorm(n = 30, mean = 3, sd = 2)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.96      2.09

Write a function to do simulations

The input are

- `n_subj`is the number of subject
- `mu`is the true mean
- `sigma`is the true standard deviation

Function simulates data from a normal and computes sample mean and sd

\##option+shift–可以选中多行，同时进行相同的修改，比如同时往后缩进一格

\##设定了default value

``` r
sim_mean_sd = function(n_subj = 30, mu = 3, sigma = 2) {
  
  sim_df = 
    tibble(
    x = rnorm(n = n_subj, mean = mu, sd = sigma)
  )

  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
  )

}
```

也可以不设定某个default value，或者都不设定，随你便

``` r
sim_mean_sd = function(n_subj, mu, sigma = 2) {
  
  sim_df = 
    tibble(
    x = rnorm(n = n_subj, mean = mu, sd = sigma)
  )

  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
  )

}
```

\##若设定了default
value，则下面的括号中不写value，function也可以运行，若写了value，则这个value会覆盖掉default
value

\##若没有设定default value，则括号中必须要写value，不然无法运行

``` r
sim_mean_sd(n_subj = 3000, 3)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.00      2.01

\##若你不写明value是谁的value，则会默认把这个值赋到第一个没有default
value的位子上 5000将会被赋值到n_subj上

``` r
sim_mean_sd(mu = 484, 5000)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   484.      2.03

Import the LoTR data

``` r
fellowship_ring =
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "Fellowship of Ring")

two_tower =
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "Two Towers")

return_of_the_king =
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "Return of the King")

lotr_df =
  bind_rows(fellowship_ring, two_tower, return_of_the_king)
```

我们用了三次一样的code！！！function该登场了！！（为了避免copy & paste
会造成的各种问题）

Turn this into function

``` r
Movie = function(cell_range, movie_name) {
  df =
   read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
   mutate(movie = movie_name)
  
  df
}
```

``` r
fellowship_ring = Movie(cell_range = "B3:D6", movie_name = "Fellowship of Ring")

two_tower = Movie(cell_range = "F3:H6", movie_name = "Tow Towers")

return_of_the_king = Movie(cell_range = "J3:L6", movie_name = "Return of the King")

bind_rows(fellowship_ring, two_tower, return_of_the_king)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie             
    ##   <chr>   <dbl> <dbl> <chr>             
    ## 1 Elf      1229   971 Fellowship of Ring
    ## 2 Hobbit     14  3644 Fellowship of Ring
    ## 3 Man         0  1995 Fellowship of Ring
    ## 4 Elf       331   513 Tow Towers        
    ## 5 Hobbit      0  2463 Tow Towers        
    ## 6 Man       401  3589 Tow Towers        
    ## 7 Elf       183   510 Return of the King
    ## 8 Hobbit      2  2673 Return of the King
    ## 9 Man       268  2459 Return of the King

Look at another example

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj_year = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
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

data_marj_month = 
  nsduh_html |> 
  html_table() |> 
  nth(2) |>
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

data_marj_first = 
  nsduh_html |> 
  html_table() |> 
  nth(3) |>
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
```

Write an import function

``` r
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
```

``` r
data_marj_year = nsduh_import(nsduh_html, table_num = 1)
data_marj_month = nsduh_import(nsduh_html, table_num = 2)
```
