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

    ##  [1] -1.1624166  0.2279242  1.1987837  1.0678639 -0.4609539  1.1756121
    ##  [7] -0.4332423 -0.4857219  0.1977500 -0.1815509  0.9578502  0.2936937
    ## [13] -0.9477798 -2.2852612  0.1782789  0.5353659 -1.2245739  1.3126471
    ## [19] -1.0458757  1.0816063

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

    ##  [1] -1.1624166  0.2279242  1.1987837  1.0678639 -0.4609539  1.1756121
    ##  [7] -0.4332423 -0.4857219  0.1977500 -0.1815509  0.9578502  0.2936937
    ## [13] -0.9477798 -2.2852612  0.1782789  0.5353659 -1.2245739  1.3126471
    ## [19] -1.0458757  1.0816063

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  1.430123347  1.191519568 -2.118327450  0.831035826 -1.304762628
    ##   [6] -0.437583670  0.021256008  0.303708902 -1.869880382  1.734295831
    ##  [11]  0.012159091 -0.174196527 -0.408405449 -1.874918813 -1.952697174
    ##  [16]  0.370611030 -1.178106114 -1.966287979  0.329151545  0.210679406
    ##  [21]  0.289945612  0.896099964  0.602733462  1.295897441  0.390156297
    ##  [26] -0.945790702  0.099509489 -1.854071280 -1.810221658 -0.677044835
    ##  [31] -0.486503503  0.557986804  0.590496538  0.419924878  1.683418039
    ##  [36]  1.422124291 -0.897230183 -1.429268209  2.021739986 -1.668877061
    ##  [41]  0.761016700 -1.285115876  1.208797147 -0.105100341  0.492393237
    ##  [46]  1.278378147  1.191813561  0.445431911 -0.919919925  0.044832249
    ##  [51]  0.916996117 -0.904375807  0.166170188  2.496302974 -1.495092135
    ##  [56]  1.087944272 -0.020243455 -1.306705203 -1.418455978  0.904723684
    ##  [61] -0.557618599 -0.440989084  0.011364142  0.133639852 -0.020304476
    ##  [66] -0.808018164  1.118723376  0.006161354  0.134094853 -0.630840469
    ##  [71] -0.478775214  0.107292939  0.268979536 -0.532986289  0.071005324
    ##  [76] -1.048420176  0.237804039  0.072777483  0.009432795 -0.716789643
    ##  [81] -0.193794357  1.038946726 -0.312898000  1.588959437  1.327086743
    ##  [86] -0.817792364  0.571869342  0.438226095 -0.188182815 -0.507436663
    ##  [91]  1.800546769  1.844890245  0.220395531 -0.036127511 -1.327545541
    ##  [96]  1.044009635 -0.311974900  1.127587702 -0.142863230  1.565883369
    ## [101]  0.526731944 -0.527611444 -0.902393125 -0.669057432  0.170306552
    ## [106]  1.261074935 -1.075682455 -0.152348001  0.753817108  0.206304364
    ## [111] -1.387863615  0.151624121 -0.466654649  0.856010621 -0.016846410
    ## [116] -1.363164388 -0.425439765  0.571160320 -1.757105345  0.285314539
    ## [121] -0.614932028  1.023326694  0.696916465

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
    ## 1  10.5  3.64

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
    ## 1   2.57      2.19

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
    ## 1   3.02      2.01

\##若你不写明value是谁的value，则会默认把这个值赋到第一个没有default
value的位子上 5000将会被赋值到n_subj上

``` r
sim_mean_sd(mu = 484, 5000)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   484.      2.04

\##刚刚写好了documentation，可以直接用自己写好的，不用写代码,相当于这是一个可以直接用的function,不需要我们自己去定义了

``` r
source("source/sim_mean_sd.R")
```

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
