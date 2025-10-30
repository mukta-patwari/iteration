writing_functions
================
Mukta Patwari
2025-10-23

## Start small

z-scores

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.8101192 -0.0506038 -0.2251663  0.7586408 -0.7386099 -0.5363705
    ##  [7] -0.8420656  1.4152150 -0.8823984 -0.1578469 -1.4445372 -1.1942013
    ## [13]  0.9841141  1.0475372  1.1777000  0.5401420 -1.7053122  1.1992307
    ## [19]  0.1449476  1.3197038

Function to compute z scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
}
```

Let’s try our function…

``` r
z_scores(x = x_vec)
```

    ##  [1] -0.8101192 -0.0506038 -0.2251663  0.7586408 -0.7386099 -0.5363705
    ##  [7] -0.8420656  1.4152150 -0.8823984 -0.1578469 -1.4445372 -1.1942013
    ## [13]  0.9841141  1.0475372  1.1777000  0.5401420 -1.7053122  1.1992307
    ## [19]  0.1449476  1.3197038

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.450862437 -0.932520915 -0.430275839  0.841313975  0.298354971
    ##   [6]  0.561336304  1.385178374  0.760369782  1.933878994 -0.682589969
    ##  [11] -0.837481058  0.333395779 -0.339515741 -1.250845186  0.143539451
    ##  [16]  0.820679495  0.738886297  0.927278727  1.933993576  1.421868608
    ##  [21] -0.185958623  1.076722019 -0.406424231 -1.322522485 -0.197670751
    ##  [26] -0.368625338  0.435716297  0.959497194 -1.028307349  0.962173169
    ##  [31] -1.293501556  0.285489498  0.070668202  1.063744486 -0.659026710
    ##  [36]  1.523244664  0.387488909  1.266572167 -1.771592958  3.307137862
    ##  [41]  0.408382239 -0.789095316  0.960786653  0.272812962 -0.712870475
    ##  [46] -0.062611221 -1.730371689  0.209441343 -0.443634328  0.555164535
    ##  [51] -0.530621575 -0.901899112  0.475982893 -1.207000753  0.145633718
    ##  [56]  1.698083685 -0.555945035  0.851472357  1.756745881  0.436635775
    ##  [61]  0.302737431 -0.609334142 -1.553079323 -1.234760577 -1.002727588
    ##  [66] -0.620549997 -0.539053617 -0.071948759 -0.006402027 -0.117653751
    ##  [71]  0.681952570  0.051935439 -0.184050631 -0.150023499 -1.280929940
    ##  [76] -0.835554466 -1.110813658 -0.403820354  1.154937132  1.070005528
    ##  [81] -1.311654822  1.959181532 -1.874972616 -0.075645907 -0.492852383
    ##  [86]  0.475934076  0.450590734 -0.249227449 -0.051715434 -0.973395733
    ##  [91]  0.210263410  0.090471947  1.867440143 -1.643644907 -1.242024727
    ##  [96] -1.458489060  0.034926251  0.422911184  0.198080304 -0.693797423
    ## [101] -1.509358722  0.589239914  0.504127578 -2.017962311  0.076896028
    ## [106] -0.307526627 -0.049246347 -1.301178859  0.624805051  0.599995817
    ## [111] -0.049636489  1.445258174 -0.393747862  0.785622315 -1.041319900
    ## [116] -0.692031688  2.193658323 -0.259578319 -1.792915895  0.279518447
    ## [121]  0.520718317  1.290572933 -0.797054959

Let’s break our function

``` r
z_scores(3)
```

    ## Error in z_scores(3): Only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is Mukta")
```

    ## Error in z_scores("my name is Mukta"): The input x should be numeric

## Computing

Compute and return mean and SD of a numeric vector

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } 
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.3  3.25

Simulation

``` r
sim_df =
  tibble(x = rnorm(n = 30, mean = 3, sd = 2))

sim_df %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.94      2.15

Write a function to do simulations

(We wrote this code chunk last time, now it’s being sourced.)

``` r
source("source/sim_mean_sd.R")
```

Let’s run this function. This is providing a named argument.

``` r
sim_mean_and_sd(n_subj = 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.52      1.66

What if you just put “50”? By default, it assigns the value to the first
argument.

``` r
sim_mean_and_sd(50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.26      2.05

Import LoTR data

``` r
fellowship_ring =
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "Fellowship of the Ring")

two_towers =
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "Two Towers")

return_of_the_king =
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "Return of the King")

lotr_df =
  bind_rows(fellowship_ring, two_towers, return_of_the_king) %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words")  %>%  
  mutate(race = str_to_lower(race)) %>%  
  select(movie, everything()) 
```

Turn this into a function

``` r
lotr_import = function(cell_range, movie_title) {
  
  df =
    read_excel("data/LotR_Words.xlsx", range = cell_range) %>%
    mutate(movie = movie_title)
  
  df
  
}

fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_tower = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return = lotr_import(cell_range = "J3:L6", movie_title = "Return of the King")

bind_rows(fellowship, two_towers, return)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie             
    ##   <chr>   <dbl> <dbl> <chr>             
    ## 1 Elf      1229   971 Fellowship        
    ## 2 Hobbit     14  3644 Fellowship        
    ## 3 Man         0  1995 Fellowship        
    ## 4 Elf       331   513 Two Towers        
    ## 5 Hobbit      0  2463 Two Towers        
    ## 6 Man       401  3589 Two Towers        
    ## 7 Elf       183   510 Return of the King
    ## 8 Hobbit      2  2673 Return of the King
    ## 9 Man       268  2459 Return of the King

Look at one more example

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
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
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}
```
