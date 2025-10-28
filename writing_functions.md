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

    ##  [1] -0.86884302 -0.65678966 -0.10331822  0.34638730 -1.49958910  0.07876022
    ##  [7] -0.14921827  1.62951068 -0.36367643  0.70867862 -2.09879085  0.73072894
    ## [13] -1.08583248 -0.87169663  1.02411059  1.49257318  0.82716980  0.88136551
    ## [19] -0.51099186  0.48946166

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

    ##  [1] -0.86884302 -0.65678966 -0.10331822  0.34638730 -1.49958910  0.07876022
    ##  [7] -0.14921827  1.62951068 -0.36367643  0.70867862 -2.09879085  0.73072894
    ## [13] -1.08583248 -0.87169663  1.02411059  1.49257318  0.82716980  0.88136551
    ## [19] -0.51099186  0.48946166

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.491551968 -1.614995875  0.239489714  0.148762080 -0.467536888
    ##   [6] -0.356573207  1.896225214  0.747964383 -1.025874449 -0.110725621
    ##  [11] -0.044606812 -1.018333568 -0.216728078 -1.747753559 -0.094970186
    ##  [16] -1.237373718 -0.813439320  2.973444349  0.701489997  0.169986484
    ##  [21]  1.761761888 -1.517657289  0.606372313  1.328685594 -1.007306321
    ##  [26] -1.416710164 -0.617361208 -1.160138628  0.506684140  0.247714721
    ##  [31] -0.179708575 -2.257847825 -0.652178249  0.914364935 -0.540402765
    ##  [36] -0.108287962  0.284250918 -0.579543208  2.140726918  0.731292913
    ##  [41] -0.237923642  0.482407957 -0.209192461  1.198403313 -0.696102990
    ##  [46]  0.741136072  0.754666461 -1.337790644 -0.755322079 -0.203973372
    ##  [51]  0.070614319 -0.766286067 -0.850108609  0.591986016  0.004204953
    ##  [56] -1.135891323  0.432132548  0.309076291  0.484659371  0.016540926
    ##  [61] -1.311149241 -0.491306060  0.088686216  0.686337968 -0.071909303
    ##  [66]  0.274196832  0.921693070 -0.348711548 -1.223912211  0.258796829
    ##  [71] -0.790208434  0.672460465  2.030628217  0.395449431  0.333391399
    ##  [76] -0.374508426 -0.816405856  1.500486141  0.727281926  0.342473092
    ##  [81]  0.197734299 -1.019533426 -0.037278763 -1.297859631 -0.156062513
    ##  [86]  0.376411737 -0.928240072 -0.910796701  0.975095932  0.403451490
    ##  [91] -1.467822585  2.320249033 -0.026663664  0.256949504 -0.292713751
    ##  [96]  1.777970322  0.943282511 -0.633326476 -0.143231928  0.065921303
    ## [101]  1.150740645 -1.951334706 -0.082822993  0.495586587  0.707363264
    ## [106]  0.587027245 -0.288280325  0.235613673  1.842628401 -0.321628626
    ## [111] -3.174866998 -0.749205117 -0.509439803  0.044767012  0.093476626
    ## [116] -0.850712387  0.913493631  0.194610007 -1.343676462  0.162235211
    ## [121]  1.955661673  1.334589316  1.332018838

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
    ## 1  8.98  2.65

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
    ## 1   4.03      1.95

Write a function to do simulations

``` r
sim_mean_and_sd = function(n_subj = 30, mu = 3, sigma = 2) {
  
    sim_df =
      tibble(
        x = rnorm(n = n_subj, mean = mu, sd = sigma)
        )
    
    sim_df %>% 
      summarize(
        mu_hat = mean(x),
        sigma_hat = sd(x)
      )
  

}
```

Let’s run this function. This is providing a named argument.

``` r
sim_mean_and_sd(n_subj = 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.14      2.32

What if you just put “50”? By default, it assigns the value to the first
argument.

``` r
sim_mean_and_sd(50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.14      1.81

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
