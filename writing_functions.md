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

    ##  [1] -0.001653831 -0.656902079 -0.823051644  1.055190880  0.173632932
    ##  [6] -0.789116231  0.338014329  2.052961561  0.110402812 -0.229090938
    ## [11] -1.374235540  1.069066225  0.778244790 -2.228347723 -0.811897522
    ## [16]  0.468077919  1.211675900 -0.695572487  0.519930836 -0.167330187

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

    ##  [1] -0.001653831 -0.656902079 -0.823051644  1.055190880  0.173632932
    ##  [6] -0.789116231  0.338014329  2.052961561  0.110402812 -0.229090938
    ## [11] -1.374235540  1.069066225  0.778244790 -2.228347723 -0.811897522
    ## [16]  0.468077919  1.211675900 -0.695572487  0.519930836 -0.167330187

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  1.258353366  1.209055677 -0.841892842  0.126611712 -0.698874082
    ##   [6]  0.919109016 -0.563334964  0.756357852 -1.386645796 -0.837986966
    ##  [11] -0.249801171 -1.189483768  0.544958301 -0.229427766 -0.130557181
    ##  [16]  0.518732575 -1.195139682 -0.878075405 -2.254866514  0.115198018
    ##  [21] -0.878347809 -0.576137374 -0.027276518  0.767245078  0.869461283
    ##  [26]  0.627574597 -1.111417145  0.478496605 -0.543273954  0.914290179
    ##  [31] -0.861240454 -0.110338616 -0.796233236 -1.036899861  1.829682901
    ##  [36]  0.110014131  2.393973245 -1.548017276 -1.710040132 -0.443858348
    ##  [41] -0.683304346 -2.697316275 -0.417404257 -1.422383231  0.470620073
    ##  [46]  1.053904031  1.573217850 -0.848511564  0.859661079 -0.263160412
    ##  [51] -0.001517592 -0.079730211  0.817337654  0.132632777  0.135220290
    ##  [56]  0.045264734  1.175415749 -0.091888021 -0.196870152  0.563115966
    ##  [61]  1.278997267  1.166737495 -0.014958651  1.206401481 -0.306089057
    ##  [66]  1.338291879  1.103104183  0.158279858 -0.177239758  0.424006378
    ##  [71] -0.257182827  0.692662917 -1.779182674  0.025519040 -0.658973237
    ##  [76] -0.878338660 -0.739048573  0.105628398  0.891518795  1.061622218
    ##  [81] -0.987494579 -0.065595057 -0.455870683  1.730540908  0.425395149
    ##  [86] -0.441007834 -0.114486415  0.280095182 -0.430539597 -1.607743744
    ##  [91] -1.704554122 -0.929759546 -0.680501887 -0.106986201 -1.492964573
    ##  [96] -0.035906590  0.759319539  0.464730146 -1.661200683  0.019681918
    ## [101] -1.332706574 -0.016731452  2.294689750 -0.365889005  0.641839578
    ## [106] -0.140906215  0.819756359  1.717151869 -0.597558055  2.633286856
    ## [111] -0.508191313  1.461032629 -1.505681668  0.798772348 -0.641954203
    ## [116]  0.601588124  2.315340140  0.128509544 -0.232680806 -0.517822015
    ## [121]  0.120444207  0.631698568  0.624851710

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
    ## 1  9.05  4.12

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
    ## 1   3.05      2.17

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
    ## 1   3.14      2.42

What if you just put “50”? By default, it assigns the value to the first
argument.

``` r
sim_mean_and_sd(50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.99      1.74

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
