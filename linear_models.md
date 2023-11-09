linear_models
================
Ruoxi Li
2023-11-09

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
set.seed(1)
library(broom)
```

``` r
data("nyc_airbnb")
nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) |> 
  filter(borough != "Staten Island") |> 
  select(price, stars, borough, neighborhood, room_type)
```

Let’s fit a model!

``` r
fit = lm(price~stars+borough,data=nyc_airbnb)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (因为不存在，9962个观察量被删除了)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

``` r
summary(fit)$coef
```

    ##                   Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)      -70.41446  14.020697 -5.022180 5.137589e-07
    ## stars             31.98989   2.527500 12.656733 1.269392e-36
    ## boroughBrooklyn   40.50030   8.558724  4.732049 2.232595e-06
    ## boroughManhattan  90.25393   8.567490 10.534465 6.638618e-26
    ## boroughQueens     13.20617   9.064879  1.456850 1.451682e-01

tidy up the output instead

``` r
fit |>
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

tidy up the coefficients

``` r
fit|>
  broom::tidy() |>
#a tibble
  mutate(term = str_replace(term,"^borough","Borough:"))|>
  knitr::kable(digits=3)
```

| term              | estimate | std.error | statistic | p.value |
|:------------------|---------:|----------:|----------:|--------:|
| (Intercept)       |  -70.414 |    14.021 |    -5.022 |   0.000 |
| stars             |   31.990 |     2.527 |    12.657 |   0.000 |
| Borough:Brooklyn  |   40.500 |     8.559 |     4.732 |   0.000 |
| Borough:Manhattan |   90.254 |     8.567 |    10.534 |   0.000 |
| Borough:Queens    |   13.206 |     9.065 |     1.457 |   0.145 |

``` r
fit = 
  nyc_airbnb |>
  mutate(
    borough = fct_infreq(borough),
    room_type=fct_infreq(room_type)
  )|>
  lm(price~stars+borough+room_type,data= _)
fit|>
  broom::tidy()
```

    ## # A tibble: 7 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              113.      11.8       9.54 1.56e-21
    ## 2 stars                     21.9      2.43      9.01 2.09e-19
    ## 3 boroughBrooklyn          -40.3      2.15    -18.8  4.62e-78
    ## 4 boroughQueens            -55.5      3.59    -15.4  1.32e-53
    ## 5 boroughBronx             -63.0      8.22     -7.67 1.76e-14
    ## 6 room_typePrivate room   -105.       2.05    -51.2  0       
    ## 7 room_typeShared room    -129.       6.15    -21.0  2.24e-97

## Quick look at diagnostics

``` r
nyc_airbnb |>
  modelr::add_residuals(fit)|>
  #ggplot(aes(x=resid)) +
  # geom_density()+
  # xlim(-100,500)
  ggplot(aes(x=stars,y=resid))+
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

![](linear_models_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
nyc_airbnb |>
  modelr::add_residuals(fit)|>
   ggplot(aes(x = borough, y = resid)) + geom_violin()
```

    ## Warning: Removed 9962 rows containing non-finite values (`stat_ydensity()`).

![](linear_models_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

## Hypothesis test for categorical predictor

fit a null and alternative

``` r
fit_null = lm(price~stars+borough,data=nyc_airbnb) 
fit_alternative = lm(price~stars+borough+room_type,data=nyc_airbnb) 
anova(fit_null,fit_alternative)|>
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30525 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30523 9.21e8     2  8.42e7     1394.       0

## Borough-level difference

``` r
fit = 
  nyc_airbnb |>
  lm(price = stars*borough +room_type*borough ,data = _)
```

    ## Warning: In lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
    ##  extra argument 'price' will be disregarded

``` r
fit |>
  broom::tidy()
```

    ## # A tibble: 184 × 5
    ##    term                          estimate std.error statistic  p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                    130.        39.4      3.30  0.000971
    ##  2 stars                            0.776      2.65     0.292 0.770   
    ##  3 boroughBrooklyn                 25.5       41.2      0.619 0.536   
    ##  4 boroughManhattan               107.        38.2      2.79  0.00532 
    ##  5 boroughQueens                   11.0       42.2      0.260 0.795   
    ##  6 neighborhoodArverne             20.5       29.0      0.708 0.479   
    ##  7 neighborhoodAstoria              7.45      20.6      0.363 0.717   
    ##  8 neighborhoodBath Beach         -16.2       72.2     -0.224 0.823   
    ##  9 neighborhoodBattery Park City   -6.26      30.3     -0.206 0.837   
    ## 10 neighborhoodBay Ridge          -14.4       27.2     -0.529 0.597   
    ## # ℹ 174 more rows

``` r
airbnb_lm =function(df){
  lm(price~stars +room_type,data=df)
}
nyc_airbnb |>
  nest(df = -borough) |>
  mutate(
    models = map(df,airbnb_lm),
    results = map(models,broom::tidy)
  )|>
  select(borough,results)|>
  unnest(results)|>
  select(borough,term,estimate)|>
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) |>
  knitr::kable(digits=2)
```

| borough   | (Intercept) | stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|------:|----------------------:|---------------------:|
| Bronx     |       90.07 |  4.45 |                -52.91 |               -70.55 |
| Queens    |       91.58 |  9.65 |                -69.26 |               -94.97 |
| Brooklyn  |       69.63 | 20.97 |                -92.22 |              -105.84 |
| Manhattan |       95.69 | 27.11 |               -124.19 |              -153.64 |

same thing but just a little different

``` r
nyc_airbnb |>
  nest(df = -borough) |>
  mutate(
    models = map(df,\(df) lm(price~stars +room_type,data=df)),
    results = map(models,broom::tidy)
  )|>
  select(borough,results)|>
  unnest(results)|>
  select(borough,term,estimate)|>
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) |>
  knitr::kable(digits=2)
```

| borough   | (Intercept) | stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|------:|----------------------:|---------------------:|
| Bronx     |       90.07 |  4.45 |                -52.91 |               -70.55 |
| Queens    |       91.58 |  9.65 |                -69.26 |               -94.97 |
| Brooklyn  |       69.63 | 20.97 |                -92.22 |              -105.84 |
| Manhattan |       95.69 | 27.11 |               -124.19 |              -153.64 |

## Homicide

``` r
baltimore_df = 
  read_csv("homicide-data.csv") |>
    filter(city == "Baltimore") |> 
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"),
    victim_age = as.numeric(victim_age),
    victim_race = fct_relevel(victim_race, "White")) |> 
  select(resolved, victim_age, victim_race, victim_sex)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

fitting a logistic regression

``` r
fit_logistic =
  baltimore_df |>
  glm(
    resolved ~ victim_age +victim_race+victim_sex,
    data = _,
    family = binomial()
  )
```

``` r
fit_logistic |> 
  broom::tidy() |> 
  mutate(OR = exp(estimate)) |>
  select(term, log_OR = estimate, OR, p.value) |> 
  knitr::kable(digits = 3)
```

| term                | log_OR |    OR | p.value |
|:--------------------|-------:|------:|--------:|
| (Intercept)         |  1.190 | 3.287 |   0.000 |
| victim_age          | -0.007 | 0.993 |   0.027 |
| victim_raceAsian    |  0.296 | 1.345 |   0.653 |
| victim_raceBlack    | -0.842 | 0.431 |   0.000 |
| victim_raceHispanic | -0.265 | 0.767 |   0.402 |
| victim_raceOther    | -0.768 | 0.464 |   0.385 |
| victim_sexMale      | -0.880 | 0.415 |   0.000 |
