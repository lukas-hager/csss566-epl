Basic Overview of EPL Data
================
Lukas Hager
5/4/2022

## General Information

-----

### Data Import

To ensure that we don’t omit any observations, we can read in each
individual file and combine them:

``` r
data_og <- bind_rows(
  lapply(list.files('/Users/hlukas/Google Drive/Raw Data/EPL/Datasets',
                    full.names = TRUE,
                    pattern = '\\d{4}\\-\\d{2}\\.csv'),
         read_csv,
         col_types = cols())
) %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date, '%d/%m/%y'))
```

### Summary Statistics

``` r
cat(str_interp('There are ${nrow(data_og)} observations in the data.'))
```

There are 7260 observations in the data.

We should confirm the level of granularity in the data:

``` r
data_og %>% 
  select(date, home_team, away_team) %>% 
  distinct() %>% 
  nrow()
```

    ## [1] 7257

So we know that each observation is a match. Let’s look at matches by
year:

``` r
data_og %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(year)
```

    ## # A tibble: 20 × 2
    ##     year     n
    ##    <int> <int>
    ##  1  2000   205
    ##  2  2001   373
    ##  3  2002   182
    ##  4  2003   188
    ##  5  2004   392
    ##  6  2005   374
    ##  7  2006   394
    ##  8  2007   371
    ##  9  2008   379
    ## 10  2009   378
    ## 11  2010   374
    ## 12  2011   377
    ## 13  2012   391
    ## 14  2013   372
    ## 15  2014   380
    ## 16  2015   380
    ## 17  2016   378
    ## 18  2017   401
    ## 19  2018   331
    ## 20  2020   640

This is kind of weird – there are theoretically 380 EPL matches each
year, so anything below that is odd, and anything above that (2012,
2017, 2020) is also strange. *TODO Analysis*

We also want to know number of bookmakers by match, so we need to
reshape the data (I’m also combining a step to get the implied odds from
the bookmakers):

``` r
data_og %>% 
  select(date:bsa) %>% 
  pivot_longer(b365h:bsa) %>% 
  mutate(pred_result = str_sub(name, -1),
         bookmaker = str_sub(name, 1, -2),
         value = 1/value) %>% 
  # divide by sum to remove vig
  group_by(date, home_team, away_team, bookmaker) %>% 
  mutate(value = value / sum(value)) %>% 
  ungroup() %>% 
  select(-name) %>% 
  pivot_wider(names_from = pred_result,
              values_from = value) %>% 
  group_by(date, home_team, away_team) %>% 
  summarise(n_bookmakers = sum(!is.na(h))) %>% 
  ungroup() %>% 
  group_by(year = year(date)) %>% 
  summarise(min = min(n_bookmakers),
            mean = mean(n_bookmakers),
            median = median(n_bookmakers),
            max = max(n_bookmakers)) %>% 
  ungroup() %>% 
  arrange(year)
```

    ## # A tibble: 20 × 5
    ##     year   min  mean median   max
    ##    <int> <int> <dbl>  <dbl> <int>
    ##  1  2000     0  0         0     0
    ##  2  2001     0  0         0     0
    ##  3  2002     0  0         0     0
    ##  4  2003     0  0         0     0
    ##  5  2004     0  0         0     0
    ##  6  2005     0  0         0     0
    ##  7  2006     0  0         0     0
    ##  8  2007     0  0         0     0
    ##  9  2008     0  0         0     0
    ## 10  2009     0  5.19     10    10
    ## 11  2010    10 10        10    10
    ## 12  2011    10 10        10    10
    ## 13  2012     8  9.49      9    10
    ## 14  2013     7  7.98      7     9
    ## 15  2014     6  6.61      7     7
    ## 16  2015     5  5.99      6     6
    ## 17  2016     6  6         6     6
    ## 18  2017     6  6         6     6
    ## 19  2018     5  5.52      6     6
    ## 20  2020     0  2.02      0     5

So it looks like some of the bookmakers fade in and out of the sample,
with the best coverage from 2009-2011. Effectively, it seems like the
most viable subset is from 2009-2018.
