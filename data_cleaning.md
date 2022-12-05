Data Cleaning
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
health_outcomes = 
  read_csv("./data/PLACES_County_2022.csv") %>% 
  janitor::clean_names() %>% 
  select(-state_abbr, -state_desc)
```

    ## Rows: 67 Columns: 126
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (64): StateAbbr, StateDesc, CountyName, ACCESS2_Crude95CI, ACCESS2_Adj95...
    ## dbl (61): CountyFIPS, ACCESS2_CrudePrev, ACCESS2_AdjPrev, ARTHRITIS_CrudePre...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
superfund_sites =
  read_csv("./data/superfund_NPL_sites.csv") %>% 
  janitor::clean_names() %>% 
  select(-region_id, -state, -construction_completion_number, -site_listing_narrative, -site_progress_profile, -proposed_fr_notice, -listing_fr_notice, -noid_fr_notice, -deletion_fr_notice, -restoration_fr_notice_jumper_page, -x, -y, -noid_date, -deletion_date) %>% 
  filter(status == "NPL Site")
```

    ## Rows: 126 Columns: 27
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (18): Site Name, Site EPA ID, State, City, County, Status, Proposed Date...
    ## dbl  (8): Site Score, SEMS ID, Region ID, Latitude, Longitude, Construction ...
    ## lgl  (1): Restoration FR Notice Jumper Page
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
