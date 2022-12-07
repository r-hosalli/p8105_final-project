Exploratory Analyses
================
Mindy Tran
2022-12-07

``` r
superfund_sites =
  read_csv("./data/superfund_NPL_sites.csv") %>% 
  janitor::clean_names() %>% 
  select(-region_id, -state, -construction_completion_number, -site_listing_narrative, -site_progress_profile, -ends_with("notice"), -restoration_fr_notice_jumper_page, -noid_date, -deletion_date) %>% 
  filter(status == "NPL Site")

superfund_by_county =
  superfund_sites %>% 
  group_by(county) %>% 
  summarize(n_superfund = n()) %>% 
  mutate(
    county = as.factor(county)
  ) %>% 
ggplot( 
  aes(x = county, y = n_superfund, color = county)
  ) + 
  geom_bar(stat = "identity") +
    labs(
    x = " ",
    y = "Number of Superfundsites"
  ) +
  ggtitle("Pennsylvania: Number of Superfund Sites per County")
```
