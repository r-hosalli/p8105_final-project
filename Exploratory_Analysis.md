Exploratory Analyses
================
Mindy Tran
2022-12-07

``` r
superfund_sites =
  read_csv("./data/superfund_NPL_sites.csv") %>% 
  janitor::clean_names() %>% 
  select(-region_id,-construction_completion_number, -site_listing_narrative, -site_progress_profile, -ends_with("notice"), -restoration_fr_notice_jumper_page, -noid_date, -deletion_date) %>% 
  filter(status == "NPL Site")

superfund_by_county =
  superfund_sites %>% 
  group_by(county) %>% 
  summarize(n_superfund = n()) %>% 
  mutate(
    county = as.factor(county)) %>%
  as.data.frame(superfund_by_county)

ggplot(superfund_by_county,
aes(x = reorder(county, -n_superfund), y = n_superfund, color = county, fill = county)) + 
  geom_bar(stat = "identity") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    legend.position = "none") +
   labs(
    x = " ",
    y = "Number of Superfund Sites"
  ) +
  ggtitle("Pennsylvania: Number of Superfund Sites per County")
```

![](Exploratory_Analysis_files/figure-gfm/plot%201-1.png)<!-- -->

``` r
health_outcomes = 
  read_csv("./data/PLACES_County_2022.csv") %>% 
  janitor::clean_names() %>% 
  rename(county = county_name, state = state_abbr) %>% 
  mutate(
    county = as.factor(county)
  ) %>% 
  select(state, county, casthma_crude_prev,casthma_crude95ci, cancer_crude_prev,cancer_crude95ci ) %>% 
  filter(state == "PA") %>%
  mutate(asthma_ci = str_remove_all(casthma_crude95ci, '[()]'),
         cancer_ci = str_remove_all(cancer_crude95ci, '[()]')) %>%
  select(-casthma_crude95ci,-cancer_crude95ci) %>%
  separate(asthma_ci, into = c("asthma_lower", "asthma_upper"), sep = ",") %>%
   separate(cancer_ci, into = c("cancer_lower", "cancer_upper"), sep = ",") %>%
  mutate(
    asthma_lower = as.numeric(asthma_lower),
    asthma_upper = as.numeric(asthma_upper),
    cancer_lower = as.numeric(cancer_lower),
    cancer_upper = as.numeric(cancer_upper)
  )
  
  
ggplot(health_outcomes,
aes(x = reorder(county,-casthma_crude_prev), y = casthma_crude_prev, color = county)) + 
  geom_point() +
  geom_errorbar(aes(ymin = asthma_lower, ymax = asthma_upper)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none") +
   labs(
    x = " ",
    y = "Crude Asthma Prevalence"
  ) +
  ggtitle("Prevalence of Asthma Among Adults 18+ by County 2020")
```

![](Exploratory_Analysis_files/figure-gfm/plot%202%20-1.png)<!-- -->

``` r
ggplot(health_outcomes,
aes(x = reorder(county,-cancer_crude_prev), y = cancer_crude_prev, color = county)) + 
  geom_point() +
  geom_errorbar(aes(ymin = cancer_lower, ymax = cancer_upper)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none") +
   labs(
    x = " ",
    y = "Crude Cancer Prevalence"
  ) +
  ggtitle("Prevalence of Cancer Among Adults 18+ by County 2020")
```

![](Exploratory_Analysis_files/figure-gfm/plot%202%20-2.png)<!-- -->
