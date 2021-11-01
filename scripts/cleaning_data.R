# Info ----
## Author: Sean Bock
## Project: Partisan ID Switching
## Date: 11/1/21
## Note: This script should be run first. It loads, organizes, and cleans data for plotting.


# Loading packages ----

## these lines install necessary packages if not already installed
if (!require(pacman))
  install.packages("pacman")
if (!require(devtools))
  install.packages("devtools")
if (!require(drlib))
  devtools::install_github("dgrtwo/drliband")


pacman::p_load(tidyverse, ggpubr, glue, drlib)

# loading data ---
rm(list = ls()) ##clearing environment


data <- haven::read_dta("data/gss2020panel_r1.dta") %>%
  mutate(id = row_number())

# leaving data in wide format since we're comparing changes in same row
data_r <- data %>%
  filter(panstat == 1) %>%
  select(id, yearid, samptype, starts_with("partyid"), weights = wtssnr_2) %>%
  mutate(across(as.numeric())) %>% ## making all vars numeric first
  haven::zap_labels()  ##sometimes Stata labels get annoying. Removing.


# cleaning data ----

## identifying party switchers -------

#coding main party id variables for each wave. 1a = 2016 wave. 1b = 2018 wave. 2 = 2020.
data_r %<>%
  mutate(
    across(starts_with("partyid_"), ~ factor(
      .x,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7),
      labels = c(
        "Strong Democrat",
        "Democrat",
        "Leans Democrat",
        "Independent",
        "Leans Republican",
        "Republican",
        "Strong Republican",
        "Other party"
      )
    )),
    across(
      starts_with("partyid_"),
      ~ fct_collapse(
        .x,
        "Other party" = c("Other party"),
        Republican = c("Strong Republican", "Republican", "Leans Republican"),
        Independent = c("Independent"),
        Democrat = c("Strong Democrat", "Democrat", "Leans Democrat")
      ),
      .names = "party3_{.col}"
    )
  ) %>%
  rename_with(.cols = starts_with("party3_"), ~ str_remove(., "partyid_")) ##cleaning up party3 var names


# identifying and labeling switchers -----

switchers <- data_r %>%
  select(id, samptype, starts_with("party3"), weights) %>%
  mutate(
    switcher16 = case_when(
      samptype == 2016 &
        party3_1a == "Democrat" &
        party3_2 == "Independent" ~ "Democrats to Independent",
      samptype == 2016 &
        party3_1a == "Democrat" &
        party3_2 == "Republican" ~ "Democrats to Republican",
      samptype == 2016 &
        party3_1a == "Democrat" &
        party3_2 == "Other party" ~ "Democrats to Other party",
      samptype == 2016 &
        party3_1a == "Democrat" &
        party3_2 == "Democrat" ~ "Democrats stayed Democrat",
      samptype == 2016 &
        party3_1a == "Independent" &
        party3_2 == "Democrat" ~ "Independents to Democrat",
      samptype == 2016 &
        party3_1a == "Independent" &
        party3_2 == "Republican" ~ "Independents to Republican",
      samptype == 2016 &
        party3_1a == "Independent" &
        party3_2 == "Other party" ~ "Independents to Other party",
      samptype == 2016 &
        party3_1a == "Independent" &
        party3_2 == "Independent" ~ "Independents stayed Independent",
      samptype == 2016 &
        party3_1a == "Republican" &
        party3_2 == "Democrat" ~ "Republicans to Democrat",
      samptype == 2016 &
        party3_1a == "Republican" &
        party3_2 == "Independent" ~ "Republicans to Independent",
      samptype == 2016 &
        party3_1a == "Republican" &
        party3_2 == "Other party" ~ "Republicans to Other party",
      samptype == 2016 &
        party3_1a == "Republican" &
        party3_2 == "Republican" ~ "Republicans stayed Republican",
      samptype == 2016 &
        party3_1a == "Other party" &
        party3_2 == "Democrat" ~ "Other party to Democrat",
      samptype == 2016 &
        party3_1a == "Other party" &
        party3_2 == "Independent" ~ "Other party to Independent",
      samptype == 2016 &
        party3_1a == "Other party" &
        party3_2 == "Republican" ~ "Other party to Republican",
      samptype == 2016 &
        party3_1a == "Other party" &
        party3_2 == "Other party" ~ "Other party stayed Other party",
      samptype == 2016 & TRUE ~ "Missing in at least one wave"
    )
    ,
    switcher18 = case_when(
      samptype == 2018 &
        party3_1b == "Democrat" &
        party3_2 == "Independent" ~ "Democrats to Independent",
      samptype == 2018 &
        party3_1b == "Democrat" &
        party3_2 == "Republican" ~ "Democrats to Republican",
      samptype == 2018 &
        party3_1b == "Democrat" &
        party3_2 == "Other party" ~ "Democrats to Other party",
      samptype == 2018 &
        party3_1b == "Democrat" &
        party3_2 == "Democrat" ~ "Democrats stayed Democrat",
      samptype == 2018 &
        party3_1b == "Independent" &
        party3_2 == "Democrat" ~ "Independents to Democrat",
      samptype == 2018 &
        party3_1b == "Independent" &
        party3_2 == "Republican" ~ "Independents to Republican",
      samptype == 2018 &
        party3_1b == "Independent" &
        party3_2 == "Other party" ~ "Independents to Other party",
      samptype == 2018 &
        party3_1b == "Independent" &
        party3_2 == "Independent" ~ "Independents stayed Independent",
      samptype == 2018 &
        party3_1b == "Republican" &
        party3_2 == "Democrat" ~ "Republicans to Democrat",
      samptype == 2018 &
        party3_1b == "Republican" &
        party3_2 == "Independent" ~ "Republicans to Independent",
      samptype == 2018 &
        party3_1b == "Republican" &
        party3_2 == "Other party" ~ "Republicans to Other party",
      samptype == 2018 &
        party3_1b == "Republican" &
        party3_2 == "Republican" ~ "Republicans stayed Republican",
      samptype == 2018 &
        party3_1b == "Other party" &
        party3_2 == "Democrat" ~ "Other party to Democrat",
      samptype == 2018 &
        party3_1b == "Other party" &
        party3_2 == "Independent" ~ "Other party to Independent",
      samptype == 2018 &
        party3_1b == "Other party" &
        party3_2 == "Republican" ~ "Other party to Republican",
      samptype == 2018 &
        party3_1b == "Other party" &
        party3_2 == "Other party" ~ "Other party stayed Other party",
      samptype == 2018 & TRUE ~ "Missing in at least one wave"
    )
  )


## calculating proportion of switchers ------

# proportion of switchers in 2016
switch_props <- switchers %>%
  filter(samptype == 2016) %>%
  count(switcher16, wt = weights) %>% ##weighted counts of respondents
  mutate(prop = n / sum(n),
         year = 2016,
         type = switcher16) %>%
  select(-switcher16)

## adding switchers in 2018
switch_props <- switchers %>%
  filter(samptype == 2018) %>%
  count(switcher18, wt = weights) %>%
  mutate(prop = n / sum(n),
         year = 2018,
         type = switcher18) %>%
  select(-switcher18) %>%
  bind_rows(switch_props)

switch_props %<>%
  group_by(year) %>%
  arrange(year, prop) %>%
  mutate(order = row_number()) ##adding ordering variable for plotting


## switching by party in first wave -----

## 2016
switch_props_byparty <- switchers %>%
  filter(samptype == 2016) %>%
  count(switcher16, wt = weights) %>%
  mutate(prop = n / sum(n),
         year = 2016,
         type = switcher16) %>%
  select(-switcher16) %>%
  mutate(
    party_first = case_when(
      startsWith(type, "Democrat") ~ "Democrat",
      startsWith(type, "Independent") ~ "Independent",
      startsWith(type, "Republican") ~ "Republican",
      startsWith(type, "Other") ~ "Other party"
    ),
    party_second = case_when(
      endsWith(type, "Democrat") ~ "Democrat",
      endsWith(type, "Independent") ~ "Independent",
      endsWith(type, "Republican") ~ "Republican",
      endsWith(type, "Other party") ~ "Other party"
    )
  ) %>%
  group_by(party_first) %>%
  mutate(party_sum = sum(prop),
         party_prop = prop / party_sum)

##2018
switch_props_byparty <- switchers %>%
  filter(samptype == 2018) %>%
  count(switcher18, wt = weights) %>%
  mutate(prop = n / sum(n),
         year = 2018,
         type = switcher18) %>%
  select(-switcher18) %>%
  mutate(
    party_first = case_when(
      startsWith(type, "Democrat") ~ "Democrat",
      ###starstWith works outside of selecting
      startsWith(type, "Independent") ~ "Independent",
      startsWith(type, "Republican") ~ "Republican",
      startsWith(type, "Other") ~ "Other party"
    ),
    party_second = case_when(
      endsWith(type, "Democrat") ~ "Democrat",
      ###starstWith works outside of selecting
      endsWith(type, "Independent") ~ "Independent",
      endsWith(type, "Republican") ~ "Republican",
      endsWith(type, "Other party") ~ "Other party"
    )
  ) %>%
  group_by(party_first) %>%
  mutate(party_sum = sum(prop),
         party_prop = prop / party_sum) %>%
  bind_rows(switch_props_byparty)


# saving data files -----

saveRDS(switch_props, "data/switch_props.rds")
saveRDS(switch_props_byparty, "data/switch_props_byparty.rds")
