# Info ----
## Author: Sean Bock
## Project: Partisan ID Switching
## Date: 11/1/21
## Note: This script loads in data, generates plots, and saves them in "figures" folder. This should be run after "cleaning data" script.



# Loading packages ----
## these lines install necessary packages if not already installed
if (!require(pacman))
  install.packages("pacman")
if (!require(devtools))
  install.packages("devtools")
if (!require(drlib))
  devtools::install_github("dgrtwo/drlib")
if (!require(drlib))
  devtools::install_github("nicolash2/ggbrace")

pacman::p_load(tidyverse, ggpubr, glue, ggbrace, drlib, ggalluvial, ggthemes)

# Load data ------
rm(list = ls()) ##clearing environment

switch_props <- read_rds("data/switch_props.rds")
switch_props_byparty <- read_rds("data/switch_props_byparty.rds")

# Plots -----

ordering <-
  switch_props_byparty %>% ## have to create ordering variable separately because of facet issues
  filter(!is.na(party_first),
         year == 2016) %>%
  arrange(party_first, -n) %>%
  mutate(order = row_number()) %>%
  select(type, order)

# Bar plot for appendix -----

(
  merge(switch_props_byparty, ordering) %>%
    mutate(
      party = as.factor(party_first),
      party = fct_relevel(party, "Democrat", "Independent", "Republican", "Other party")
    ) %>%
    ggplot(aes(
      x = reorder(type, -order),
      y = party_prop,
      fill = party
    )) +
    ggthemes::theme_clean(base_size = 9) +
    geom_col(position = position_dodge(width = 10)) +
    scale_y_continuous(limits = c(0.00, 1.2), breaks = seq(0, 1, .25)) +
    coord_flip() +
    labs(y = "% among initial party I.D.",
         x = "",
         caption = "Note: Labels indicate % of each type of switching among party ID in first wave.") +
    geom_text(aes(
      y = party_prop + .228,
      label = glue::glue("{scales::percent(party_prop,accuracy=1)} (N={round(n)})")
    ), size = 2.3) +
    facet_grid(party ~ year, scales = 'free_y') +
    scale_fill_manual(values = c(
      "#006a8d", "#a8a6a7", "#b02739", "dark green"
    )) +
    theme(
      legend.position = "",
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(color = "white")
    )
) %>%
  ggsave("figures/switching_bar.pdf", .)



## alluvial plot (main figure in text) -----

# identifying cut points for brackets
max_2016 <- switch_props_byparty %>%
  filter(type != "Missing in at least one wave",
         str_detect(type, "Other party", negate = TRUE)) %>%
  filter(year == 2016) %>%
  ungroup() %>%
  mutate(sum = sum(n)) %>%
  select(sum) %>%
  distinct() %>% pull()

values <- switch_props_byparty %>%
  
  filter(type != "Missing in at least one wave",
         str_detect(type, "Other party", negate = TRUE)) %>% ##removing other party for clarity
  filter(year == 2016) %>%
  group_by(type) %>%
  mutate(
    sum = sum(n),
    order = case_when(
      party_first == "Democrat" ~ 1,
      party_first == "Republican" ~ 3,
      party_first == "Independent" ~ 2
    ),
    order2 = case_when(
      party_second == "Democrat" ~ 1,
      party_second == "Republican" ~ 3,
      party_second == "Independent" ~ 2
    )
  ) %>%
  arrange(order2, order) %>%
  select(type, sum, party_first, order) %>%
  group_by(type) %>%
  ungroup() %>%
  select(type, sum) %>%
  mutate(
    cum_sum = cumsum(sum),
    ##Identifying cutpoints for flow categories
    max = if_else(
      type == "Democrats stayed Democrat",
      max_2016,
      max_2016 - lag(cum_sum)
    ),
    min = lead(max, default = 0)
  ) %>%
  select(type, min, max, sum) %>%
  rowwise() %>%
  mutate(mid = mean(c(min, max))) %>%
  mutate(type = if_else(
    str_detect(type, "to"),
    str_replace(type, "to", "\U2192"),
    str_replace(type, "stayed", "\U2192")
  )) ##adding unicode right arrow to label


## creating ordered version of data
switch_props_byparty2 <- switch_props_byparty %>%
  filter(
    year == "2016",
    type != "Missing in at least one wave",
    str_detect(type, "Other party", negate = TRUE)
  ) %>%
  mutate(
    year = recode(year, "2016" = "2016 %->% 2020", "2018" = "2018 %->% 2020"),
    order = case_when(
      party_first == "Democrat" ~ 1,
      party_first == "Republican" ~ 3,
      party_first == "Independent" ~ 2
    ),
    order2 = case_when(
      party_second == "Democrat" ~ 1,
      party_second == "Republican" ~ 3,
      party_second == "Independent" ~ 2
    )
  ) %>%
  arrange(order2, order) %>%
  ungroup() %>%
  mutate(order = row_number())


(
  switch_props_byparty2 %>%
    pivot_longer(cols = c(party_first, party_second)) %>%
    ggplot(aes(
      x = name,
      y = n,
      stratum = value,
      fill = value,
      alluvium = type
    )) +
    geom_alluvium() +
    geom_stratum(size = 0, alpha = .8) +
    ggthemes::theme_map(base_size = 12) +
    scale_fill_manual(
      values = c("#006a8d", "#ABB0B8", "#b02739"),
      name = ""
    ) +
    coord_cartesian(# This focuses the x-axis on the range of interest
      clip = 'off') +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      plot.margin = unit(c(1, 14, 1, 1), "lines")
    ) +
    annotate(
      "text",
      label = "bold(2016)",
      x = 1,
      y = 900,
      size = 7,
      parse = TRUE,
      color = "grey"
    ) +
    annotate(
      "segment",
      x = 1.3,
      xend = 1.7,
      y = 900,
      yend = 900,
      colour = "grey",
      arrow = arrow(),
      size = .3,
      alpha = .7
    ) +
    annotate(
      "text",
      label = "bold(2020)",
      x = 2,
      y = 900,
      size = 7,
      parse = TRUE,
      color = "grey"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[1], values$max[1])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#669bb1"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[2], values$max[2])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#ABB0B8"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[3], values$max[3])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#c9727e"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[4], values$max[4])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#669bb1"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[5], values$max[5])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#ABB0B8"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[6], values$max[6])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#c9727e"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[7], values$max[7])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#669bb1"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[8], values$max[8])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#ABB0B8"
    ) +
    ggbrace::geom_brace(
      aes(
        x = c(2.2, 2.25),
        y = c(values$min[9], values$max[9])
      ),
      inherit.data = F,
      rotate = 90,
      color = "#c9727e"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[1])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[1])} of {values$type[1]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#669bb1",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[2])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[2])} of {values$type[2]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#669bb1",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[3])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[3])} of {values$type[3]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#669bb1",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[4])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[4])} of {values$type[4]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#ABB0B8",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[5])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[5])} of {values$type[5]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#ABB0B8",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[6])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[6])} of {values$type[6]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#ABB0B8",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[7])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[7])} of {values$type[7]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#c9727e",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[8])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[8])} of {values$type[8]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#c9727e",
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.3, y = (values$mid[9])),
      label = glue(
        '{scales::percent(switch_props_byparty2$party_prop[9])} of {values$type[9]}'
      ),
      parse = FALSE,
      size = 4.5,
      color = "#c9727e",
      hjust = 0,
      fontface = "bold"
    )
) %>%
  ggsave(
    "figures/switching_flow.png",
    .,
    height = 8.5,
    width = 11,
    dpi = 600
  ) ##saving as pdf doesn't recognize unicode characters
