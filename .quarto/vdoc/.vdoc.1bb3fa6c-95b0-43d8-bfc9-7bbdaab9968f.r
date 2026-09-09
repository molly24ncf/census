#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(tidycensus)
library(sf)
#
#
#
#| message: false
income_tx <- get_acs(
  geography = "county",
  state = "TX",
  variables = "B19013_001",
  year = 2020,
  geometry = TRUE,
  output = "wide"
)
#
#
#
income_tx |>
  ggplot(aes(fill = B19013_001E)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    labels = scales::label_dollar(),
    name = "Median household income"
  ) +
  labs(
    title = "Median household income by county in Texas, 2020",
    caption = "Data source: U.S. Census Bureau, American Community Survey (ACS) 5-year estimates, 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )
#
#
#
#| message: false
#| cache: true
edu_state <- get_acs(
  geography = "state",
  variables = c(
    "B15003_001",
    "B15003_022",
    "B15003_023",
    "B15003_024",
    "B15003_025"
  ),
  year = 2020,
  summary_var = "B15003_001"
)
#
#
#
edu_state |>
  group_by(NAME) |>
  summarise(
    total = first(summary_est),
    bachelors_plus = sum(estimate[variable %in% c("B15003_022", "B15003_023", "B15003_024", "B15003_025")]),
    pct_bachelors_plus = bachelors_plus / total * 100,
    .groups = "drop"
  ) |>
  arrange(desc(pct_bachelors_plus)) |>
  mutate(NAME = forcats::fct_inorder(NAME)) |>
  ggplot(aes(x = pct_bachelors_plus, y = NAME)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, max(edu_state |>
      group_by(NAME) |>
      summarise(
        total = first(summary_est),
        bachelors_plus = sum(estimate[variable %in% c("B15003_022", "B15003_023", "B15003_024", "B15003_025")]),
        pct_bachelors_plus = bachelors_plus / total * 100,
        .groups = "drop"
      ) |>
      pull(pct_bachelors_plus)) + 5)
  ) +
  labs(
    x = "Adults with at least a bachelor's degree",
    y = "State",
    title = "Bachelor's-or-higher percentage by state, 2020",
    caption = "Data source: U.S. Census Bureau, American Community Survey (ACS) 5-year estimates, 2020"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )
#
#
#
#| message: false
age_ca <- get_acs(
  geography = "county",
  state = "CA",
  variables = c(
    median_age = "B01002_001",
    population = "B01003_001"
  ),
  year = 2020,
  geometry = FALSE
)
#
#
#
age_ca |>
  select(-moe) |>
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |>
  ggplot(aes(x = median_age)) +
  geom_histogram(binwidth = 2, color = "white", fill = "steelblue") +
  labs(
    x = "Median age",
    y = "Number of counties",
    title = "Distribution of median age across California counties, 2020"
  ) +
  theme_minimal()
#
#
#
#
#
