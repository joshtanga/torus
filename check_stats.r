library(tidyverse)
library(jsonlite)

# 1. Load the files
# manifest contains filenames/radii, ratios_data contains the actual area numbers
manifest <- read_csv('assets/torus_grid_sweep/manifest.csv') |> janitor::clean_names()
ratios   <- read_csv('assets/torus_grid_sweep/ratios_data.csv')

# 2. Calculate stats only on the numeric ratio columns
dist_stats_tidy <- ratios |> 
  summarise(across(
    where(is.numeric), 
    list(
      mean = \(x) mean(x, na.rm = TRUE),
      max  = \(x) max(x, na.rm = TRUE),
      min  = \(x) min(x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) |> 
  pivot_longer(
    everything(), 
    names_to = c("column_index", "stat"), 
    names_pattern = "v(.*)_(.*)" # Matches "v0_mean", "v1_mean", etc.
  ) |> 
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(column_index = as.numeric(column_index)) |>
  arrange(column_index)

# 3. Bind the radii and filenames from the manifest to the calculated stats
# This ensures each row has: file, r_idx, r_major, r_minor, mean, max, min
combined_payload <- bind_cols(manifest, dist_stats_tidy |> select(-column_index))

# explort combined payload to csv for debugging
# write_csv(dist_stats_tidy, "assets/torus_grid_sweep/combined_payload.csv")


