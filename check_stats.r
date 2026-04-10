library(tidyverse)
library(jsonlite)

# 1. Load the files
# manifest contains filenames/radii, ratios_data contains the actual area numbers
manifest <- read_csv('assets/torus_grid_sweep/manifest.csv') |> janitor::clean_names()
ratios   <- read_csv('assets/torus_grid_sweep/ratios_data.csv') |>
    janitor::clean_names() |>
    mutate(n=row_number()) # Add a row number for later joining

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
    names_pattern = "x(.*)_(.*)" # Matches "v0_mean", "v1_mean", etc.
  ) |> 
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(column_index = as.numeric(column_index)) |>
  arrange(column_index)

# 3. Bind the radii and filenames from the manifest to the calculated stats
# This ensures each row has: file, r_idx, r_major, r_minor, mean, max, min
# combined_payload <- bind_cols(manifest, dist_stats_tidy |> select(-column_index))

# explort combined payload to csv for debugging
# write_csv(dist_stats_tidy, "assets/torus_grid_sweep/combined_payload.csv")

# for every column in ratios, plot the distortion ratio vs faces the values, and save it to a file 
# all but the last column
for (col_name in head(names(ratios), -1))
     # just do two for testing
#for (col_name in names(ratios)[1:2])
{
  p <- ggplot(ratios, aes(x=n,y = .data[[col_name]])) +
    #geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
    #labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
    geom_line() +
    labs(title = paste("Area Distortion of Torus Number ", str_remove(col_name,'x')), x = str_c('Faces of Torus Number ', str_remove(col_name,'x')), y = "Distortion Ratio") +
    theme_minimal()
  
  ggsave(filename = paste0("assets/torus_grid_sweep/r_dist_plot_", col_name, ".png"), plot = p)
}



