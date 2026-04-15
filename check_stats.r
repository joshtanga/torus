library(tidyverse)
library(jsonlite)


# after running moduli_tori.py, we have two csv files: manifest.csv and ratios_data.csv
# moduli_tori.py creates: 
# - inside torus_grid_sweep/ directory, 
#  - manifest.csv: contains the filenames and radii of the tori
#  - ratios_data.csv: contains the actual area distortion ratios for each torus, with columns named x0, x1, etc. corresponding to the filenames in the manifest
#  - dist_plot_{n}.png histograms of distribution of distortions
#  - r_dist_plot_{n}.png line plots of distortion vs faces for each torus

# This script:
# 1. Loads the manifest and ratios data (from moduli_tori.py)
# 2. Plots histograms of the distortion ratios for each column in the ratios data, saving each plot to a file

# 1. Load the files
# manifest contains filenames/radii, ratios_data contains the actual area numbers
# we keep these csv files up a directory (they're too big for github)
manifest <- read_csv('../torus_grid_sweep/manifest.csv') |> janitor::clean_names()
ratios   <- read_csv('../torus_grid_sweep/ratios_data.csv') |>
    janitor::clean_names() |>
    mutate(n=row_number()) # Add a row number for later joining

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



