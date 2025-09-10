library(r3PG)
library(dplyr)
library(ggplot2)
library(tidyr)

#data prep

site.grid <- read.csv(file.path("3PG_Lec/pixel_1/site_grid.csv"))
spp.grid <- read.csv(file.path("3PG_Lec/pixel_1/spp_grid.csv"))
biloxi.grid <- read.csv(file.path("3PG_Lec/biloxi.csv"))
athens.grid <- read.csv(file.path("3PG_Lec/athens.csv"))
mic.grid <- read.csv(file.path("3PG_Lec/mic.csv"))


PX <- get_parameters(mode = "parameters", sp_names = "Pinus taeda")

qformater <- function(grid,grid_type){

  if(grid_type == "site"){

  colnames(grid) <- c("latitude", "longitude", "altitude", "soil_class", "asw_i", "asw_min", "asw_max", "from", "to")
  required_site_columns <- c("latitude", "altitude", "soil_class", "asw_i", "asw_min", "asw_max", "from", "to") # nolint
  missing_site_columns <- setdiff(required_site_columns, colnames(grid))
  if (length(missing_site_columns) > 0) {
      stop(paste("Missing columns in site grid:", paste(missing_site_columns, collapse = ", ")))
  }
  grid2 <- grid[, required_site_columns] } else if(grid_type == "spp"){
  colnames(grid) <- c("species", "planted", "fertility", "stems_n", "biom_stem", "biom_root", "biom_foliage")

  required_spp_columns <- c("species", "planted", "fertility", "stems_n", "biom_stem", "biom_root", "biom_foliage")
  missing_spp_columns <- setdiff(required_spp_columns, colnames(grid))
  if (length(missing_spp_columns) > 0) {
      stop(paste("Missing columns in species grid:", paste(missing_spp_columns, collapse = ", ")))
  }
  grid2 <- grid[, required_spp_columns]
    }

  return(grid2)  
}

site.grid2 <- qformater(site.grid,"site")

spp.grid <- qformater(spp.grid, "spp")

spp.grid <- spp.grid %>%
    mutate(
        stems_n = 10000,
        biom_stem = 0.1,
        biom_root = 0.1,
        biom_foliage = 0.1
    )

# Combine climate data frames into a named list
climate_data <- list(athens.grid = athens.grid, mic.grid = mic.grid, biloxi.grid = biloxi.grid)

# Initialize a list to store outputs
pg_runs <- list()

# Loop through each climate data frame using names
for (name in names(climate_data)) {
  climate_value <- climate_data[[name]]
  
  # Run 3PG model
  out_3PG <- run_3PG(
    site = site.grid2, 
    species = spp.grid, 
    climate = climate_value, 
    thinning = NULL,
    parameters = PX, 
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1, 
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = TRUE
  )
  
  pg_runs[[name]] <- out_3PG
}

# Combine all runs into a single dataframe
combined_output <- bind_rows(pg_runs, .id = "run_id")


head(combined_output)

# Define variables for plotting
i_var <- c('stems_n', 'dbh', 'height', 'biom_stem', 'biom_root', 'biom_foliage')
i_lab <- c('Stem density', 'DBH', 'Height', 'Stem biomass', 'Root biomass', 'Foliage biomass')

# Filter and plot the combined output
combined_output %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = run_id), linewidth = 0.9) +  # Use color to differentiate runs
  facet_wrap(~ variable, scales = 'free_y', ncol = 3, 
             labeller = labeller(variable = setNames(i_lab, i_var))) +
  scale_color_brewer('', palette = 'Dark2') +  # Set color palette
  theme_classic() +  # Use a classic theme for the plot
  theme(legend.position = "bottom") +  # Position the legend at the bottom
  xlab("Calendar date") +  # Label for the x-axis
  ylab('Value')  # Label for the y-axis



