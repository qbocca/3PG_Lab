# Load necessary libraries

library(r3PG)
library(dplyr)
library(ggplot2)
library(tidyr)

PX <- read.csv("/home/quentinb/3PG/Scripts/3PG/3PGT.csv")

# Read the site, spp, and climate CSV files for the current folder
site.grid <- read.csv(file.path("/home/quentinb/PhdWork/3PG_Lec/pixel_1/site_grid.csv"))
spp.grid <- read.csv(file.path("/home/quentinb/PhdWork/3PG_Lec/pixel_1/spp_grid.csv"))
climate.grid <- read.csv(file.path("/home/quentinb/PhdWork/3PG_Lec/pixel_1/climate_grid.csv"))

# Rename columns in site.grid to match required names
colnames(site.grid) <- c("latitude", "longitude", "altitude", "soil_class", "asw_i", "asw_min", "asw_max", "from", "to")

# Ensure necessary columns are present for site.grid
required_site_columns <- c("latitude", "altitude", "soil_class", "asw_i", "asw_min", "asw_max", "from", "to")
missing_site_columns <- setdiff(required_site_columns, colnames(site.grid))
if (length(missing_site_columns) > 0) {
    stop(paste("Missing columns in site grid:", paste(missing_site_columns, collapse = ", ")))
}
site.grid2 <- site.grid[, required_site_columns]

# Debugging: Check the number of rows in site.grid2
cat("Number of rows in site.grid2:", nrow(site.grid2), "\n")

# Select a specific row from site.grid2
site <- site.grid2[1, , drop = FALSE]  # Ensure it remains a data frame

# Debugging: Print the selected site
print(site)

# Ensure the site data frame has the correct column names
colnames(site) <- required_site_columns

# Rename columns in spp.grid to match required names
colnames(spp.grid) <- c("species", "planted", "fertility", "stems_n", "biom_stem", "biom_root", "biom_foliage")

# Ensure necessary columns are present for species grid
required_spp_columns <- c("species", "planted", "fertility", "stems_n", "biom_stem", "biom_root", "biom_foliage")
missing_spp_columns <- setdiff(required_spp_columns, colnames(spp.grid))
if (length(missing_spp_columns) > 0) {
    stop(paste("Missing columns in species grid:", paste(missing_spp_columns, collapse = ", ")))
}
spp.grid <- spp.grid[, required_spp_columns]

# Define two different spacing configurations
spacing_configs <- list(
    config1 = list(stems_n = 100, biom_stem = 50, biom_root = 30, biom_foliage = 20),
    config2 = list(stems_n = 200, biom_stem = 100, biom_root = 60, biom_foliage = 40)
)

# Initialize a list to store results
results <- list()

# Loop through each spacing configuration
for (config_name in names(spacing_configs)) {
    config <- spacing_configs[[config_name]]
    
    # Modify spp.grid based on the current configuration
    spp.grid_modified <- spp.grid %>%
        mutate(
            stems_n = config$stems_n,
            biom_stem = config$biom_stem,
            biom_root = config$biom_root,
            biom_foliage = config$biom_foliage
        )
    
    # Run the 3PG model
    out_3PG <- run_3PG(
        site = site,
        species = spp.grid_modified,
        climate = climate.grid,
        thinning = NULL,
        parameters = PX,  # Ensure PX is defined
        size_dist = NULL,
        settings = list(light_model = 2, transp_model = 2, phys_model = 2, 
                        height_model = 1, correct_bias = 0, calculate_d13c = 0),
        check_input = TRUE, df_out = TRUE
    )
    
    # Store the results
    results[[config_name]] <- out_3PG
}

# Now you can compare the results
# For example, you can plot the results for each configuration
# Combine results for plotting
combined_results <- do.call(rbind, lapply(names(results), function(name) {
    df <- results[[name]]
    df$spacing <- name  # Add a column for spacing configuration
    return(df)}
))