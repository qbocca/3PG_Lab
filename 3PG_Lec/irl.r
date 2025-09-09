library(r3PG)
library(dplyr)
library(ggplot2)
library(tidyr)

#data prep

site.grid <- read.csv(file.path("ValdPix/site_grid.csv"))
spp.grid <- read.csv(file.path("ValdPix/spp_grid.csv"))
biloxi.grid <- read.csv(file.path("ValdPix/climate_grid2.csv"))

PX <- read.csv(file.path("3PGT.csv")) 


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

  
  # Run 3PG model
  out_3PG <- run_3PG(
    site = site.grid2, 
    species = spp.grid, 
    climate = biloxi.grid, 
    thinning = NULL,
    parameters = PX, 
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1, 
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = TRUE
  )
  



i_var <- c('stems_n',  'dbh', 'height', 'biom_stem', 'biom_root', 'biom_foliage')
i_lab <- c('Stem density', 'DBH', 'Height', 'Stem biomass', 'Root biomass', 'Foliage biomass')

out_3PG %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = species), size = 0.5)+
  facet_wrap( ~ variable, scales = 'free_y', ncol = 3, 
    labeller = labeller(variable = setNames(i_lab, i_var) )) +
  scale_color_brewer('', palette = 'Dark2') +
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("Calendar date") + ylab('Value')


library(dplyr)
library(lubridate)  # For date manipulation

dst <- out_3PG %>%
  filter(variable == "biom_stem") %>%
  mutate(year = year(date)) %>%  # Extract the year from the date
  group_by(year) %>%
  summarize(mean_value = mean(value, na.rm = TRUE)) %>%
  mutate(cumulative_mean = mean_value / row_number())

head(dst)

plot(dst$cumulative_mean)

lines(dst$cumulative_mean)


