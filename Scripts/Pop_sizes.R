# Combines NG LGA shapefiles with Worldpop population data 

# Packages 
library(sf)
library(here)
library(wopr)
library(tidyverse)

# Shape file inputs 
path_to_data <- here("Data","nigeria-lgas", "new_lga_nigeria_2003.shp")
NGA_LGA <- st_read(path_to_data) %>% 
  filter(STATE %in% c("Jigawa", "Sokoto", "Kaduna", "Adamawa"))

# Get population estimates 
getCatalogue(spatial_query = T) # NGA v1.2
NGA_0_1 <- woprize(features = NGA_LGA, 
                   country = 'NGA', 
                   version = '1.2', 
                   agesex_select = c('f0', 'm0'),
                   confidence = 0.95, 
                   tails = 2, 
                   abovethresh = 2e4, 
                   belowthresh = 1e4)
saveRDS(NGA_0_1, file = "Data/Shapefile_startingpop01")


NGA_1_4 <- woprize(features = NGA_LGA, 
                       country = 'NGA', 
                       version = '1.2', 
                       agesex_select = c('f1', 'm1'),
                       confidence = 0.95, 
                       tails = 2, 
                       abovethresh = 2e4, 
                       belowthresh = 1e4)
saveRDS(NGA_0_1, file = "Data/Shapefile_startingpop14")

increase <- 1.00022

# For 0-1 years divide by 6
NGA_pop_0_1 <- NGA_0_1 %>%
  mutate(start_pop = mean/6) %>% 
  subset(select = c("STATE", "LGA", "start_pop")) %>% 
  mutate(mon = as.Date("2018-01-01")) %>%
  st_drop_geometry() %>% 
  group_by(STATE, LGA) %>%
  complete(mon = seq.Date(from = as.Date("2018-01-01"), to = as.Date("2021-12-01"), by = "month")) %>%
  mutate(start_pop = if_else(is.na(start_pop), increase, start_pop),
         start_pop = cumprod(start_pop)) %>% 
    pivot_wider(names_from = mon, values_from = start_pop)

saveRDS(NGA_pop_0_1, file = "NGA_popdata_0_1")

# For 1-4 years divide by 24
NGA_pop_1_4 <- NGA_1_4 %>%
  mutate(start_pop = mean/24) %>% 
  subset(select = c("STATE", "LGA", "start_pop")) %>% 
  mutate(mon = as.Date("2018-01-01")) %>%
  st_drop_geometry() %>% 
  group_by(STATE, LGA) %>%
  complete(mon = seq.Date(from = as.Date("2018-01-01"), to = as.Date("2021-12-01"), by = "month")) %>%
  mutate(start_pop = if_else(is.na(start_pop), increase, start_pop),
         start_pop = cumprod(start_pop)) %>% 
  pivot_wider(names_from = mon, values_from = start_pop)

saveRDS(NGA_pop_1_4, file = "NGA_popdata_1_4")

