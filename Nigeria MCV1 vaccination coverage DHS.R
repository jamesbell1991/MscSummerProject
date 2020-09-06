# This script creates maps to show DHS estimates of MCV1 coverage in Nigeria 

# Load packages 
library(rdhs)
library(ggplot2)
library(dplyr)
library(sp)
library(here)
library(broom)

# Use rdhs package to access data

# Query country codes
dhs_countries(returnFields = c("CountryName", "DHS_CountryCode"))
# Nigeria country code: NG 

# Look up relevant tags
dhs_tags()
# Immunization: 32

# Identify measles indicators 
Imm_indicators <- dhs_indicators(tagIds = 32)
# 	Percentage of children 12-23 months who had received Measles vaccination: CH_VACC_C_MSL

# Request data
MCV1 <- dhs_data(indicatorIds = "CH_VACC_C_MSL", 
                    countryIds = "NG", 
                    surveyYearStart = 2018, 
                    breakdown = "subnational")

# Data tidying 
MCV1 <- MCV1[,c(21, 1:20, 22:27)]
MCV1 <- MCV1[-c(1,9,16,24,30,37), ]
MCV1 <- MCV1[order(MCV1$CharacteristicLabel),]
MCV1$CharacteristicLabel <- sub("..", "", MCV1$CharacteristicLabel)
MCV1<- rename(MCV1, c("id" = "CharacteristicLabel"))
MCV1["6", "id"] <- "Nassarawa"
MCV1["2", "id"] <- "Federal Capital Territory"

# Set path and load shape data
path_to_data <- here("Data","gadm36_NGA_1_sp.rds")
NG_shape<- readRDS(path_to_data)
plot(NG_shape)
NG_shape <- tidy(NG_shape, region = "NAME_1")

# Merge data sets 
map_data <- inner_join(NG_shape, MCV1[ , c("Value", "id")], by.x = "id", by.y = "id")




#Plot
ggplot()+
  geom_polygon(data = map_data, aes(fill = Value, x = long, y = lat, group = group))+
  theme_void()+
  coord_map()+ 
  scale_fill_gradient2(low = "#852D05", high = "#248505", mid = "white")+
  labs(fill = "%")+ 
  labs(title = "Percentage of children 12-23 months who had received Measles vaccination",
       subtitle = "By state",
       caption = "Data source: Nigeria DHS 2018")
             
  
