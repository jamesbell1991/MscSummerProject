# This script creates maps of various MCV1 coverage estimates in Nigeria 

# Load packages 
library(rdhs)
library(ggplot2)
library(dplyr)
library(sp)
library(here)
library(broom)
library(ggpubr)

# Use rdhs package to access DHS data

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
DHS18 <- dhs_data(indicatorIds = "CH_VACC_C_MSL", 
                    countryIds = "NG", 
                    surveyYearStart = 2018, 
                    breakdown = "subnational")

# Data tidying 
DHS18 <- DHS18[,c(21, 1:20, 22:27)]
DHS18 <- DHS18[-c(1,9,16,24,30,37), ]
DHS18 <- DHS18[order(DHS18$CharacteristicLabel),]
DHS18$CharacteristicLabel <- sub("..", "", DHS18$CharacteristicLabel)
DHS18<- rename(DHS18, c("id" = "CharacteristicLabel"))
DHS18<- rename(DHS18, c("DHS.2018" = "Value"))
DHS18["6", "id"] <- "Nassarawa"
DHS18["2", "id"] <- "Federal Capital Territory"

# Download data from https://msdat.fmohconnect.gov.ng/central_analytics?indicator=Measles%20Immunization%20Coverage&location=National
# Indicators for 2018 on NNHS, PCCS, NHMIS
# Load data 
path_to_data2 <- here ("Data", "MSDAT.csv")
MSDAT <- read.csv(path_to_data2)

# Tidying 
MSDAT<- rename(MSDAT, c("id" = "ï..Category"))
MSDAT["5", "id"] <- "Nassarawa"
MSDAT["4", "id"] <- "Federal Capital Territory"

# Set path and load shape data
path_to_data <- here("Data","gadm36_NGA_1_sp.rds")
NG_shape<- readRDS(path_to_data)
plot(NG_shape)
NG_shape <- tidy(NG_shape, region = "NAME_1")

# Merge data sets 
map_data <- inner_join(NG_shape, DHS18[ , c("DHS.2018", "id")], by.x = "id", by.y = "id")
map_data <- inner_join(map_data, MSDAT[ , c("id", "NNHS.2018", "PCCS.2018", "NHMIS.2018")], by.x = "id", by.y = "id")

#Plot- DHS
DHS <- ggplot()+
  geom_polygon(data = map_data, aes(fill = DHS.2018, x = long, y = lat, group = group))+
  theme_void()+
  coord_map()+ 
  scale_fill_gradient2(low = "#852D05", high = "#248505", mid = "white", limits = c(0,130))+
  labs(title = "DHS 2018",
       caption = "Indicator: % of children aged 12-23 who received 1 meales dose",
       fill = "%")
             
#Plot- NNHS
NNHS <- ggplot()+
  geom_polygon(data = map_data, aes(fill = NNHS.2018, x = long, y = lat, group = group))+
  theme_void()+
  coord_map()+ 
  scale_fill_gradient2(low = "#852D05", high = "#248505", mid = "white", limits = c(0,130))+
  labs(fill = "%")+ 
  labs(title = "NNHS 2018",
       caption = "Indicator: Children (12-23) who received measles vaccine")

#Plot- NHMIS
NHMIS <- ggplot()+
  geom_polygon(data = map_data, aes(fill = NHMIS.2018, x = long, y = lat, group = group))+
  theme_void()+
  coord_map()+ 
  scale_fill_gradient2(low = "#852D05", high = "#248505", mid = "white", limits = c(0,130))+
  labs(fill = "%")+ 
  labs(title = "NHMIS 2018",
       caption = "Indicator: doses given/ eligible population estimate")

#Arrange plots ("Figure 2: MCV1 coverage by state according to DHS 2018, NNHS 2018 and NHMIS 2018 estimates")
ggarrange(DHS, NHMIS, NNHS, common.legend = TRUE, legend = "right")
maps <- ggarrange(DHS, NHMIS, NNHS, common.legend = TRUE, legend = "right")
ggsave(path = "output", filename = "MCV1_map.png", plot = maps, width = 210, height = 297, units = "mm")
