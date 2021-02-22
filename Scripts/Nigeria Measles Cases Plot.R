# This script takes WHO surveillance data on measles and creates an incidence plot for Nigeria 2011-2020

# Load packages 
library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Download data from: https://www.who.int/immunization/monitoring_surveillance/burden/vpd/surveillance_type/active/measles_monthlydata/en/

# Remove the first tab of the file 

# Set path and load data
path_to_data <- here("Data","surveillancedata.xls")
surdata<- read_excel(path_to_data)
surdata <- as.data.frame(surdata)

# Reshape data 
surdata <- pivot_longer(
  surdata, 
  January:December, 
  names_to = "Month"
)

# Create combined year and month variable 
surdata$Month <- as.factor(surdata$Month)
surdata$Month <- recode_factor(surdata$Month, 
                               "January"="1", 
                               "February"="2",
                               "March"="3", 
                               "April"="4", 
                               "May"="5", 
                               "June"="6", 
                               "July"="7", 
                               "August"="8", 
                               "September"="9", 
                               "October"="10", 
                               "November"="11", 
                               "December"="12")

surdata$label <- paste(surdata$Month, surdata$Year)

# Change column types 
surdata$value <- as.numeric(surdata$value)
surdata$label <- parse_date_time(surdata$label, "my")

# Subset to Nigeria
surdata_NG<- subset(surdata, Country == "Nigeria")

# Remove NA rows 
surdata_NG <- surdata_NG[-c(117:120), ]

# Plot (Title: Reported Measles Cases in Nigeria, 2011-2020. Data source: WHO surveillance)
plot <- ggplot(
  data = surdata_NG, 
  aes(x=label, 
      y=value))+
    geom_line(colour = "#6E7677", size = 1)+
  theme_bw()+
  xlab("Year")+
  ylab("Number of cases")
ggsave(path = "output", filename = "Measles_cases.png", plot = plot, width = 297, height = 210, units = "mm")


