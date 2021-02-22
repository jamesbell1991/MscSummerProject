# This script plots COVID cases over time in Nigeria 

# Required packages 
library(dplyr)
library(tidyr)
library(ggplot2)
library(covid19.analytics)
library(lubridate)
library(magrittr)

# Retrieve data using covid19.analytics package (Hopkins data)
coronavirus<- covid19.data("ts-confirmed")


# Filter to Nigeria 
NG <- coronavirus %>% 
  filter(Country.Region == "Nigeria")

# Reshape data 
NG <- pivot_longer(
  NG, 
  "2020-01-22":"2021-02-19", 
  names_to = "Date"
)

# Convert dates
NG$Date<-as.Date(NG$Date)

# Add incidence 
NG %<>% 
  mutate(
    incidence = value - lag(value)
  )

# Add milestones  
dates = data.frame(Event = c("Lockdown in Lagos, Ogun & Abuja", "Lockdown extended to Kano", "Lockdown replaced with curfew measures, \n movement and hygiene guidelines", "School closures", "Kaduna recloses schools"),
                   StartDate = c("2020-03-30", "2020-04-27", "2020-05-17", "2020-03-01", "2020-12-16"),
                   EndDate = c("2020-04-27", "2020-05-17", "2021-01-01", "2020-10-01", "2020-12-30"),
                   y = c(1300,1300,1300,1400,1400),
                   yend = c(1300,1300,1300,1400,1400))
dates$StartDate<-as.Date(dates$StartDate)
dates$EndDate<-as.Date(dates$EndDate)
dates$Event <- as.factor(dates$Event)
dates$Event <- dates %$% factor(Event, levels(Event)[c(3,2,4,5,1)])


# Plot (Reported COVID Cases in Nigeria and Timeline of Control Measures" Data source: Johns Hopkins)
plot <- ggplot(
  data = NG, 
  aes(x=Date, 
      y=incidence, 
      group=1))+
  geom_line(colour = "#6E7677", size = 1)+
  theme_bw()+
  geom_segment(data = dates, aes(x=StartDate, xend = EndDate, y = y, yend = yend, colour = Event), size = 4)+
  # geom_text(data = dates, aes(x=StartDate, y = y, label = Event), size = 3, nudge_x = c(-5, -40, -5, -25, -5), nudge_y = c(-20, 0, -20, 0, -20))+
  xlab("Date")+
  ylab("Number of cases")+ 
  xlim(as.Date(c('1/1/2020', '1/1/2021'), format="%d/%m/%Y"))+ 
  ylim(0,1500)+ 
  theme(legend.position = 'right', 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+ 
  scale_colour_brewer(palette = "Greens")
print(plot)
ggsave(path = "output", filename = "COVID_plot.png", plot = plot, width = 297, height = 210, units = "mm")
  
  
  
  

  