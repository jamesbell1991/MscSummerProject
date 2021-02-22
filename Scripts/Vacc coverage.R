# Packages 
library(here)
library(readxl)
library(readr)
library(tidyverse)
library(janitor)

# Load population estimates 
path_to_01 <- here("Data", "NGA_popdata_0_1")
pop_01 <- readRDS(path_to_01) %>% 
  select("2019-01-01":"2020-11-01") %>% 
  arrange(STATE, LGA) %>% 
  rename(State = STATE)

path_to_14 <- here("Data", "NGA_popdata_1_4")
pop_14 <- readRDS(path_to_14) %>% 
  select("2019-01-01":"2020-11-01")%>% 
  arrange(STATE, LGA)%>% 
  rename(State = STATE)

# Load vaccine coverage 
MCV1 <- read_excel("Data/coverage_data.xlsx", sheet = "MCV1")
MCV1[MCV1 == "Girei"] <- "Girie"
MCV1[MCV1 == "Toungo"] <- "Teungo"
MCV1[MCV1 == "Malam Madori"] <- "Malam Maduri"
MCV1[MCV1 == "Sule-Tankarkar"] <- "Sule Tankarkar"
MCV1[MCV1 == "Birniwa"] <- "Biriniwa"
MCV1 <- MCV1 %>% 
  pivot_longer(!c("State", "LGA")) %>% 
  mutate(name = as.numeric(name)) %>% 
  mutate(name = excel_numeric_to_date(name)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  filter(LGA != "Total")%>% 
  arrange(State, LGA)


MCV2 <- read_excel("Data/coverage_data.xlsx", sheet = "MCV2")
MCV2[MCV2 == "Girei"] <- "Girie"
MCV2[MCV2 == "Toungo"] <- "Teungo"
MCV2[MCV2 == "Malam Madori"] <- "Malam Maduri"
MCV2[MCV2 == "Sule-Tankarkar"] <- "Sule Tankarkar"
MCV2[MCV2 == "Birniwa"] <- "Biriniwa"
MCV2 <- MCV2 %>% 
  pivot_longer(!c("State", "LGA")) %>% 
  mutate(name = as.numeric(name)) %>% 
  mutate(name = excel_numeric_to_date(name)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  filter(LGA != "Total")%>% 
  arrange(State, LGA)


all.equal(pop_14$State, MCV2$State)
all.equal(pop_01$State, MCV1$State)
all.equal(pop_14$LGA, MCV2$LGA)
all.equal(pop_01$LGA, MCV1$LGA)


# Calculations
MCV1_calc <- MCV1 %>% 
  pivot_longer(!c("State", "LGA")) %>% 
  rename(value_num = value)
pop_01_calc <- pop_01 %>%
  pivot_longer(!c("State", "LGA")) %>% 
  rename(value_denom = value)
MCV1_cov <- left_join(MCV1_calc, pop_01_calc, by = c("State", "LGA", "name")) %>% 
  mutate(cov = (value_num / value_denom)*100) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  select(!c("value_num", "value_denom")) %>% 
  pivot_wider(names_from = name, values_from = cov)
saveRDS(MCV1_cov, file = "Data/MCV1_coverage")

MCV2_calc <- MCV2 %>% 
  pivot_longer(!c("State", "LGA")) %>% 
  rename(value_num = value)
pop_14_calc <- pop_14 %>%
  pivot_longer(!c("State", "LGA")) %>% 
  rename(value_denom = value)
MCV2_cov <- left_join(MCV2_calc, pop_14_calc, by = c("State", "LGA", "name")) %>% 
  mutate(cov = (value_num / value_denom)*100) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>%
  select(!c("value_num", "value_denom")) %>% 
  pivot_wider(names_from = name, values_from = cov)
saveRDS(MCV2_cov, file = "Data/MCV2_coverage")

# Graphs 
MCV1_cov_long<- MCV1_cov %>%
  pivot_longer(!c("State", "LGA"))
MCV1_cov_long$name<- as.Date(MCV1_cov_long$name, "%Y-%m-%d")
MCV2_cov_long<- MCV2_cov %>%
  pivot_longer(!c("State", "LGA"))
MCV2_cov_long$name<- as.Date(MCV2_cov_long$name, "%Y-%m-%d")

plot_MCV1 <- function(x) {
  plot <- MCV1_cov_long %>% 
    filter(State == x) %>% 
    ggplot(aes(x = name, y = value, group = LGA))+ 
    geom_line()+ 
    scale_x_date(date_labels = "%b \n %y", date_breaks = "6 months")+
    ylim(0,100)+ 
    labs(y = "Percentage MCV1 Coverage", 
         x = "Date", 
         title = paste0(x))+
    theme_bw()+ 
    theme(axis.text.x = element_text(size = 6), 
          axis.text.y = element_text(size = 6))+
    facet_wrap(~LGA)
  if (dir.exists("output")) {}
  else {dir.create("output")}
  ggsave(filename = paste0("output/", 
                           x, 
                           "MCV1_plot.png"),
         width = 210, height = 297, units = "mm")
}

map(states, plot_MCV1)


plot_MCV2 <- function(x) {
  plot <- MCV2_cov_long %>% 
    filter(State == x) %>% 
    ggplot(aes(x = name, y = value, group = LGA))+ 
    geom_line()+ 
    scale_x_date(date_labels = "%b \n %y", date_breaks = "6 months")+
    ylim(0,100)+ 
    labs(y = "Percentage MCV2 Coverage", 
         x = "Date", 
         title = paste0(x))+
    theme_bw()+ 
    theme(axis.text.x = element_text(size = 6), 
          axis.text.y = element_text(size = 6))+
    facet_wrap(~LGA)
  if (dir.exists("output")) {}
  else {dir.create("output")}
  ggsave(filename = paste0("output/", 
                           x, 
                           "MCV2_plot.png"), 
         width = 210, height = 297, units = "mm")
}

states <- unique(MCV1_cov_long$State)
map(states, plot_MCV2)

