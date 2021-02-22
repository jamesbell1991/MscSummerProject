# For LGA-level maps

# Packages 
library(here)
library(sf)
library(tidyverse)
library(ggsflabel)
library(ggpubr)
library(xlsx)
library(rdhs)

# Set path and load shape data
path_to_data <- here("Data","nigeria-lgas", "new_lga_nigeria_2003.shp")

Jigawa <- st_read(path_to_data) %>% 
  filter(STATE == "Jigawa") %>%
  mutate(number = c(1:27)) %>% 
  mutate(Setting = factor(ifelse(LGA == "Garki", "Urban", "Rural"), levels = c("Urban", "Rural")))

Adamawa <- st_read(path_to_data) %>% 
  filter(STATE == "Adamawa") %>%
  mutate(number = c(28:48)) %>% 
  mutate(Setting = factor(ifelse(LGA %in% c("Mubi North", "Mubi South", "Yola North", "Yola South"), "Urban", "Rural"), levels = c("Urban", "Rural")))

Sokoto <- st_read(path_to_data) %>% 
  filter(STATE == "Sokoto")%>% 
  mutate(number = c(49:71)) %>% 
  mutate(Setting = factor(ifelse(LGA %in% c("Sokoto North", "Sokoto South"), "Urban", "Rural"), levels = c("Urban", "Rural")))


Kaduna <- st_read(path_to_data) %>% 
  filter(STATE == "Kaduna")%>% 
  mutate(number = c(72:94)) %>% 
  mutate(Setting = factor(ifelse(LGA %in% c("Kaduna North", "Kaduna South", "Zaria"), "Urban", "Rural"), levels = c("Urban", "Rural")))


#Basic maps
Jigawa_basic_map <- ggplot(Jigawa)+ 
  geom_sf((aes(fill = Setting)))+ 
  theme_void()+ 
  labs(title = "Jigawa")+ 
  geom_sf_text(aes(label= number), size = 2.5)+ 
  scale_fill_brewer(palette = "Greens")
print(Jigawa_basic_map)
ggsave(path = "output", filename = "Jigawa_map.png", plot = Jigawa_basic_map)

Adamawa_basic_map <- ggplot(Adamawa)+ 
  geom_sf((aes(fill = Setting)))+ 
  theme_void()+ 
  labs(title = "Adamawa")+ 
  geom_sf_text(aes(label= number), size = 2.5)+ 
  scale_fill_brewer(palette = "Greens")
print(Adamawa_basic_map)
ggsave(path = "output", filename = "Adamawa_map.png", plot = Adamawa_basic_map)

Sokoto_basic_map <- ggplot(Sokoto)+ 
  geom_sf((aes(fill = Setting)))+ 
  theme_void()+ 
  labs(title = "Sokoto")+ 
  geom_sf_text(aes(label= number), size = 2.5)+ 
  scale_fill_brewer(palette = "Greens")
print(Sokoto_basic_map)
ggsave(path = "output", filename = "Sokoto_map.png", plot = Sokoto_basic_map)

Kaduna_basic_map <- ggplot(Kaduna)+ 
  geom_sf((aes(fill = Setting)))+ 
  theme_void()+ 
  labs(title = "Kaduna")+ 
  geom_sf_text(aes(label= number), size = 2.5)+ 
  scale_fill_brewer(palette = "Greens")
print(Kaduna_basic_map)
ggsave(path = "output", filename = "Kaduna_map.png", plot = Kaduna_basic_map)

# NG map
path_to_data2 <- here("Data","gadm36_NGA_1_sp.rds")
NG_shape<- readRDS(path_to_data2)
NG_shape <- tidy(NG_shape, region = "NAME_1")
NG_shape <- NG_shape %>% 
  mutate(Highlight = factor(ifelse(id %in% c("Adamawa", "Sokoto", "Jigawa", "Kaduna"), "No", "Yes"), levels = c("Yes", "No")))
NG_map <- ggplot()+
  geom_polygon(data = NG_shape, aes(fill = Highlight, x = long, y = lat, group = group))+
  theme_void()+
  coord_map()+ 
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Greens")+ 
  annotate("text", x = 12, y = 9, label = "Adamawa", size = 5)+
  annotate("text", x = 5.5, y = 13, label = "Sokoto", size = 5)+ 
  annotate("text", x = 10, y = 12, label = "Jigawa", size = 5)+ 
  annotate("text", x = 8, y = 10.5, label = "Kaduna", size = 5)
ggsave(path = "output", filename = "NG_highlighted map.png", plot = NG_map, width = 210, height = 297, units = "mm")

maps <- ggarrange(Jigawa_basic_map, Adamawa_basic_map, Sokoto_basic_map, Kaduna_basic_map, common.legend = TRUE, legend = "bottom")
print(maps)
ggsave(path = "output", filename = "Basic_maps.png", plot = maps, width = 210, height = 297, units = "mm")


# Key 
key <- rbind(Jigawa, Adamawa, Sokoto, Kaduna)
key <- key %>% 
  select(c(STATE, LGA, number)) %>% 
  st_drop_geometry() %>% 
  rename(State = STATE) %>% 
  rename (Number = number) %>% 
  relocate(Number)
write.xlsx(key, file = "output/Map_key.xlsx", col.names = T, row.names = F, append = F)




