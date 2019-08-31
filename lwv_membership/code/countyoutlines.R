library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sp)
library(maptools)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# data for environment -----------------------------------------------------------
membership_work <- read_csv("data/membership_work.csv") %>% 
  select(-X12) %>% 
  rename(NAME = `Contact: Mailing City`)
dots <- read_csv("data/member_xy.csv") 

# Download Geography ------------------------------------------------------
ohio <- states(cb = TRUE) %>% filter(STATEFP == 39)
oh_ctys <- counties(state = "OH", cb = T)
districts <- congressional_districts(cb = TRUE) %>% filter(STATEFP == 39)
state_house <- state_legislative_districts(state = "oh", cb = TRUE, house = "lower")
state_senate <- state_legislative_districts(state = "oh", cb = TRUE, house = "upper")
roads <- primary_roads()
oh_rds <- st_intersection(ohio,roads) %>% select(-(STATEFP:AWATER))

# Dot operations ----------------------------------------------------------
#dots_sf <- dots %>% st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)
#
#districts_sf <- st_transform(districts, crs = 4326)
#dot_merge_districts <- st_intersection(districts_sf,dots_sf) %>% 
#  select(uniqueid,GEOID) %>% 
#  group_by(GEOID) %>% tally() %>% 
#  st_drop_geometry() #this gives us the count of dots for each GEOID 
#distnew <- full_join(districts,dot_merge2)



# 3 district plots -------------------------------------------------------------------
mapcolorsd <- inlmisc::GetColors((nrow(districts)))
mapcolorssh <- inlmisc::GetColors((nrow(state_house)))
mapcolorsss <- inlmisc::GetColors((nrow(state_senate)))

# With (random) colors ----------------------------------------------------
#ggplot() +
#  geom_sf(data = districts, 
#          aes(fill = factor(GEOID)),
#          alpha=0.3, color = "BLACK", size=0.6) +
#  geom_point(data = dots, 
#             aes(y = Latitude, x = Longitude),
#             color = "BLACK", size=0.35, shape=18) +
#  geom_sf(data = ohio, 
#          alpha=0, color = "BLACK", size = 0.6) +
#  scale_fill_manual(values = sample(mapcolorsd),
#                    guide = FALSE) +
#  ggtitle("League of Women Voters of Ohio Membership",  
#          subtitle = "Congressional Districts") +
#  theme_void() +
#  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
#        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
#  ggsave("plots/congressional.png", width=7, height=7)

ggplot() +
  geom_sf(data = districts, 
          aes(fill = factor(GEOID)),
          alpha=0.3, color = NA, size=0.9) +
  geom_sf(data = oh_ctys, color = "white",
          size = .95, alpha = 0) +
  geom_sf(data = oh_rds, color = "gray46",
          size = .75) +
  geom_point(data = dots, 
             aes(y = Latitude, x = Longitude),
             color = "BLACK", size=0.9, shape=18) +
  geom_sf(data = ohio, 
          alpha=0, color = "BLACK", size = 0.75) +
  scale_fill_manual(values = sample(mapcolorsd),
                    guide = FALSE) +
  ggtitle("League of Women Voters of Ohio Membership",  
          subtitle = "Ohio Congressional Districts") +
  theme_void() +
  theme(plot.title = element_text(face="bold", size = 34, hjust = 0.5), 
        plot.subtitle = element_text(size = 28, hjust = 0.5)) +
  ggsave("plots/state_congressn.png", width=15, height=15)

ggplot() +
  geom_sf(data = state_house, 
          aes(fill = factor(GEOID)),
          alpha=0.3, color = NA, size=0.9) +
  geom_sf(data = oh_ctys, color = "white",
          size = .95, alpha = 0) +
  geom_sf(data = oh_rds, color = "gray46",
          size = .75) +
  geom_point(data = dots, 
             aes(y = Latitude, x = Longitude),
             color = "BLACK", size=0.9, shape=18) +
  geom_sf(data = ohio, 
          alpha=0, color = "BLACK", size = 0.75) +
  scale_fill_manual(values = sample(mapcolorssh),
                    guide = FALSE) +
  ggtitle("League of Women Voters of Ohio Membership",  
          subtitle = "Ohio House Districts") +
  theme_void() +
  theme(plot.title = element_text(face="bold", size = 34, hjust = 0.5), 
        plot.subtitle = element_text(size = 28, hjust = 0.5)) +
  ggsave("plots/state_housen.png", width=15, height=15)

ggplot() +
  geom_sf(data = state_senate, 
          aes(fill = factor(GEOID)),
          alpha=0.3, color = NA, size=0.9) +
  geom_sf(data = oh_ctys, color = "white",
          size = .95, alpha = 0) +
  geom_sf(data = oh_rds, color = "gray46",
          size = .75) +
  geom_point(data = dots, 
             aes(y = Latitude, x = Longitude),
             color = "BLACK", size=0.9, shape=18) +
  geom_sf(data = ohio, 
          alpha=0, color = "BLACK", size = 0.75) +
  scale_fill_manual(values = sample(mapcolorsss),
                    guide = FALSE) +
  ggtitle("League of Women Voters of Ohio Membership",  
          subtitle = "Ohio Senate Districts") +
  theme_void() +
  theme(plot.title = element_text(face="bold", size = 34, hjust = 0.5), 
        plot.subtitle = element_text(size = 28, hjust = 0.5)) +
  ggsave("plots/state_senaten.png", width=15, height=15)

#ggplot() +
#  geom_sf(data = state_senate, 
#          aes(fill = factor(GEOID)),
#          alpha=0.3, color = "BLACK", size=0.6) +
#  geom_point(data = dots, 
#             aes(y = Latitude, x = Longitude),
#             color = "BLACK", size=0.35, shape=18) +
#  geom_sf(data = ohio, 
#          alpha=0, color = "BLACK", size = 0.6) +
#  scale_fill_manual(values = sample(mapcolorsss),
#                    guide = FALSE) +
#  ggtitle("League of Women Voters of Ohio Membership",  
#          subtitle = "Ohio Senate Districts") +
#  theme_void() +
#  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
#        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
#  ggsave("plots/state_senate.png", width=7, height=7)

# No color --------------------------------------------------------------
ggplot() +
  geom_sf(data = districts, show.legend = FALSE,
          aes(fill = factor(GEOID)),
          alpha=0, color = "BLACK", size=0.6) +
  geom_sf(data = ohio, fill = "grey60",  
          alpha=0.3, color = "BLACK", size = 0.75) +
  geom_point(data = dots, 
             aes(y = Latitude, x = Longitude),
             color = "BLACK", size=0.35, shape=18) +
  #scale_fill_manual(values = sample(mapcolorsss),
  #guide = FALSE) +
  ggtitle("League of Women Voters of Ohio Membership", 
          subtitle = "Congressional Districts") +
  theme_void() +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
  ggsave("plots/congressional_nocolor.png", width=7, height=7)

ggplot() +
  geom_sf(data = state_house, show.legend = FALSE,
          aes(fill = factor(GEOID)),
          alpha=0, color = "BLACK", size=0.6) +
  geom_sf(data = ohio, fill = "grey60",  
          alpha=0.3, color = "BLACK", size = 0.75) +
  geom_point(data = dots, 
             aes(y = Latitude, x = Longitude),
             color = "BLACK", size=0.35, shape=18) +
  #scale_fill_manual(values = sample(mapcolorsss),
  #guide = FALSE) +
  ggtitle("League of Women Voters of Ohio Membership", 
          subtitle = "Congressional Districts") +
  theme_void() +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
  ggsave("plots/state_house_nocolor.png", width=7, height=7)

ggplot() +
  geom_sf(data = state_senate, show.legend = FALSE,
          aes(fill = factor(GEOID)),
          alpha=0, color = "BLACK", size=0.6) +
  geom_sf(data = ohio, fill = "grey60",  
          alpha=0.3, color = "BLACK", size = 0.75) +
  geom_point(data = dots, 
             aes(y = Latitude, x = Longitude),
             color = "BLACK", size=0.35, shape=18) +
  #scale_fill_manual(values = sample(mapcolorsss),
  #guide = FALSE) +
  ggtitle("League of Women Voters of Ohio Membership", 
          subtitle = "Ohio Senate Districts") +
  theme_void() +
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
  ggsave("plots/state_senate_nocolor.png", width=7, height=7)

####


ggplot() +
  geom_sf(data = distnew, 
          aes(fill = n),
          alpha=0.5,
          color = "BLACK") +
  theme_void()



# Graveyard ---------------------------------------------------------------
#dot_merge <- st_intersection(districts_sf,dots_sf) %>% 
#  select(uniqueid,GEOID) %>% 
#  inner_join(.,dots) #gives us the GEOID for each dot
