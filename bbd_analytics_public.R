###
# BBD data analysis
# User: IJM
# Last modified: 10/31/2021
###

# Load required libraries
library(tidyverse)
library(googlesheets4)
library(plotly)
library(ggpubr)

###############################################
# Load and process BBD data
###############################################

sheets_auth(email = "")

# Read in the soil data
planeData <- read_sheet(ss = "",
                      range = "BBD_Data!A:Z",
                      col_types = "ddddddddddddcccccccccccccc"
)

d_planeData <- filter(planeData, tfat != 0)

###

d_planeData$cause2 <- factor(d_planeData$cause2, c("human","mechanical","unknown"), labels = c("Human","Mechanical","Unknown"))

# deadliest month
ggplot(d_planeData, aes(as.factor(imonth),tfat, fill = cause2))+
  geom_bar(stat = "identity",
           position = "dodge")+
  scale_fill_viridis_d(option = "plasma")+
  xlab("Month") +
  ylab("Fatalities")+
  labs(fill = "Cause")+
  theme_classic()+
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
        axis.text.x = element_text(size=12, face="bold", colour = "black"), # bold
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black")
        )

# deadliest year
ggplot(d_planeData, aes(as.factor(iyear),tfat, fill = cause2))+
  geom_bar(stat = "identity",
           position = "dodge")+
  scale_fill_viridis_d(option = "plasma")+
  xlab("Year") +
  ylab("Fatalities")+
  labs(fill = "Cause")+
  theme_classic()+
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
        axis.text.x = element_text(size=12, face="bold", colour = "black"), # bold
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black")
  )

# deadliest year
ggplot(d_planeData, aes(manu,tfat, fill = cause2))+
  geom_bar(stat = "identity",
           position = "dodge")+
  scale_fill_viridis_d(option = "plasma")+
  xlab("Manufacturer") +
  ylab("Fatalities")+
  labs(fill = "Cause")+
  theme_classic()+
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
        axis.text.x = element_text(size=12, face="bold", colour = "black"), # bold
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black")
  )

# deadliest departure country
ggplot(d_planeData, aes(dcountry,tfat, fill = cause2))+
  geom_bar(stat = "identity",
           position = "dodge")+
  scale_fill_viridis_d(option = "plasma")+
  xlab("Departure Country") +
  ylab("Fatalities")+
  labs(fill = "Cause")+
  theme_classic()+
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
        axis.text.x = element_text(size=12, face="bold", colour = "black"), # bold
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black")
  )

# deadliest airline
ggplot(d_planeData, aes(airl,tfat, fill = cause2))+
  geom_bar(stat = "identity",
           position = "dodge")+
  scale_fill_viridis_d(option = "plasma")+
  xlab("Airline") +
  ylab("Fatalities")+
  labs(fill = "Cause")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
        axis.text.x = element_text(size=12, face="bold", colour = "black"), # bold
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black")
  )

###

# Mapping

library(sf)
library(raster)
library(dplyr)
library(sp)
library(spData)
#library(spDataLarge)
library(leaflet)
library(choroplethr)
library(tmap)

df <- data.frame(longitude = planeData$long, 
            latitude = planeData$lat,
            cause_popup = planeData$cause,
            cause2_popup = planeData$cause2,
            ep_popup = planeData$title
)

coordinates(df) <- ~longitude+latitude+popup

m <- leaflet() %>% 
  addMarkers(lng = df$longitude, 
             lat = df$latitude, 
             popup = paste("Title: ", df$ep_popup, "<br>", "<br>",
                          "General cause: ", df$cause2_popup, "<br>", "<br>",
                          "Description: ", df$cause_popup)) %>% 
  addTiles()

m
