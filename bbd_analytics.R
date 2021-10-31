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

# googlesheets::gs_auth(token = "MME_Oceania_token.rds")
sheets_auth(email = "imarrs@iu.edu")

# Read in the soil data
planeData <- read_sheet(ss = "1MPYG9HHopzCaACWB663W3eqSAfBbASOCKSJFuf0fRIc",
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

# Deadliest episode of the podcast
d_ep <- ggplot(planeData, aes(ep))+
  geom_bar(aes(weight = tfat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Episode") +
  ylab("Fatalities")

ggplotly(d_ep)

# Deadliest airline
dairl <- filter(planeData, tfat != 0)

d_airl <- ggplot(dairl, aes(x = airl, y = tfat, fill = cause2)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Airline") +
  ylab("Fatalities")

# Deadliest month
d_mo <- ggplot(planeData, aes(x=imonth, y=tfat, fill = cause2))+
  geom_bar(stat = "identity")+
  scale_x_continuous(name = "Month of incident", breaks = seq(1,12,1),
                     labels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Month") +
  ylab("Fatalities")

p <- ggplotly(d_mo)
p 

# Deadliest year
d_yr <- ggplot(d_planeData, aes(x=iyear, y=tfat, fill = cause2))+
  geom_bar(stat = "identity")+
  scale_x_continuous(name = "Year of incident", limits = c(1930, 2015))

d_yr

# Deadliest manu
d_manu <- ggplot(d_planeData, aes(x = manu, y = tfat, fill = cause2))+
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Manufacturer") +
  ylab("Fatalities")


d_manu

# Most common cause of incident
ggplot(planeData)+
  geom_bar(aes(x = cause2, fill = cause2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Cause of crash") +
  ylab("Number of crashes")

ggplot(planeData)+
  geom_bar(aes(x = imonth#, 
               #color = cause2
               )) +
  scale_x_continuous(name = "Month of incident", breaks = seq(1,12,1),
                     labels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Cause of crash") +
  ylab("Number of crashes")

ggplot(planeData)+
  geom_bar(aes(x = imonth), width=0.5, position = position_dodge(width=1)) +
  scale_x_continuous(name = "Month of incident", breaks = seq(1,12,1),
                     labels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Cause of crash") +
  ylab("Number of crashes")

# Playing with ggpubr ###

# Deadliest manu boxplot

ggboxplot(d_planeData, x = "manu", y = "tfat",
          color = "manu",
          palette = "jco")

# Cause histogram

gghistogram(d_planeData, x = "tfat",
          add = "mean",
          color = "cause2", fill = "cause2",
          palette = c("#00AFBB", "#E7B800", "#FC4E07")
)

ggbarplot(d_planeData, x = "id", y = "tfat",
          fill = "cause2",            # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 45           # Rotate vertically x axis texts
)

ggdotchart(d_planeData, x = "manu", y = "ep",
           color = "cause2",
           palette = "jco",
           #sorting = "descending",
           rotate = TRUE,
           dot.size = 2,
           y.text.col = TRUE,
           ggtheme = theme_pubr()
           )+
  theme_cleveland()

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
