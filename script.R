##### Import libraries #####
library(tools)
library(tmap)
library(sp)
library(ggmap)
library(raster)
library(rgdal)
library(tmaptools)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(data.table)

setwd("C:/work/SDG/SDG_4")

country_data <- read.csv("data/NER_countries.csv")
state_data <- read.csv("data/NER_states.csv")
district_data <- read.csv("data/NER_districts.csv")


district_data$DISTNAME <- toTitleCase(as.character(tolower(district_data$DISTNAME)))
district_data$DISTNAME <- as.factor(district_data$DISTNAME)

summary(district_data$primary)
summary(district_data$upper_primary)

boxplot(district_data$primary, na.omit = TRUE)
boxplot(district_data$upper_primary)

##omit the rows for which NER for primary is missing.
district_data_p <- district_data[complete.cases(district_data[,-4]),-4]

##omit the rows for which NER for upper primary is missing.
district_data_up <- district_data[complete.cases(district_data[,-3]),-3]

str(district_data_p)


#################### Visualization###############################


###### Thematic Map of Districts ######

Ind_dist <- read_shape("shapefile/District/2011_Dist.shp")

Ind_state <- read_shape("shapefile/State/Admin2.shp")

district_data$NER_P[district_data$primary >= 99] <- ">=99"
district_data$NER_P[district_data$primary >=90 & district_data$primary < 99] <- "90-99"
district_data$NER_P[district_data$primary >=80 & district_data$primary <90] <- "80-90"
district_data$NER_P[district_data$primary >= 70 & district_data$primary <80] <- "70-80"
district_data$NER_P[district_data$primary <70] <- "0-70"

district_data$NER_P <- as.factor(district_data$NER_P)
district_data$NER_P <- ordered (district_data$NER_P, c(">=99", "90-99","80-90","70-80","0-70"))


district_data$NER_UP[district_data$upper_primary >= 99] <- ">=99"
district_data$NER_UP[district_data$upper_primary >= 90 & district_data$upper_primary<99] <- "90-99"
district_data$NER_UP[district_data$upper_primary >=80 & district_data$upper_primary<90] <- "80-90"
district_data$NER_UP[district_data$upper_primary >= 70 & district_data$upper_primary<80] <- "70-80"
district_data$NER_UP[district_data$upper_primary<70] <- "0-70"

district_data$NER_UP <- as.factor(district_data$NER_UP)
district_data$NER_UP <- ordered(district_data$NER_UP, c(">=99", "90-99","80-90","70-80","0-70"))

districtdata_map <- merge(Ind_dist, district_data, by.x= "DISTRICT", by.y = "DISTNAME", all.x = TRUE)  

#### Net Enrolment Ratio at the Primary Level

## Interactive Mode  
tmap_mode("view")

tm_shape(districtdata_map) + 
  tm_polygons("NER_P", aes.palette = "seq", palette = "-Blues", 
              contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) +
  tm_scale_bar(position=c("left", "bottom")) + 
  tm_style("albatross", bg.color = "grey85", frame.lwd=10) +
  tm_layout(  title = "Target 4.1: Net Enrolment Ratio at Primary levels")  +
  tmap_options(max.categories = 650) + 
  tm_view(view.legend.position = c("right","bottom"), alpha = 1) +
  tm_shape(Ind_state) +
  tm_borders("grey", lwd = .8) 

## Plot Mode

tmap_mode("plot")


tm_shape(districtdata_map) + 
  tm_polygons("NER_P", aes.palette = "seq", palette = "-Blues", 
              contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) + 
  tmap_options(max.categories = 650) +
  tm_layout(legend.text.size = 0.6,
            legend.position = c("right","top"))  +
  tm_shape(Ind_state) +
  tm_borders("grey", lwd = .8)
    
district_interactive_map_p <- tmap_last()
  
district_interactive_map_p <- tmap_leaflet(district_interactive_map_p)
  
district_interactive_map_p <-  district_interactive_map_p %>% 
    setView(78, 20, zoom = 4)
  
saveWidget(district_interactive_map_p, "districtmap_p.html", selfcontained = FALSE)
  

#### Net Enrolment Ratio at the Upper Primary Level

## Interactive Mode

tmap_mode("view")

tm_shape(districtdata_map) + 
  tm_polygons("NER_UP", aes.palette = "seq", palette = "-Blues", 
              contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) + 
  tm_style("albatross", bg.color = "grey85") +
  tm_layout(  title = "Target 4.1: Net Enrolment Ratio at Upper Primary levels")  +
  tmap_options(max.categories = 650) +
  tm_view(view.legend.position = c("right","bottom")) +
  tm_shape(Ind_state) +
  tm_borders("grey", lwd = .8)

## Plot Mode

tmap_mode("plot")

tm_shape(districtdata_map) + 
  tm_polygons("NER_UP", aes.palette = "seq", palette = "-Blues", 
              contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) + 
  tmap_options(max.categories = 650) +
  tm_layout(legend.text.size = 0.6,
            legend.position = c("right","top"))  +
  tm_shape(Ind_state) +
  tm_borders("grey", lwd = .8)

  
district_interactive_map_up <- tmap_last()
  
district_interactive_map_up <- tmap_leaflet(district_interactive_map_up)
  
  
district_interactive_map_up <-  district_interactive_map_up %>% 
    setView(78, 20, zoom = 4 )
  
saveWidget(district_interactive_map_up, "districtmap_up.html", selfcontained = FALSE)

###### Thematic Map of States #######
  
state_data <- read.csv("data/NER_states.csv")

Ind_state <- read_shape("shapefile/State/Admin2.shp")

state_data$STATNAME <- toTitleCase(as.character(tolower(state_data$STATNAME)))
state_data$STATNAME <- as.factor(state_data$STATNAME)

summary(state_data)  

state_data_2017 <- dplyr::filter(state_data, state_data$Year == '2017') 

summary(state_data_2017)
boxplot(state_data_2017$primary)
boxplot(state_data_2017$upper_primary)

state_data_2017$NER_P[state_data_2017$primary >= 99] <- ">=99"
state_data_2017$NER_P[state_data_2017$primary >= 90 & state_data_2017$primary <99] <- "90-99"
state_data_2017$NER_P[state_data_2017$primary >=80 & state_data_2017$primary <90] <- "80-90"
state_data_2017$NER_P[state_data_2017$primary >=70 & state_data_2017$primary <80] <- "70-80"
state_data_2017$NER_P[state_data_2017$primary < 70] <- "0-70"

state_data_2017$NER_P <- as.factor(state_data_2017$NER_P)
state_data_2017$NER_P <- ordered(state_data_2017$NER_P, c(">=99","90-99","80-90","70-80","0-70"))

state_data_2017$NER_UP[state_data_2017$upper_primary >= 99] <- ">=99"
state_data_2017$NER_UP[state_data_2017$upper_primary >= 90 & state_data_2017$upper_primary < 99] <- "90-99"
state_data_2017$NER_UP[state_data_2017$upper_primary >=80 & state_data_2017$upper_primary <90] <- "80-90"
state_data_2017$NER_UP[state_data_2017$upper_primary >=70 & state_data_2017$upper_primary<80] <- "70-80"
state_data_2017$NER_UP[state_data_2017$upper_primary<70] <- "0-70"

state_data_2017$NER_UP <- as.factor(state_data_2017$NER_UP)
state_data_2017$NER_UP <- ordered(state_data_2017$NER_UP, c(">=99","90-99","80-90","70-80","0-70"))

state_data_map <- merge(Ind_state, state_data_2017, by.x = "ST_NM", by.y = "STATNAME", all.x = TRUE)

#### Net Enroment at Primary Level

## Interactive Mode

tm_view("view")

tm_shape(state_data_map) +
  tm_polygons("NER_P", aes.palette = "cat", palette = "-Blues", contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) +
  tm_style("albatross", bg.color = "grey85") +
  tm_layout(  title = "Target 4.1: Net Enrolment Ratio at Primary levels")  +
  tmap_options(max.categories = 650) +
  tm_view(view.legend.position = c("right","bottom"))

## Plot Mode

tm_view("plot")

tm_shape(state_data_map) +
  tm_polygons("NER_P", aes.palette = "cat", palette = "-Blues", 
              contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) +
  tmap_options(max.categories = 650) +
  tm_layout(legend.text.size = 0.6,
            legend.position = c("right","top"))
  

state_interactive_map_p <- tmap_last()

state_interactive_map_p <- tmap_leaflet(state_interactive_map_p)

state_interactive_map_p <-  state_interactive_map_p %>% 
  setView(78, 20, zoom = 4)

saveWidget(state_interactive_map_p, "statemap_p.jpg", selfcontained = FALSE)

#### Net Enrolment Ratio at the Upper Primary Level

## Interactive Mode

tm_view("view")
  
tm_shape(state_data_map) +
  tm_polygons("NER_UP", aes.palette = "cat", palette = "-Blues", contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) +
  tm_style("albatross", bg.color = "grey85") +
  tm_layout(  title = "Target 4.1: Net Enrolment Ratio at Upper Primary levels")  +
  tmap_options(max.categories = 650) +
  tm_view(view.legend.position = c("right","bottom"))

## Plot Mode

tm_view ("plot")

tm_shape(state_data_map) +
  tm_polygons("NER_UP", aes.palette = "cat", palette = "-Blues", contrast = .7, title = "Net Enrolment Ratio (%)",
              legend.show = TRUE) +
  tm_layout(legend.text.size = 0.6,
            legend.position = c("right","top"))  +
  tmap_options(max.categories = 650) +
  tm_view(view.legend.position = c("right","bottom"))



#### Comparing India's Net Enrolment Ratio with the rest of the world #####

country_data <- read.csv("data/NER_countries.csv")

country_data_dt <- data.table( country_data )

#  Get latest data for each country
country_data_p_latest <- country_data_dt[ , .SD[which.max(Year)] ,  keyby =  Country]

bar_df <- country_data_p_latest %>% dplyr::filter(!country_data_p_latest$Country %in% c("World"))
line_df <- country_data_p_latest %>% dplyr::filter(country_data_p_latest$Country %in% c("World")) 

## Net Enrolment Ratio at the Primary Level

ggplot(bar_df,aes(x=Country, y=primary)) +
  geom_bar(stat = "identity", width = .5, fill = "lightsteelblue") +
  theme(axis.text.x=element_text(size = 8, angle=45,hjust = .8))+
  theme(axis.title.y = element_text(size = 10))+
  theme(plot.title=element_text(size=12, colour = "darkblue", 
  vjust=3, hjust = .5)) +
  theme(plot.margin = unit(c(.5,1,-1,1), "cm"))+
  scale_y_continuous("School enrolment,primary(% net)", limits = c(0,100), 
  breaks = c(0,20,40,60,80,100)) +
  geom_hline(yintercept = line_df$primary, colour = "red")
  
## Net Enrolment Ratio at the Upper Primary Level

ggplot(bar_df,aes(x=Country, y=upper_primary)) +
  geom_bar(stat = "identity", width = .5, fill = "lightsteelblue") +
  theme(axis.text.x=element_text(size = 8, angle=45,hjust = .8))+
  theme(axis.title.y = element_text(size = 10))+
  theme(plot.title=element_text(size=12, colour = "darkblue", 
  vjust=3, hjust = .5)) +
  theme(plot.margin = unit(c(.5,1,-1,1), "cm"))+
  scale_y_continuous("School Enrolment,upper primary(% net)", 
  limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  geom_hline(yintercept = line_df$upper_primary, colour = "red")


##### Net Enrolment Ratio trends in India ########

scatter_india <- country_data %>% dplyr::filter(country_data$Country %in% c("India"))

## Net Enrolment Ratio at the Primary Level

ggplot(scatter_india, aes(x=Year, y=primary)) +
  geom_line(color = "red", size = 1) +
  scale_y_continuous("School Enrolment, primary(% net)", 
  breaks = seq(0, 100, by = 10),limits = c(0,100)) +
  geom_point(color = "red")

## Net Enrolment Ratio at the Upper Primary Level

ggplot(scatter_india, aes(x=Year, y=upper_primary)) +
  geom_line(color = "red", size = 1) +
  scale_y_continuous("School Enrolment, upper primary(% net)", 
  breaks = seq(0, 100, by = 10),limits = c(0,100)) +
  geom_point(color = "red")


