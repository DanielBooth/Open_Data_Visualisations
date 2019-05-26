# global file

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(shinythemes)


#---- lists -----

small_areas_list <- c("Melbourne (CBD)", 
                 "Parkville",
                 "North Melbourne",
                 "Kensington",
                 "Carlton",
                 "Docklands",
                 "Port Melbourne",
                 "West Melbourne (Residential)",
                 "West Melbourne (Industrial)",
                 "East Melbourne",
                 "Southbank",
                 "South Yarra",
                 "Melbourne (Remainder)")

year_list <- c(2017,
               2015,
               2013,
               2011,
               2009,
               2006,
               2003)


industry_list <-c("Accommodation and Food Services",
                  "Administrative and Support Services",
                  "Agriculture, Forestry and Fishing",
                  "Arts and Recreation Services",
                  "Construction",
                  "Education and Training",
                  "Electricity, Gas, Water and Waste Services",
                  "Financial and Insurance Services",
                  "Health Care and Social Assistance",
                  "Information Media and Telecommunications",
                  "Manufacturing",
                  "Mining",
                  "Other Services",
                  "Professional, Scientific and Technical Services",
                  "Public Administration and Safety",
                  "Transport, Postal and Warehousing",
                  "Retail Trade",
                  "Rental, Hiring and Real Estate Services",
                  "Wholesale Trade"
                  )



#---- data download ----

population <- 
  read_csv("https://data.melbourne.vic.gov.au/resource/vtsx-jhki.csv?$query=SELECT%20year,%20total_population,%20geography")

small_areas <- 
  st_read("https://data.melbourne.vic.gov.au/api/geospatial/gei8-3w86?method=export&format=GeoJSON", stringsAsFactors = FALSE)


employment <- 
  read_csv("https://data.melbourne.vic.gov.au/api/views/b36j-kiy4/rows.csv?accessType=DOWNLOAD") %>%
  gather(ANZSIC_1, Jobs, -`Census year`, -`Block ID`, -`CLUE small area`) %>%
  mutate(Jobs = ifelse(is.na(Jobs), 0, Jobs)) %>%
  mutate(`Block ID` = as.character(`Block ID`))


dwelling_data <-
  read_csv("https://data.melbourne.vic.gov.au/api/views/44kh-ty54/rows.csv?accessType=DOWNLOAD")

dwellings <-
  dwelling_data %>%
  group_by(`Census year`, `Block ID`) %>%
  summarise(Dwellings = sum(`Dwelling number`, na.rm =TRUE)) %>%
  mutate(`Block ID` = as.character(`Block ID`))

clue_block_poly <-
  st_read("https://data.melbourne.vic.gov.au/api/geospatial/aia8-ryiq?method=export&format=GeoJSON",stringsAsFactors = FALSE) %>%
  select(-census_yr)

jobs_forecast <-
  read_csv("https://data.melbourne.vic.gov.au/api/views/gb88-t7zc/rows.csv?accessType=DOWNLOAD")


#---- functions  -----

emp_area_year_ind_plot <- 
  function(area, year){
    ggplot(data = employment %>% 
             filter(`CLUE small area` == area) %>%
             filter(`Census year` == year) %>%
             filter(ANZSIC_1 != "Total employment in block") %>%
             group_by(ANZSIC_1) %>%
             summarise(Jobs = sum(Jobs, na.rm = TRUE)) %>%
             arrange(-Jobs), 
           aes(y = `Jobs` ,
               x = reorder(`ANZSIC_1`, `Jobs`))) +
      geom_bar(stat = "identity",
               fill = "orangered1") +
      coord_flip()
      }

population_area_forecast <-
  function(area){
    ggplot(data = population %>%
             filter(geography == area),
           aes(y = total_population, 
               x = year))+
      geom_line(color = "orangered1",
                size = 2)
  }
  
basic_choropleth <- function (data, value, pall) {
  
  tm <- 
    tm_shape(data) + 
    tm_polygons(value,
                alpha = .5,
                palette = pall,
                border.col = "black",
                n = 7) 
#    tm_shape(subject) +
#    tm_fill(col = "black")
  
  tmap_leaflet(tm)
}




