# global file

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(shinythemes)
library(viridisLite)
library(stringr)

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
  read_csv("https://data.melbourne.vic.gov.au/api/views/mi2q-4527/rows.csv?accessType=DOWNLOAD") %>% # load from open data 
  gather(ANZSIC_1, Jobs, -`Census year`, -`Block ID`, -`CLUE small area`) %>% # change from wide table to long tables
  mutate(Jobs = as.integer(Jobs)) %>% # Changes the jobs number to an integer rather than text, seriously open data team this is broken.
  mutate(Jobs = ifelse(is.na(Jobs), 0, Jobs)) %>% # Change NAs to 0 because a NA in this case is a 0
  mutate(`Block ID` = as.character(`Block ID`)) # Change the block id to a character so its type fits with our other data 


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
               fill = "#7947b8") +
      coord_flip()
      }

population_area_forecast <-
  function(area){
    ggplot(data = population %>%
             filter(geography == area),
           aes(y = total_population, 
               x = year))+
      geom_line(color = "#7947b8",
                size = 2)
  }
  
basic_choropleth <- function (data, value, pall) { # function to build a simple choropleth
  tm <- 
    tm_shape(data) + # 'data' is a data.frame with polygon
    tm_polygons(value, # 'value' is the layer we want to visualise
                alpha = .5, 
                palette = pall, # 'pall' is the style of colour pallete we want to use  
                border.col = "black",
                n = 7,
                showNA = FALSE) 
  tmap_leaflet(tm)
}




