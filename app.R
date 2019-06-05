#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(viridisLite)
library(stringr)

source("global.R")

# Define UI for application that draws a histogram
ui <- 
  navbarPage("CoM Open Data - v.Empire",
             #----- Introduction tab -----
             tabPanel("Information",
                      fluidPage(
                        theme = shinytheme("simplex"),
                      h1("Welcome"),
                      p("This is a really basic shiny web application. It uses R / Shiny and City of Melbourne's open data."),
                      p("All the data is queried from City of Melbourne open data."),
                      p("If you like you can get the data from" , a(href = "https://data.melbourne.vic.gov.au/", "here.")),
                      p(a(href="https://github.com/DanielBooth/Open_Data_Visualisations", "The source code can be take from the github repo here.")),
                      p(a(href = "https://www.linkedin.com/in/daniel-booth-b6751129", "LinkedIn Profile")
                      ))
                      ),
             #----- "Industry Comparison (Example)" -----
             tabPanel("Area Profile",
                      #theme = shinytheme("cosmo"),
                      fluidPage(
                        theme = shinytheme("simplex"),
                        titlePanel(h1("Small Area Profile")),
                        h2("Demographics"),
                        flowLayout(
                          selectInput("Area", "Area", small_areas_list),
                          selectInput("Ind_year", "Year", year_list),
                          submitButton("Apply Changes")),
                        fluidRow(
                          column(6,
                                 h3(textOutput("future_population_titles")),
                                 plotOutput(outputId = "population_line_chart"),
                                 p("This shows the future population of the small area"),
                                 p("")
                          ),
                          column(6,
                                 h3(textOutput("dwelling_map_title")),
                                 leafletOutput("Dwelling_Map"),
                                 p("This shows where the dwellings across the City of Melbourne"),
                                 p("")
                                 ),
                          h3("Dwelling Types"),
                          tableOutput("Dwelling_Table")
                        ),
                        h2("Economics"),
                        flowLayout(
                          selectInput("Industry", "Industry", industry_list),
                          submitButton("Apply Changes")),
                        fluidRow(
                          column(6,
                                 h3(textOutput("Industry_Chart_Title")),
                                 plotOutput(outputId = "anzsic_jobs_chart")                                 
                          ),
                          column(6,
                                 h3(textOutput("jobs_map_title")),
                                 leafletOutput("Industry_Map")                          )
                        ),
                        h3("Industry Tables"),
                        tableOutput("Industry_Table")
                        )
                      ),

             # #----- tab -----
             tabPanel("Other Stuff",
                      theme = shinytheme("simplex"),
                      h1("Under Development"),
                      p(img(src="https://media.giphy.com/media/l2JJKs3I69qfaQleE/giphy.gif")),
                      p("Nothing to see here, more along")))
             
# Define server logic required to draw a histogram
server <- function(input, output) {
  
   # Dynamic titles 
  
   output$future_population_titles <- renderText(str_c("Future population - ", input$Area))
  
   output$Industry_Chart_Title <- renderText(str_c("Industry Profile - Jobs by ANZSIC 1 - ", input$Area))
   
   output$dwelling_map_title <- renderText(str_c("Dwelling Map ", " - ", input$Ind_year))
   
   output$jobs_map_title <- renderText(str_c("Jobs Map - ", input$Area, " - ", input$Ind_year, " - " , input$Industry))

   # jobs chart
        
   output$anzsic_jobs_chart <- renderPlot(emp_area_year_ind_plot(area = input$Area, year = input$Ind_year))

   
   # industry chart output   
   output$population_line_chart <- renderPlot(population_area_forecast(area = input$Area))

   # industry map output   
   output$Industry_Map = renderLeaflet({
     
     tab_data <- # build our tabular data for the industry we are looking at
       employment %>%
       filter(ANZSIC_1 == input$Industry) %>%
       filter(`Census year` == input$Ind_year)
       
     data_s <- # join the spatial data on the fly!
       clue_block_poly %>%
       left_join(tab_data, c("block_id" = "Block ID"))
     
     data_s[is.na(data_s)] <- 0 # remove NA's and turn them into 0
     
     basic_choropleth(data_s, # call our choropleth function
                      "Jobs",
                      "Purples"
                    )
   })
   
   output$Dwelling_Map = renderLeaflet({
     
     tab_data <- 
       dwellings %>%
       filter(`Census year` == input$Ind_year)
     
     data_s <- 
       clue_block_poly %>%
       left_join(tab_data, c("block_id" = "Block ID"))
     
     data_s[is.na(data_s)] <- 0
     
     basic_choropleth(data_s,
                      "Dwellings",
                      "Purples"
                      )
   })
   
   output$Dwelling_Table = renderTable({
     dwelling_table <- 
       dwelling_data %>%
       group_by(`Census year`, 
                `CLUE small area`,
                `Dwelling type`) %>%
       filter(`Census year` %in% year_list) %>%
       summarise(Dwellings = round(sum(`Dwelling number`))) %>%
       filter(`CLUE small area` == input$Area) %>%
       spread(`Census year`, Dwellings, fill = 0) %>%
       as.data.frame() %>%
       select(-`CLUE small area`)
     
     dwelling_table
     
   })
   
   output$Industry_Table = renderTable({
     employment %>%
       filter(`CLUE small area` == input$Area) %>%
       filter(`Census year` %in% year_list) %>%
       group_by(`Census year`, ANZSIC_1, `CLUE small area`) %>%
       summarise(Jobs = round(sum(Jobs, na.rm = TRUE))) %>%
       spread(`Census year`, Jobs) %>%
       arrange(-`2017`) %>%
       as.data.frame() %>%
       select(-`CLUE small area`)
  
   })
   
   
   
#   output$downloadData() 
   
}

# Run the application 
shinyApp(ui = ui, server = server)

