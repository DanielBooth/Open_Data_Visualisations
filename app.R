#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("global.R")

# Define UI for application that draws a histogram
ui <- 
  navbarPage("CoM Open Data - v.Hope",
             #----- Introduction tab -----
             tabPanel("Information",
                      fluidPage(
                        theme = shinytheme("slate"),
                      h1("Welcome"),
                      p("This is a really basic shiny web application. It uses R / Shiny and City of Melbourne's open data."),
                      p("All the data is queried from City of Melbourne open data."),
                      p("If you like you can get the data from here."),
                      p("The source code can be take from the github repo here."))
                      ),
             #----- "Industry Comparison (Example)" -----
             tabPanel("Area Profile",
                      #theme = shinytheme("cosmo"),
                      fluidPage(
                        theme = shinytheme("slate"),
                        titlePanel(h1("Small Area Profile")),
                        h2("Demogrphics"),
                        flowLayout(
                          selectInput("Area", "Area", small_areas_list),
                          submitButton("Apply Changes")),
                        fluidRow(
                          column(6,
                                 h3("Future Population"),
                                 plotOutput(outputId = "population_line_chart")
                          ),
                          column(6,
                                 h3("Dwellings Map"),
                                 leafletOutput("Dwelling_Map")
                                 )
                        ),
                        h2("Economics"),
                        flowLayout(
                          selectInput("Ind_year", "Year", year_list),
                          selectInput("Industry", "Industry", industry_list),
                          submitButton("Apply Changes")),
                        fluidRow(
                          column(6,
                                 h3("Industry Profile - Jobs by ANZSIC 1"),
                                 plotOutput(outputId = "anzsic_jobs_chart")
                          ),
                          column(6,
                                 h3("Jobs Map"),
                                 leafletOutput("Industry_Map")                          )
                        )
                        )
                      ),
#                        actionButton("download_data", "Download Data")
#                        )
#             ),
             # #----- tab -----
             # tabPanel("Allocation Analysis", h1("Under Development")),
             # #----- tab -----
             tabPanel("Other Stuff",
                      theme = shinytheme("slate"),
                      h1("Under Development"),
                      p(img(src="obi.gif")),
                      p("Nothing to see here, more along")))
             
# Define server logic required to draw a histogram
server <- function(input, output) {
   output$anzsic_jobs_chart <- renderPlot(emp_area_year_ind_plot(area = input$Area, year = input$Ind_year))

   # industry map output   
   output$population_line_chart <- renderPlot(population_area_forecast(area = input$Area))

   # industry map output   
   output$Industry_Map = renderLeaflet({
     
     tab_data <- 
       employment %>%
       filter(ANZSIC_1 == input$Industry) %>%
       filter(`Census year` == input$Ind_year)
       
     data_s <- 
       clue_block_poly %>%
       left_join(tab_data, c("block_id" = "Block ID"))
     
     data_s[is.na(data_s)] <- 0
     
     basic_choropleth(data_s,
                      "Jobs",
                      "PuBu"
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
                      "Blues"
                      )
   })
   
#   output$downloadData() 
   
}

# Run the application 
shinyApp(ui = ui, server = server)

