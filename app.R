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
                        theme = shinytheme("united"),
                      h1("Welcome"),
                      p("This is a really basic shiny web application. It uses R / Shiny and City of Melbourne's open data."),
                      p("All the data is queried from City of Melbourne open data."),
                      p("If you like you can get the data from here."),
                      a(href="https://github.com/DanielBooth/Open_Data_Visualisations", "The source code can be take from the github repo here.")
                      )
                      ),
             #----- "Industry Comparison (Example)" -----
             tabPanel("Area Profile",
                      #theme = shinytheme("cosmo"),
                      fluidPage(
                        theme = shinytheme("united"),
                        titlePanel(h1("Small Area Profile")),
                        h2("Demogrphics"),
                        flowLayout(
                          selectInput("Area", "Area", small_areas_list),
                          submitButton("Apply Changes")),
                        fluidRow(
                          column(6,
                                 h3("Future Population"),
                                 plotOutput(outputId = "population_line_chart"),
                                 p("This shows the future population of the small area"),
                                 p("")
                          ),
                          column(6,
                                 h3("Dwellings Map"),
                                 leafletOutput("Dwelling_Map"),
                                 p("This shows where the dwellings across the City of Melbourne"),
                                 p("")
                                 ),
                          h3("Dwelling Types"),
                          tableOutput("Dwelling_Table")
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
                        ),
                        h3("Industry Tables"),
                        tableOutput("Industry_Table")
                        )
                      ),
#                        actionButton("download_data", "Download Data")
#                        )
#             ),
             # #----- tab -----
             # tabPanel("Allocation Analysis", h1("Under Development")),
             # #----- tab -----
             tabPanel("Other Stuff",
                      theme = shinytheme("united"),
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
                      "Oranges"
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
                      "Oranges"
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

