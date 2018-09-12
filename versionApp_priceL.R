## Functioning app
## Using price/L

# Beer data from the LDB in BC and US economic data

library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(shinythemes)
library(DT)



############################
# beer data stored locally #   ## make SQL database for these data??
############################

state <- read.csv("beer_by_state.csv", stringsAsFactors = FALSE)
ldb_clean <- read.csv("bcl-data-beer-clean-subset.csv", stringsAsFactors = FALSE)
heatmap_subset <- read.csv("ldb_heatmap_subset.csv", stringsAsFactors = FALSE)


###########
# UI code #
###########

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("BeerApp!", windowTitle = "Beer Picker"),
                "The beer industry has exploded in recent years.", 
                br(),
                "With so many new beers on the market, it can be hard decide",
                strong("what to drink"), 
                "and who is making", 
                strong("the best value beers."),
                br(), br(),
                "This app will help!",
                br(), br(),
                
                
                sidebarLayout(
                  sidebarPanel(strong("What kind of beer are you looking for?"),
                               br(), br(),
                               sliderInput("priceInput", "Price", min = 0, max = 30,
                                           value = c(4, 7), pre = "$"),
                               checkboxGroupInput("regionInput", label = "Choose Region(s)", choices =  c("ASIA", "CANADA", "EUROPE", "OTHER AMERICAS", "USA"), selected = c("CANADA", "USA")), #"AFRICA", "OCEANIA", 
                               uiOutput("styleSelectOutput")),
                  
                  mainPanel(
                    h3(textOutput("beerSummary")),# br(),
                    h3(textOutput("statSummary")), br(),
                    tabsetPanel(
                      tabPanel("Ratings for selected beers", plotOutput("beerRating")),
                      tabPanel("Results table", dataTableOutput("results"))
                    ))),
                br(), br(),
                fluidRow(column(width = 12, 
                                h3("Get the most bang for your buck!"),
                                plotOutput("heatmap"))),
                br(),
                fluidRow(column(width = 12, plotOutput("beerUS")))
)




###############
# Server code #
###############

server <- function(input, output) {
  filter_results <- reactive({
    if (is.null(input$regionInput)) {
      return(NULL)
    } 
    ldb_clean %>%
      filter(Price_L >= input$priceInput[1],
             Price_L <= input$priceInput[2],
             Style %in% input$styleInput,
             Region %in% input$regionInput
      )
  })
  
  heatmap_results <- reactive({
    if (is.null(input$regionInput)) {
      return(NULL)
    } 
    heatmap_subset %>%
      filter(Price_L >= input$priceInput[1],
             Price_L <= input$priceInput[2],
             Style %in% input$styleInput,
             Region %in% input$regionInput
      )
  })
  # output$regionOutput <- renderUI({
  #   selectInput("regionInput", "Region of Origin",
  #               sort(unique(ldb_clean_subset$Region)),
  #               selected = "CANADA")
  # })
  
  output$styleSelectOutput <- renderUI({
    selectInput("styleInput", "Beer Style",
                sort(unique(ldb_clean$Style)),
                multiple = TRUE,
                selected = c("IPA", "PORTER", "LAGER"))
  })
  
  output$beerRating <- renderPlot({
    if (is.null(filter_results())){
      return()
    }
    ggplot(filter_results(), aes(Rating, fill = Region)) +
      geom_histogram() + scale_fill_manual(values = alpha(c("#FF4081", "#5E35B1", "#2962FF", "#1DE9B6", "#AEEA00", "#F57C00", "#E040FB")))
  })
  
  output$results <- renderDataTable({
    filter_results()
  })
  
  output$heatmap <- renderPlot({
    if (is.null(heatmap_results())){
      return()
    }
    ggplot(heatmap_results(), aes(x = Region, y = Style)) +
      geom_tile(aes(fill = Value_metric_norm), color = "white") +
      scale_fill_gradient2(low = "yellow", mid = "orange", high = "blue", midpoint = 0.5) +
      #scale_fill_gradient2(low = "white", mid = "orange", high = "blue", midpoint = 0.5) +
      labs(fill = "Value") +
      theme(legend.title = element_text(face = "bold", size = 14)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size = 10, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.text.y = element_text(size = 10, face = "bold"))
  })
  
  # output$results <- renderDataTable({
  #   filter_results()
  # })
  # 
  
  
  output$beerSummary <- renderText({
    numBeers <- nrow(filter_results())
    if (is.null(numBeers)) {
      numBeers <- 0
    }
    paste0(numBeers, " beers meet your criteria")
  })
  
  output$statSummary <- renderText({
    statSummary <- round(mean(filter_results()$Rating), 2)
    if (is.na(statSummary)) {
      statSummary <- 0
    }
    paste0("The average rating for your selection is ", statSummary, " out of 5")
  })
  
}



# Run the app
shinyApp(ui = ui, server = server)

# color = c("#FF4081", "#5E35B1", "#2962FF", "#1DE9B6", "#AEEA00", "#F57C00", "#E040FB")
