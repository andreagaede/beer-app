# Beer data from the LDB in BC and US economic data

library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(shinythemes)
library(DT)
library(usmap)



############################
# beer data stored locally #   ## make SQL database for these data??
############################

state <- read.csv("beer_by_state.csv", stringsAsFactors = FALSE)
state_growth <- read.csv("state_growth.csv")
ldb_clean <- read.csv("bcl-data-beer-clean-subset.csv", stringsAsFactors = FALSE)
heatmap_subset <- read.csv("ldb_heatmap_subset.csv", stringsAsFactors = FALSE)
ldb_clean_all <- read.csv("bcl-data-beer-clean.csv", stringsAsFactors = FALSE)
brew_growth <- read.csv("breweries_yr.csv")
brewers_actual <- read.csv("brewers_actual.csv")
brewers_percent <- read.csv("brewers_percent.csv")


###########
# UI code #
###########


ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel(tags$em(tags$mark(style="background-color: #64FFDA", "BeerApp!!")), windowTitle = "Beer Picker"),
                br(),
                h4("The beer industry has exploded in recent years."), 
                br(),
                h4("With so many new beers on the market, it can be hard to decide",
                   strong("what to drink"), 
                   "and who is making", 
                   strong("the best value beers.")),
                br(),
                h4("This app will help!"),
                br(), br(),
                
                
                sidebarLayout(
                  sidebarPanel(strong("What kind of beer are you looking for?"),
                               br(), br(),
                               sliderInput("priceInput", "Price", min = 0, max = 56,
                                           value = c(3, 20), pre = "$"),
                               sliderInput("ratingInput", "Rating", min = 0, max = 5,
                                           value = c(0, 5)),
                               checkboxGroupInput("regionInput", label = "Choose Region(s)", choices =  c("ASIA", "CANADA", "EUROPE", "OTHER AMERICAS", "USA"), selected = c("CANADA", "USA")), #"AFRICA", "OCEANIA", 
                               uiOutput("styleSelectOutput")),
                  
                  mainPanel(
                    h3(textOutput("beerSummary")), # br(),
                    h3(textOutput("statSummary")), br(),
                    tabsetPanel(
                      tabPanel("Ratings for selected beers", plotOutput("beerRating")),
                      tabPanel("Results table", dataTableOutput("results"))
                    ))),
                br(), br(),
                fluidRow(column(width = 12, 
                                h3("Get the most bang for your buck!"),
                                "Let's take a look at the largest markets: Canada, Europe, and the USA", br(),
                                "This heat map uses your ", strong("Price,"),  strong("Region,"),  " and ", strong("Style "), "selections to show areas of the market with the best value:",
                                br(),
                                tags$mark(style="background-color: #9932CC; color: white", "Violet = Good Value"), "    ", 
                                tags$mark(style="background-color: #FFFF00", "Yellow = Poor Value"),
                                plotOutput("heatmap"))),
                br(), br(),
                h3("Beer prices per liter in Canada by producing region"),
                h4("It turns out American beer is more expensive than European beer, even though they are neighbors...and trade partners (for now)!"),
                fluidRow(column(width = 12, 
                                tabsetPanel(
                                  tabPanel("All Regions", plotOutput("allRegion")),
                                  tabPanel("Major Markets", plotOutput("majorMarkets")),
                                  tabPanel("Major Markets - Price Distribution", plotOutput("marketHist")),
                                  tabPanel("Quality vs $$", plotOutput("marketLM"))
                                ))),
                h3("What do these highly rated beers have in common?"),
                h4("We'll take a closer look at Lagers and IPAs"),
                fluidRow(column(width = 6,
                                tabsetPanel(
                                  tabPanel("Major Styles", plotOutput("majorStyles")),
                                  tabPanel("Lagers & IPAs", plotOutput("selectStyles"))
                                )
                ),
                column(width = 6,
                       tabsetPanel(
                         tabPanel("ABV vs Price", plotOutput("abvPrice")),
                         tabPanel("Rating vs Price", plotOutput("ratingPrice"))
                       )
                )),
                br(),
                h3("The United States' craft brewing market is rapidly growing"),
                br(),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("brewInput", label = "Choose Market Share:", choices =  c("Brewpubs", "Microbreweries", "Regional_Breweries"), selected = "Microbreweries")),
                  mainPanel(
                    h4("Craft Beer Market Changes"),
                    plotOutput("numActual"),
                    br(),
                    h4("Percent Change in Growth of the Craft Beer Market"),
                    plotOutput("numPercent")
                  )),
                br(),
                br(), 
                fluidRow(column(width = 8, offset = 4, 
                                h4("Direct & Indirect Economic Impact of Craft Brewers in 2017 (USD)"),
                                plotOutput("beerUS")))
                
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
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Rating >= input$ratingInput[1],
             Rating <= input$ratingInput[2],
             Style %in% input$styleInput,
             Region %in% input$regionInput
      )
  })
  
  heatmap_results <- reactive({
    if (is.null(input$regionInput)) {
      return(NULL)
    } 
    heatmap_subset %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
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
      geom_histogram(color = "gray") + scale_fill_manual(values = c("AFRIA" = "#FF4081", "ASIA" = "#5E35B1", "CANADA" = "#2962FF", "EUROPE" = "#1DE9B6", "OCEANIA" = "#AEEA00", "OTHER AMERICAS" = "#F57C00", "USA" = "#E040FB"))
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
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      theme(panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 14, face = "bold"),
            axis.title = element_blank())
  })
  
  output$allRegion <- renderPlot({
    ggplot(ldb_clean_all, aes(x = Region, y = Price_L, fill = Region, color = Region, alpha = 0.2)) +
      geom_boxplot(show.legend = FALSE) +
      scale_y_log10() + 
      ylab("Price per Liter (CAD)") +
      scale_fill_manual(values = c("#FF4081", "#5E35B1", "#2979FF", "#B9F6CA", "#AEEA00", "#F57C00", "#EA80FC")) +
      scale_color_manual(values = c("#FF4081", "#5E35B1", "#2962FF", "#1DE9B6", "#AEEA00", "#F57C00", "#E040FB")) +
      geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 1/2, show.legend = FALSE) +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      ggtitle("Beer prices in Canada by producing region")
  })
  
  
  output$majorMarkets <- renderPlot({
    majorMarket_subset <- ldb_clean_all %>% 
      filter(Region == "CANADA" | Region == "USA" | Region == "EUROPE")
    ggplot(majorMarket_subset, aes(x = Region, y = Price_L, fill = Region, color = Region, alpha = 0.2)) + 
      scale_y_log10() + 
      ylab("Price per Liter (CAD)") +
      geom_boxplot(outlier.colour = "dark gray", show.legend = FALSE) +
      scale_fill_manual(values = c("#2979FF", "#B9F6CA", "#EA80FC")) +
      scale_color_manual(values = c("#2962FF", "#1DE9B6", "#E040FB")) +
      geom_jitter(position = position_jitter(width = 0.3, height = 0), alpha = 1/2, show.legend = FALSE) +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      ggtitle("A closer look at beer prices from major markets in Canada") 
  })
  
  output$marketHist <- renderPlot({
    majorMarket_subset <- ldb_clean_all %>% 
      filter(Region == "CANADA" | Region == "USA" | Region == "EUROPE") %>% 
      filter(Price_L < 16)
    ggplot(majorMarket_subset, aes(x = Price_L)) +
      geom_histogram(data = subset(majorMarket_subset, Region == "CANADA"), fill = "#2962FF", alpha = 1/3, binwidth = 0.5) +
      geom_histogram(data = subset(majorMarket_subset, Region == "EUROPE"), fill = "#1DE9B6", alpha = 1/3, binwidth = 0.5) +
      geom_histogram(data = subset(majorMarket_subset, Region == "USA"), fill = "#E040FB", alpha = 1/2, binwidth = 0.5) +
      xlab("Beer price per liter ($/L)") +
      ylab("Number of beers") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      ggtitle("Number of beers sold at each price point in each market:   Blue = Canada,   Green = Europe,   Pink = USA")
  })
  
  output$marketLM <- renderPlot({
    majorMarket_subset <- ldb_clean_all %>% 
      filter(Region == "CANADA" | Region == "USA" | Region == "EUROPE") %>% 
      filter(Price_L < 16)
    ggplot(majorMarket_subset, aes(x = Price_L, y = Rating, color = Region, show.legend = FALSE)) +
      scale_x_continuous(limits = c(2,15)) +
      scale_y_continuous(limits = c(1.5,5)) +
      geom_smooth(show.legend = FALSE) +
      geom_point(alpha = 1/2, show.legend = FALSE) + 
      scale_color_manual(values = c("#2962FF", "#1DE9B6", "#E040FB")) +
      facet_wrap(~Region) +
      xlab("Beer price per liter (CAD/L)") +
      ylab("Rating out of 5") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      ggtitle("Rating increases with price up to $6/Liter in Canada and the US -- you get what you pay for, unless you pay more than $6")
  })
  
  output$majorStyles <- renderPlot({
    ldb_IBU <- ldb_clean_all %>% 
      filter(!is.na(IBU)) %>% 
      filter(Style == "LAGER" | Style == "IPA" | Style == "DARK ALE" | Style == "PALE ALE" | Style == "WHEAT")
    ldb_IBU$IBU <- as.numeric(ldb_IBU$IBU)
    ggplot(ldb_IBU, aes(x = IBU, y = Rating, color = Style)) +
      geom_point() +
      scale_x_log10() +
      scale_color_manual(values = c("#FF4081", "#6200EA", "#00C853", "#F57C00", "#E040FB")) +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      geom_smooth(method = lm, se = FALSE)
  })
  
  output$selectStyles <- renderPlot({
    ldb_IBU <- ldb_clean_all %>% 
      filter(!is.na(IBU)) %>% 
      filter(Style == "LAGER" | Style == "IPA" | Style == "DARK ALE" | Style == "PALE ALE" | Style == "WHEAT")
    ldb_IBU$IBU <- as.numeric(ldb_IBU$IBU)
    lager_IPA_IBU <- ldb_IBU %>% 
      filter(Style == "LAGER" | Style == "IPA")
    
    ggplot(lager_IPA_IBU, aes(x = IBU, y = Rating, color = Style)) +
      geom_point() +
      scale_x_log10() +
      scale_color_manual(values = c("#6200EA", "#00C853")) +
      geom_smooth(method = lm, se = FALSE) +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      xlab("International Bittering Unit (IBU)")
  })
  
  
  output$abvPrice <- renderPlot({
    majorMarket_subset <- ldb_clean_all %>% 
      filter(Region == "CANADA" | Region == "USA" | Region == "EUROPE")
    ggplot(majorMarket_subset, aes(x = Price_L, y = ABV)) + 
      geom_point(aes(size = Rating), pch = 21) + 
      scale_fill_gradient2(low = "yellow", high = "blue", mid = "orange", midpoint = 3) +
      scale_size_continuous(range = c(0.1,8)) +
      scale_x_continuous(limits = c(0, 15)) +
      facet_grid(~Region) + 
      aes(alpha = 1/3, fill = Rating) +
      guides(size = FALSE) +
      guides(alpha = FALSE) +
      xlab("Beer price per liter (CAD/L)") +
      ylab("Alcohol content (%)") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      ggtitle("High alcohol beers have higher prices")
  })
  
  output$ratingPrice <- renderPlot({
    majorMarket_subset <- ldb_clean_all %>% 
      filter(Region == "CANADA" | Region == "USA" | Region == "EUROPE")
    ggplot(majorMarket_subset, aes(x = Price_L, y = Rating)) + 
      scale_x_continuous(limits = c(0, 15)) +
      geom_point(aes(size = ABV), pch = 21) + 
      scale_size_continuous(range = c(0.1,10)) + 
      facet_grid(~Region) + aes(alpha = 1/3, fill = ABV) +
      guides(size = FALSE) +
      guides(alpha = FALSE) +
      scale_fill_gradient2(low = "yellow", high = "blue", mid = "orange", midpoint = 4.5) +
      xlab("Beer price per liter (CAD/L)") +
      ylab("Rating out of 5") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      ggtitle("Highly rated beers have higher prices")
  })
  
  
  output$beerSummary <- renderText({
    numBeers <- nrow(filter_results())
    if (is.null(numBeers)) {
      numBeers <- 0
    }
    paste0(numBeers, " beers meet your criteria!")
  })
  
  output$statSummary <- renderText({
    statSummary <- round(mean(filter_results()$Rating), 2)
    if (is.na(statSummary)) {
      statSummary <- 0
    }
    paste0("The average rating for your selection is ", statSummary, " out of 5")
  })
  
  
  output$beerUS <- renderPlot({
    plot_usmap(data = state_growth, values = "Economic_impact", lines = "#FFA726") +
      scale_fill_continuous(low = "#FFF3E0", high = "#EF6C00", name = "Economic Impact (USD)", label = scales::comma) +
      theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 14))
  })
  
  
  
  filter_brewers_percent <- reactive({
    if (is.null(input$brewInput)) {
      return(NULL)
    } 
    brewers_percent %>%
      filter(variable %in% input$brewInput
      )
  })
  
  filter_brewers_actual <- reactive({
    if (is.null(input$brewInput)) {
      return(NULL)
    } 
    brewers_actual %>%
      filter(variable %in% input$brewInput
      )
  })
  
  
  output$numActual <- renderPlot({
    if (is.null(filter_brewers_actual())){
      return()
    }
    ggplot(filter_brewers_actual(), aes(x = Year, y = value, color = variable, fill = variable)) +
      geom_line() +
      geom_area(position = "identity", alpha = 1/2) +
      scale_colour_manual(values=c("Brewpubs" = "#2962FF", "Microbreweries" = "#D500F9", "Regional_Breweries" = "#1DE9B6")) +
      scale_fill_manual(values=c("Brewpubs" = "#2962FF", "Microbreweries" = "#D500F9", "Regional_Breweries" = "#1DE9B6")) +
      ylab("Number of craft brewers") +
      theme(legend.position="top") +
      theme(legend.text =  element_text(size = 14), axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      theme(legend.title=element_blank())
  })
  
  output$numPercent <- renderPlot({
    if (is.null(filter_brewers_percent())){
      return()
    }
    ggplot(filter_brewers_percent(), aes(x = Year, y = value, color = variable, fill = variable)) +
      geom_line() +
      geom_area(position = "identity", alpha = 1/2) +
      scale_colour_manual(values=c("Brewpubs" = "#2962FF", "Microbreweries" = "#D500F9", "Regional_Breweries" = "#1DE9B6")) +
      scale_fill_manual(values=c("Brewpubs" = "#2962FF", "Microbreweries" = "#D500F9", "Regional_Breweries" = "#1DE9B6")) +
      ylab("% growth in number of craft brewers") +
      theme(legend.position="top") +
      theme(legend.text =  element_text(size = 14), axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
      theme(legend.title=element_blank())
  })
  
  
}



# Run the app
shinyApp(ui = ui, server = server)

# color = c("#FF4081", "#5E35B1", "#2962FF", "#1DE9B6", "#AEEA00", "#F57C00", "#E040FB")
