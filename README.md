# Spotify_Regression
An RShiny app going through multiple linear regression on Spotify Data

library(shiny)
library(corrplot)
library("readxl")

setwd("C:/Users/chris/Documents/MAT353/SpotifyRegression")
spot_data <- read_excel("Spotify-2000.xlsx")

ui <- fluidPage(
  navbarPage("Spotify Regression",

    sidebarLayout(
      sidebarPanel(
        titlePanel("Sidebar Regression Controls"),
        
        fluidRow(    
          column(12, 
              checkboxGroupInput(inputId = "varFinder", 
                     label = "Selected input variable(s):", 
                     choices = c("Year" = 1, 
                                 "BPM" = 2, 
                                 "Energy" = 3,
                                 "Danceability" = 4,
                                 "Loudness" = 5,
                                 "Liveness" = 6,
                                 "Valence" = 7,
                                 "Length" = 8,
                                 "Acousticness" = 9,
                                 "Speechiness" = 10),
                    selected = c(1,2,3,4,5,6,7,8,9,10))
              ),
               ),
         
        fluidRow(
          column(12,
              submitButton("Submit")
          )
        )
        
        ),
      mainPanel(
        tabsetPanel(type = "tabs",
              tabPanel
              (
                "Data", 
                h2("Table of the raw data", align = "center"),
                tableOutput('table')
              ),
              tabPanel(
                "Model Summary",
                h2("Model summary output", align = "center"),
                verbatimTextOutput('summary')
              ),
              tabPanel
              (
                "Normality",
                plotOutput('normPlot'),
                verbatimTextOutput('shapWilk')
                
              ),
              tabPanel
              (
                "ANOVA",
                h2("Analysis of variance output", align = "center"),
                verbatimTextOutput('anovaOut')
              ),
              tabPanel
              (
                "Correlation",
                h2("Correlation plot", align = "center"),
                plotOutput('corrplot')
              ),
              tabPanel
              (
                "Outliers",
                h2("Box plots", align = "center"),
                  plotOutput('box')
              )  
       )
      )
    )
  )
);

server <- function(input, output) {
  output$table <- renderTable(
    head(spot_data[,as.numeric(c(input$varFinder, 11))])
  )
  output$var <- renderPrint(
    input$varFinder
  )
  output$summary <- renderPrint(
    summary(lm(spot_data$Popularity~., data=spot_data[,as.numeric(c(input$varFinder, 11))]))
  )  
  output$anovaOut <- renderPrint(
    anova(lm(spot_data$Popularity~., data=spot_data[,as.numeric(c(input$varFinder, 11))]))
  )
  output$normPlot <- renderPlot(
    qqnorm(resid(lm(spot_data$Popularity~., data=spot_data[,as.numeric(c(input$varFinder, 11))])))
  )
  output$shapWilk <- renderPrint(
    shapiro.test(resid(lm(spot_data$Popularity~., data=spot_data[,as.numeric(c(input$varFinder, 11))])))
  )
  output$corrplot <- renderPlot(
    corrplot.mixed(cor(spot_data[,as.numeric(input$varFinder)]))
  )
  output$box <- renderPlot(
    boxplot(spot_data[,as.numeric(c(input$varFinder))], main="Year")
  )
}

shinyApp(ui = ui, server = server)
