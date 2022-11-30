library(shiny)
library(corrplot)
library("readxl")
library(car)

setwd("C:/Users/chris/Documents/MAT353/SpotifyRegression")
spot_data <- read_excel("Spotify-2000.xlsx")

ui <- fluidPage(
  navbarPage("Spotify Regression",
    sidebarLayout(
      sidebarPanel(width = 3,
        titlePanel("Regression Variables"),
        fluidRow(    
          column(6,
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
          column(
            6,
            h5("Output variable:"),
            h5("Popularity")
          )
               ),
        fluidRow(
          column(6,
              submitButton("Submit")
          )
        )
        ),
      mainPanel(
        tabsetPanel(type = "tabs",
              tabPanel
              (
                "Main", 
                img(src='spotify.png', height="15%", width="15%", align = "right"),
                h2("Spotify Regression", align = "center"),
                h3("Research Question: "), 
                h4("Is it possible to predict a song’s popularity from its key features?"),
                br(),
                h3("Purpose:"),
                h4("This app will walk through some of the important steps of creating a model using multiple linear regression"),
                br(),
                h3("Introduction:"),
                h4("The variables on the left-hand side correspond to the variables in the linear model."),
                h4("To choose input variables, check the variables and hit the submit button."),
                h4("The tabs above go through the important steps of creating a multiple linear regression model.")
              ),
              tabPanel
              (
                "1 - Data", 
                h2("Table of The Data", align = "center"),
                br(),
                h4("One of the most important steps in making a model is to look at the data. 
                   The table below shows the first 6 observations of each chosen variable."),
                tableOutput('table'),
                h4("Decription of the variables:"),
                h5("Popularity is the value we are trying to predict. 
                   It indicates the ranking of song based on its popularity."),
                h5("Year is the year that the song came out."),
                h5("BPM stands for beats per minute, and it indicates how fast the song is."),
                h5("Energy is a ranked variable based on how much energy the song has."),
                h5("Danceability is a ranked variable based on how much the song makes a person want to dance."),
                h5("Loudness indicates how loud the song is in decibels."),
                h5("Liveness is a variable to indicate how live the song is."),
                h5("Valance is a ranked variable based on how positive a song is."),
                h5("Length is how long the song lasts in minutes."),
                h5("Acousticness is a ranked variable based on how acoustic the song sounds."),
                h5("Speechiness is a variable to indicate how wordy a song is.")
              ),
              tabPanel
              (
                "2 - Outliers",
                h2("Outliers Using Boxplots", align = "center"),
                br(),
                h4("Outliers are important to take notice of when building a model. 
                   We use boxplots to visualize the distribution of each variable and to identify where we may have outliers. 
                   The outliers are indicated by the circles in the graph."),
                plotOutput('box')
              ),
              tabPanel(
                "3 - Model Summary",
                h2("Model Summary Output", align = "center"),
                br(),
                h4("This step is where we start builing the linear model. 
                   The output below shows us the summary of the model. 
                   We are interested in looking at the p-value Pr(>|t|) 
                   to make sure that each of our parameters are significant. 
                   A good guide to go by is if this value is greater than 0.05, 
                   we remove the variable from the model."),
                verbatimTextOutput('summary')
              ),
              tabPanel
              (
                "4 - Normality",
                h2("Normality", align = "center"),
                br(),
                h4("Checking for normality is one of the assumptions that we need to check before moving forward with our model. 
                   The Graph below shows a plot of the residuals versus the normal quantiles. 
                   For a model that is normal, the data should follow the solid green straight line"),
                plotOutput('normPlot'),
                h4("Sometimes it is hard to tell from the graph whether or not the model is normal. 
                   We now look at the Shapiro-wilk test for normality. 
                   The null hypothesis is that the model is normal. 
                   If the p-value is low, then we reject the null hypothesis."),
                verbatimTextOutput('shapWilk'),
                h4("If we conclude that the model is not normal, we would look into some transformations 
                   for the model such as the log transformation or a Box-Cox transformation.")
                
              ),
              tabPanel
              (
                "5 - Correlation",
                h2("Correlation Plot", align = "center"),
                h4("Another assumption that we need to look into for our model is multicolinearity.
                   We do not want our model to have variables that are highly correlated."),
                h4("Variables with an r value greater than 0.5 or lower than -0.5 is considered highly correlated.
                   This is also indicated in the graph by the size of the circles and how light or dark green the circles appear.
                   It would not make sense to have less than 2 variables as input 
                   in our graph because we need to compare the relationship between at least 2 variables.
                   Inputting less than 2 variables will result in an error."),
                h4("We can fix the issue of multicolinearity by removing either variable."),
                plotOutput('corrplot')
              ),
              tabPanel
              (
                "6 - ANOVA",
                h2("Analysis of variance output", align = "center"),
                h4("Below is the output for ANOVA: The Analysis of Variance. 
                   The output is useful for conducting tests of significance and displays the levels of variability within the model. "),
                verbatimTextOutput('anovaOut')
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
    qqPlot(resid(lm(spot_data$Popularity~., data=spot_data[,as.numeric(c(input$varFinder, 11))])),
           col="black",col.lines = "#1ed862", main = "QQ plot", ylab = "Residuals")
  )
  output$shapWilk <- renderPrint(
    shapiro.test(resid(lm(spot_data$Popularity~., data=spot_data[,as.numeric(c(input$varFinder, 11))])))
  )
  output$corrplot <- renderPlot(
      corrplot(cor(spot_data[,as.numeric(input$varFinder)]), method = 'circle', 
            type = 'lower', insig='blank', addCoef.col ='black', number.cex = 1.1, 
            diag=FALSE, tl.srt = 45, col = COL1("Greens"), tl.col = 'black')
  )
  output$box <- renderPlot(
    boxplot(spot_data[,as.numeric(c(input$varFinder, 11))], main="Box Plot", col = "#1ed862")
  )
}

shinyApp(ui = ui, server = server)
