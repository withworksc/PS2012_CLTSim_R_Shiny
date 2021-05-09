library(shiny)
library(tidyverse)
library(tseries)

set.seed(123)

#####################################################################
# Some Function that will be used in server 
#####################################################################

## Convert Input Distributiion as Integer 

distStrToNum <- function (distStr){
  if (distStr == "Uniform Distibution (0, 1)") {
    distNum <- 1
  } else if (distStr == "Normal Distribution (0, 1)") {
    distNum <- 2
  } else if (distStr == "Beta Distibution (0.5, 2)") {
    distNum <- 3
  } else if (distStr == "Beta Distibution (2, 0.5)") {
    distNum <- 4
  } else if (distStr == "Binomial Distribution (1, 0.5)") {
    distNum <- 5
  } else if (distStr == "Poisson Distribution (6)") {
    distNum <- 6
  } else if (distStr == "Exponential Distribution (6)") {
    distNum <- 7
  } else {
    distNum <- 8
  }
  
  return (distNum)
}

## Convert Input Statistic Type as Integer 

typeStrToNum <- function (typeStr) {
  if (typeStr == "Mean") {
    typeNum <- 1
  } else if (typeStr == "Median") {
    typeNum <- 2
  } else {
    typeNum <- 3
  }
  
  return (typeNum)
}

## Generating samples

makeSample <- function(dist, type, n, iter, p = NULL){
  
  estVec <- c()
  firstVec <- c()
  i = 1
  
  while (i != iter) {
    
    randVec <- c()
    estValue <- NULL
    
    if (dist == 1) {
      randVec <- runif(n, 0, 1)
    } else if (dist == 2) {
      randVec <- rnorm(n, 0, 1)
    } else if (dist == 3) {
      randVec <- rbeta(n, 0.5, 2)
    } else if (dist == 4) {
      randVec <- rbeta(n, 2, 0.5)
    } else if (dist == 5) {
      randVec <- rbinom(n, 1, 0.5)
    } else if (dist == 6) {
      randVec <- rpois(n, 6)
    } else if (dist == 7) {
      randVec <- rexp(n, 6)
    } else {
      randVec <- runif(n, 1, 100)
    }
    
    if (i == 1) {
      firstVec <- randVec
    }
    
    if (type == 1) {
      estValue <- mean(randVec)
    } else if (type == 2) {
      estValue <- median(randVec)
    } else if (type == 3){
      estValue <- quantile(randVec, p)
    }
    
    estVec[i] <- estValue
    
    i <- i + 1
  }
  
  estList <- list(firstVec, estVec)

  return(estList)
}

## Extract test statistic from jarque-bera test

extractJbStat <- function (listInput) {
  jbObj <- jarque.bera.test(listInput) 
  jbStatValue <- jbObj[[1]][[1]]
  
  return(jbStatValue)
}

## Extract test pvalue from jarque-bera test

extractJbPvalue <- function (listInput) {
  jbObj <- jarque.bera.test(listInput) 
  jbPvalue <- jbObj[[3]]
  
  return(jbPvalue)
}

#####################################################################
#####################################################################


#####################################################################
# Shiny UI 
#####################################################################

ui <- fluidPage(
  titlePanel("Central Limit Theorem Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("Distribution", 
                  "Please Select A Distribution Type", 
                  choices = c("Uniform Distibution (0, 1)", 
                              "Normal Distribution (0, 1)",
                              "Beta Distibution (0.5, 2)",
                              "Beta Disribution (2, 0.5)",
                              "Binomial Distribution (1, 0.5)",
                              "Poisson Distribution (6)",
                              "Exponential Distribution (6)",
                              "Any Number From 1 to 100")
                  ),
      
      selectInput("Statistics",
                  "Please Select A Statistic Type",
                  choices = c("Mean",
                              "Median",
                              "Quantile")
                  ),
      
      textInput("N",
                "Sample Number (N)",
                100),
      
      textInput("Iter",
                "Iterations (Iter)",
                1000),

      conditionalPanel(condition = "input.Statistics == 'Quantile'",
                       textInput("AnyQuantile",
                                 "Please Enter A Number Between 0 to 1",
                                 0)
                       )
      
    ),
    mainPanel(
      
      tabsetPanel(
      
      tabPanel("Plot", 
               h3("Plot of the Sampling Distribution"), 
               plotOutput("cltPlot"),
               h3("Plot of the Distribution of First Sample"),
               plotOutput("firstSamplePlot")
               ),
      
      tabPanel("Sampling Distribution Normality Check",
               h4("Test Statistic in Jarque-Bera Test:"),
               textOutput("jbstat"),
               h4("p-Value in Jarque-Bera Test:"),
               textOutput("pvalue"),
               h4("Does the Sample Statistic Follows Normal Distribution?"),
               textOutput("cltNorm")
               )
      
      )
      
    )
  )
)

#####################################################################
#####################################################################


#####################################################################
# Shiny Server
#####################################################################

server <- function (input, output, session) {

  estSample <- reactive({
    
    distValue <- distStrToNum(input$Distribution)
    typeValue <- typeStrToNum(input$Statistics)
    nValue <- as.numeric(input$N)
    iterValue <- as.numeric(input$Iter)
    
    if (typeValue != 3) {
      estSample <- makeSample(distValue, typeValue, nValue, iterValue)
    } else {
      anyQuantValue <- as.numeric(input$AnyQuantile)
      estSample <- makeSample(distValue, typeValue, nValue, iterValue, anyQuantValue)
    }
    
    return(estSample)
    
  })
  
  output$cltPlot <- renderPlot(
    
    ggplot() + 
      geom_histogram(aes(estSample()[[2]], ..density..), 
                     bins = 50, 
                     fill = "steelblue3", 
                     alpha = 0.5) +
      geom_density(aes(estSample()[[2]]), 
                   color = "goldenrod2", 
                   size = 0.8) +
      labs(x = "Values") + 
      theme_bw()
    
  )
  
  output$firstSamplePlot <- renderPlot(
    
    ggplot()+
      geom_histogram(aes(estSample()[[1]], ..density..), 
                     bins = 50, 
                     fill = "steelblue3", 
                     alpha = 0.5) +
      labs(x = "Values") + 
      theme_bw()
    
  )
  
  
  output$jbstat <- renderText(extractJbStat(estSample()[[2]]))
  
  output$pvalue <- renderText(extractJbPvalue(estSample()[[2]]))  
  
  output$cltNorm <- renderText(if_else(extractJbPvalue(estSample()[[2]]) >= 0.05, "Yes", "No"))
  
}

#####################################################################
#####################################################################


#####################################################################
# Run Shiny App
#####################################################################

shinyApp(ui = ui, server = server)

