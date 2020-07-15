library(shiny)
library(arules)
library(ggplot2)
library(gridExtra)
library(arulesViz)
library(plotly)

supp = 0.03

conf = 0.2

ui <- shinyUI(fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose CSV File', accept=c('.csv')),
      sliderInput("supp", "Support:", min = 0.005, max = 0.1, value = supp , step = 1/10000), br(),
      sliderInput("conf", "Confidence:", min = 0.1, max = 1, value = conf , step = 1/10000), br()
      #numericInput("minL", "Min. items per set:", 2), br(), 
      #numericInput("maxL", "Max. items per set::", 3), br()
    ),
    
    mainPanel(
      titlePanel("Association Rules"),
      tabsetPanel(
        tabPanel('Frequency', value='frequency', plotOutput("freq", width='800px', height='800px')),
        tabPanel('Graph', value='graph', plotOutput("graph", width='800px', height='800px')),
        tabPanel('Scatter Plot', value='scatter plot', plotOutput("scatter", width='800px', height='800px'))
      ),
    )
  )
))