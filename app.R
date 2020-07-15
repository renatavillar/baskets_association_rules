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

server <- shinyServer(function(input, output) {
  transactions <- reactive({
    req(input$file)
    read.transactions(input$file$datapath, format='basket', sep=',')
  })
  rules <- reactive({
    groceries <- apriori(transactions(), parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[5], target="rules"))
    write(groceries, file = "rules.csv", sep = ",", quote=TRUE, row.names=FALSE)
    groceries
  })
  output$freq <- renderPlotly({
    itemFrequencyPlot(transactions, support=0.04, type="relative", xlab='Item name')
    
  })
  
  output$supp <- renderPlot({
    # Support and confidence values
    supportLevels <- c(0.1, 0.05, 0.01, 0.005)
    confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
    
    # Empty integers 
    rules_sup10 <- integer(length=9)
    rules_sup5 <- integer(length=9)
    rules_sup1 <- integer(length=9)
    rules_sup0.5 <- integer(length=9)
    
    # Apriori algorithm with a support level of 10%
    for (i in 1:length(confidenceLevels)) {
      
      rules_sup10[i] <- length(apriori(transactions(), parameter=list(sup=supportLevels[1], 
                                                                    conf=confidenceLevels[i], target="rules")))
      
    }
    
    # Apriori algorithm with a support level of 5%
    for (i in 1:length(confidenceLevels)){
      
      rules_sup5[i] <- length(apriori(transactions(), parameter=list(sup=supportLevels[2], 
                                                                   conf=confidenceLevels[i], target="rules")))
      
    }
    
    # Apriori algorithm with a support level of 1%
    for (i in 1:length(confidenceLevels)){
      
      rules_sup1[i] <- length(apriori(transactions(), parameter=list(sup=supportLevels[3], 
                                                                   conf=confidenceLevels[i], target="rules")))
      
    }
    
    # Apriori algorithm with a support level of 0.5%
    for (i in 1:length(confidenceLevels)){
      
      rules_sup0.5[i] <- length(apriori(transactions(), parameter=list(sup=supportLevels[4], 
                                                                     conf=confidenceLevels[i], target="rules")))
      
    }
    # Data frame
    num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)
    
    # Number of rules found with a support level of 10%, 5%, 1% and 0.5%
    ggplot(data=num_rules, aes(x=confidenceLevels)) +
      
      # Plot line and points (support level of 10%)
      geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
      geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
      
      # Plot line and points (support level of 5%)
      geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
      geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
      
      # Plot line and points (support level of 1%)
      geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
      geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
      
      # Plot line and points (support level of 0.5%)
      geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
      geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
      
      # Labs and theme
      labs(x="Confidence levels", y="Number of rules found", 
           title="Apriori algorithm with different support levels") +
      theme_bw() +
      theme(legend.title=element_blank())
  })
  output$inspect <- renderPrint({
    groceries <- apriori(transactions(), parameter=list(sup=supportLevels[3], conf=confidenceLevels[5], target="rules"))
    inspect(groceries)})
  
  output$freq <- renderPlot({
    itemFrequencyPlot(transactions(), support=0.04, type="relative", xlab='Item name')
  })
  
  output$scatter <- renderPlot({
    supportLevels <- c(0.1, 0.05, 0.01, 0.005)
    confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
    groceries <- apriori(transactions(), parameter=list(sup=input$supp, conf=input$conf, target="rules"))
    plot(groceries, measure=c("support", "lift"), shading="confidence")
  })
  
  output$valuesup <- renderPrint({input$valuesup})
  output$valueconf <- renderPrint({input$confi})
  
  #output$confi <- renderPrint({input$confi})
  
  output$graph <- renderPlot({
    supportLevels <- c(0.1, 0.05, 0.01, 0.005)
    confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
    #rules_sup1_conf50 <- apriori(transactions(), parameter=list(sup=supportLevels[3], 
     #                                                         conf=confidenceLevels[5], target="rules"))
    rules_sup1_conf50 <- apriori(transactions(), parameter=list(sup=input$supp, conf=input$conf, target="rules"))
    plot(rules_sup1_conf50, method="graph")})
  })

shinyApp(ui, server)