#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

if(!("shiny" %in% rownames(installed.packages()))) {
  install.packages("shiny")
}
library(shiny)
if(!("ggplot2" %in% rownames(installed.packages()))) {
  install.packages("ggplot2")
}
library(ggplot2)

data(diamonds)
set.seed(112233)

# Define server logic
shinyServer(function(input, output) {
  
  # Filter diamonds dataset based on input criteria
  small_diamonds <- reactive({
    filter_data <- diamonds
    filter_data <- filter_data[sample(nrow(filter_data), input$vsample_size), ]
    filter_data <- filter_data[which(filter_data$cut == input$vcut),]
    filter_data <- filter_data[which(filter_data$color == input$vcolor),]
  })
  
  # Calculate diamond price prediction based on linear model and input carat
  diamond_pred <- reactive({
    diamond_model <- lm(price ~ carat, data = small_diamonds())
    predict(diamond_model, newdata = data.frame(carat = input$vcarat))
  })
  
  # Draw the plot Diamond price vs. carat
  output$diamondsPlot <- renderPlot({
    ggplot(small_diamonds(), aes(x=carat,y=price)) +
      geom_point(color='blue',fill='blue') +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      geom_point(aes(x=input$vcarat, y=diamond_pred()), color='red', fill='red', size = 4)
    
  })
  
  # Return predicted diamond price
  output$vprice_pred <- renderText({
    paste(round(diamond_pred(), digits=2), " $")
  })
  
})
