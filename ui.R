#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application
shinyUI(navbarPage("Predict Diamond Price",
  tabPanel("Run the Application",    
    
    # Sidebar with several input variables              
    sidebarPanel(
      
       width = 5,
       h4("Select Diamonds Input Criteria"),
       
       sliderInput("vsample_size",
                  "How big the sample size should be?",
                  min = 1000,
                  max = 10000,
                  value = 5000),
       
       selectInput("vcut", label="Select quality of the cut:", multiple=TRUE, selectize=FALSE,
                     choices=levels(diamonds$cut), selected=levels(diamonds$cut)),
      
       selectInput("vcolor", label="Pick a color:", multiple=FALSE, selectize=FALSE,
                   choices=levels(diamonds$color), selected=levels(diamonds$color)),
       
       hr(),
       h4("Select Diamonds Predictor"),
       sliderInput("vcarat",
                   "Weight of the diamond (carat):",
                   min = 0.2,
                   max = 3.0,
                   value = 1.0)
    ),
    
    # Show a plot of the diamonds dataset based on selected input values
    mainPanel(
       width = 7,
       h3("Diamond price vs. carat"),
       plotOutput("diamondsPlot"), 
       h3("Diamond price prediction from linear model:"),
       #textOutput("vprice_pred")
       span(textOutput("vprice_pred"), style="color:red")
    )
  ),
  
  # Information on how to use the application
  tabPanel("Documentation",
    mainPanel(
       h3("Documentation about the application"),
       h4("Goal"),
       p("The goal of the application is to predict a diamond price using a prediction model trained on the 'diamonds' dataset "),
    
       h4("Utilization"),
       HTML("The application is splitted in two parts:
            <ul>
            <li>the left side contains the input parameters that can be selected</li>
            <li>the right side contains the output generated by the selected inputs</li>
            </ul>
            The input parameters are divided in two types:
            <ul>
            <li>input criteria allow to filter the initial diamonds dataset by the following:
              <ul>
              <li>sample size. You are allowed to select a random sample having the size between 1 and 10000. Initial diamonds dataset size is around 54000, however for performance reasons we restrict the sample size to 19% of the original dataset.</li>
              <li>quality of the cut. You can filter the quality of the diamond cut by selecting one or multiple values between the following: Fair, Good, Very Good, Premium, Ideal</li>
              <li>color. You can choose one color to filter the diamonds dataset. The possible values are from J (worst) to D (best).</li>
              </ul>	
            </li>
            <li>the predictor allows to make a prediction for the price of a diamond based on the linear model</li>
            </ul>
            The linear model is displayed a red line in the 'Diamond price vs. carat' plot.<br>
            The prediction depending on the input carat value is represented on the plot by a red point a little bit bigger than the other points.<br>
            In addition the diamond price prediction exact value is showed under the plot in red text.<br>
            Depending on the selection you make on either the filter criteria or the predictor variable, the plot and the prediction price is updated in real time.<br>
            "),
       
       h4("Related Links"),
       p("The correspondent R presentation is available at:"),
       a(href = "https://icatana.github.io/DDP-Course-Project/diamonds_price_pred.html",
         "https://icatana.github.io/DDP-Course-Project/diamonds_price_pred.html",
         target = "_blank"),
       
       br(),br(),       
       p("The code is available at the following GitHub link:"),
       a(href = "https://github.com/icatana/DDP-Course-Project",
         "https://github.com/icatana/DDP-Course-Project",
         target = "_blank"),  
       
       br(),br(),
       p("This Shiny application is part of the Developing Data Products Coursera course project:"),
       a(href = "https://www.coursera.org/learn/data-products/",
         "https://www.coursera.org/learn/data-products/",
         target = "_blank"),
       br(),br(),
       
       p("More information about the diamonds datase can be found here:"),
       a(href = "https://ggplot2.tidyverse.org/reference/diamonds.html",
         "https://ggplot2.tidyverse.org/reference/diamonds.html",
         target = "_blank"),  
       
       br(),br(),       
       h4("Created by Ioan Catana"),
       h4("July 30, 2018")
    )             
  )  
  
))
