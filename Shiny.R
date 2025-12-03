#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Use needed libraries
#install.packages("shiny")
library(shiny)
library(ggplot2)
library(summarytools)
#install.packages("readxl")  # Install readxl package
library(readxl) # load readxl package

# Set the working directory to where file is contained

Spain <- read_excel("STAT8010_CA2_2024-cleaned.xlsx", sheet = "Sheet1") # Assign each sheet to a Data frame (using cleaned by Justin's clean set to be sure)
# Spain <- read_excel("STAT80010_CA2_2024-cleaned_by_Quentin.xlsx", sheet = "Sheet1") # In case you want to try my clean set
Spain$Date <-substring(Spain$Date, 1, 10) # Reformatting Date categories to have less values (Making app more user friendly)

# Use clean data set
dataset <- Spain
# Create a vector of colours
colors <- c("black", "blue","red","yellow","green","orange","purple","cyan","brown","grey","violet","pink","seagreen", "peru", "chartreuse", "turquoise")


# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Spain Fuel Sales"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Dropdown lists for the variables to plot and factor to color
      selectInput('x', 'X', names(dataset[-c(1)]), names(dataset)[[2]]),
      selectInput('y', 'Y', names(dataset[-c(1)]), names(dataset)[[1]]),
      selectInput('z', 'Factor to color', names(dataset[-c(1,5,6,7,8)]), names(dataset)[[1]]),
      
      # Checkbox for regression line option
      checkboxInput("line", "Show Scatterplot line")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Tabset with plots, text and Histograms
      tabsetPanel(type = "tabs",
                  # Panel for Plot and messages if needed, defining dimensions for better readablity (feel free, to amend or remove height and width to fit your screen)
                  tabPanel("Plot", style = "color: red;",textOutput("warn"),textOutput("warn4"),textOutput("warn5"), plotOutput("plotxy",height = "800px", width = "1200px"), verbatimTextOutput("lm")),
                  # Panel for Summary details
                  tabPanel("Summary Details", textOutput("sumdatamess"),verbatimTextOutput("sumdata")),
                  # Panel for Histograms and messages (Splitting in 2 colums for side by side comparison)
                  tabPanel("Histograms",
                           fluidRow(textOutput("warn3"),
                             splitLayout(cellWidths = c("50%", "50%"), textOutput("warn1"), textOutput("warn2")),
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("histx"), plotOutput("histy"))
                           ))
      
      )
    )
  )
)

# Define server logic for app
server <- function(input, output) {
  # Displaying warnings in case identical variables or inverting variable to generate boxplot
  output$warn <- renderText({
    if (input$x == input$y){
      "Please do not choose identical variables for x and y"
      }
    else if(typeof(dataset[[input$x]])=="double" && typeof(dataset[[input$y]])=="character"){
      "Please intervert x and y to generate a boxplot"
      }
    })
  
  # Display a warning in case line feature cannot be applied on plot (Only for scatterplots)
  output$warn4 <- renderText({
    if(typeof(dataset[[input$x]])!="double" && typeof(dataset[[input$y]])!="double" && input$line == TRUE){
      "Fitting line feature cannot be applied to barplots"
    }
  })
    
    # Display a warning in case factor to color feature cannot be applied on plot (Only for scatterplots, and boxplots per requirements)
    output$warn5 <- renderText({
      if(typeof(dataset[[input$x]])=="character" && typeof(dataset[[input$y]])=="character"){
        "Factor to color feature cannot be applied on barplots"
      }

  })
  
  # Plot type based on variable types
  output$plotxy <- renderPlot({
    fact <- factor(dataset[[input$z]], labels = unique(dataset[[input$z]]))
    
    # If Both variables are numeric -> Scatterplot
    if(typeof(dataset[[input$x]])=="double" && typeof(dataset[[input$y]])=="double"){
      plot(dataset[input$x][[1]],dataset[input$y][[1]],
           xlab = paste(input$x, "prices in €"),
           ylab= paste(input$y, "prices in €"),
           main = paste("Scatterplot of", input$y, "vs", input$x),
           col=colors[fact],
           pch = 19)
      legend("topleft", legend = unique(dataset[[input$z]]),
             col=colors,
             pch = 19,
             cex=1)
      # If line box is checked, draw regression line and give Equation
      if(input$line == TRUE){
        model = lm(dataset[input$y][[1]] ~ dataset[input$x][[1]])
        abline(lm(model),lwd=4)
        mtext(paste("Equation: ", input$y , " = ", coef(model)[1], " + ", coef(model)[2], input$x), col = "red")
      }
    }
    
    # If Both variables are character -> Barplot
    else if(typeof(dataset[[input$x]])=="character" && typeof(dataset[[input$y]])=="character"){
      
      barplot(table(dataset[[input$y]],dataset[[input$x]]),
        xlab = input$x, ylab=input$y,
        main = paste("Barplot of", input$y, "vs", input$x),
        beside = TRUE,
        col=colors[1:length(unique(dataset[[input$y]]))],
        legend = rownames(table(dataset[[input$y]],dataset[[input$x]])),
        args.legend = list(x = "topleft"))
    }
    # If one variable is numeric and the other character -> Boxplot
    else{
      
      if(typeof(dataset[[input$x]])=="character" && input$line == TRUE){
        
        ggplot(dataset, aes(x=dataset[input$x][[1]], y=dataset[input$y][[1]], fill = fact)) + 
          labs(title = paste("Boxplot of", input$y, "vs", input$x),
               subtitle = "Fitting line feature cannot be applied to boxplots",
               x= paste(input$x),
               y= paste(input$y,"prices in €"),
               fill = input$z) +
          geom_boxplot() +
          theme(legend.position = "right",
                legend.title = element_text(size=20),
                legend.text = element_text(size=15),
                axis.text = element_text(size = (10)),
                plot.subtitle = element_text(color = "red"))
      }
      else if(typeof(dataset[[input$x]])=="character"){
        ggplot(dataset, aes(x=dataset[input$x][[1]], y=dataset[input$y][[1]], fill = fact)) + 
          labs(title = paste("Boxplot of", input$y, "vs", input$x),
               x= paste(input$x),
               y= paste(input$y,"prices in €"),
               fill = input$z) +
          geom_boxplot() +
          theme(legend.position = "right",
                legend.title = element_text(size=20),
                legend.text = element_text(size=15),
                axis.text = element_text(size = (10)))
      }
    }
  })
  
  # If line feature is activated on scatterplot, give details of linear model
  output$lm <- renderPrint(
    if(typeof(dataset[[input$x]])=="double" && typeof(dataset[[input$y]])=="double"){
      if(input$line == TRUE){
        Y <- dataset[input$y][[1]]
        X <- dataset[input$x][[1]]
        lm(Y ~ X)
      }
    }
  )
  
  # Summary tab
  # Display a welcom message
  output$sumdatamess <- renderText({
    "Please find the summary details of the dataset below:"
  })
  
  # Render a summary of descriptive stats
  output$sumdata <- renderPrint({
    dfSummary(dataset[2:12], varnumbers = FALSE,graph.col = FALSE)
  })
  
  # Histograms, display warning in case no numerical data has been chosen for 1st hist
  output$warn1 <- renderText({
    if (typeof(dataset[[input$x]])!="double"){
      "Please choose a numerical variable (any fuel type)"
    }
  })
  
  # Histograms, display warning in case no numerical data has been chosen for 2nd hist
  output$warn2 <- renderText({
    if (typeof(dataset[[input$y]])!="double"){
      "Please choose a numerical variable (any fuel type)"
    }
  })
  
  # Display reminder features are not applicable in this tab (per requirement)
  output$warn3 <- renderText({
      "Please keep in mind line fitting and coloring factors cannot be applied on those historgrams"
  })
  
  # Plot 1st hist
  output$histx <- renderPlot({
    if ((typeof(dataset[[input$x]])=="double")){
    hist(dataset[input$x][[1]], 
         xlab = input$x, 
         main = paste("Histogram of", input$x),
         col = "blue")
      }
    })
  
  # Plot 2nd hist
   output$histy <- renderPlot({
     if ((typeof(dataset[[input$y]])=="double")){
       hist(dataset[input$y][[1]], 
            xlab = input$y, 
            main = paste("Histogram of", input$y),
            col = "red")
     }
  })
  
}

#generate app
shinyApp(ui, server)
