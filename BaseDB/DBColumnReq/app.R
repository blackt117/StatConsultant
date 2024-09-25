#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# Install required packages
# install.packages("shiny")
# install.packages("readxl")

library(readxl)  # For reading Excel files

# Define the UI
ui <- fluidPage(
  titlePanel("Upload CSV or Excel File"),
  
  fluidRow(
    # File upload input
    column(6,
           fileInput("file", HTML("Choose CSV or Excel File<br>Remember, there must be at
                     least two numerical and two categorical columns (data types)<br>
                                  Also there must be a discrete numerical and continuous
                                  numerical varible"),
                     accept = c(".csv", ".xlsx")),
    ),
    #Display Results
    column(6, 
           textOutput("textcontents"))
  ), 
  fluidRow(
    # Display the data table
    column(12,
           tableOutput("contents"))
  )
)

# Define the server
server <- function(input, output) {
  output$contents <- renderTable({
    # Check if a file is uploaded
    req(input$file)
    
    # Determine file extension and read the data accordingly
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      data <- read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      data <- read_excel(input$file$datapath)
    } else {
      stop("Invalid file type. Please upload a .csv or .xlsx file.")
    }
    
    return(data)  # Display the uploaded data
  })
  output$textcontents <- renderText({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      data <- read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      data <- read_excel(input$file$datapath)
    } else {
      stop("Invalid file type. Please upload a .csv or .xlsx file.")
    }
    #Finding count for numeric and categorical
    count_numeric <- length(select_if(data,is.numeric)) 
    count_cat <- length(select_if(data,is.character))
    
    #Finding count for discrete and continuous
    
    dis_count <- 0
    cont_count <- 0
    for (i in colnames(data)){
      if (is.numeric(data[[i]])) {
        # Continuous if decimal (essentially)
        if ((all(data[[i]]==round(data[[i]])) == FALSE)) 
          {
          cont_count <- cont_count + 1
        } else {
          dis_count <- dis_count + 1
        }
      }
    }
    
    
    
    
    #Checking the Requirements and Returning the Information
    if (count_numeric < 2 & count_cat < 2){
      if(dis_count < 1 & cont_count < 1){
        return("There must be at least two categorical and numerical columns. Additionally,
               there must be a discrete variable and a continuous variable")
      }else if (dis_count < 1){
        return("There must be at least two categorical and numerical columns. Additionally 
        there must be a discrete variable")
      }else if (cont_count < 1){
        return("There must be at least two categorical and numerical columns. Additionally 
        there must be a continuous variable")
      }else{
        return("There must be at least two categorical and numerical columns.")
      }
     
    } else if (count_numeric < 2){
      if(dis_count < 1 & cont_count < 1){
        return("There must be at least two numerical columns. Additionally,
               there must be a discrete variable and a continuous variable")
      }else if (dis_count < 1){
        return("There must be at least two numerical columns. Additionally 
        there must be a discrete variable")
      }else if (cont_count < 1){
        return("There must be at two least numerical columns. Additionally 
        there must be a continuous variable")
      }else{
        return("There must be at least two numerical columns.")
      }
    }else if (count_cat < 2){
      if(dis_count < 1 & cont_count < 1){
        return("There must be at least two categorical columns. Additionally,
               there must be a discrete variable and a continuous variable")
      }else if (dis_count < 1){
        return("There must be at least two categorical columns. Additionally 
        there must be a discrete variable")
      }else if (cont_count < 1){
        return("There must be at least categorical columns. Additionally 
        there must be a continuous variable")
      }else{
        return("There must be at least categorical columns.")
      }
    }else {
      return('The requirements have been satisifed')  
    }
  }
    
  )
}

# Run the Shiny app
shinyApp(ui, server)
