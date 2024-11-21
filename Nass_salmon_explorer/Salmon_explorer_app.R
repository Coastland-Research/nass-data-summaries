
# Initial setup
library(shiny)
library(tidyverse)
library(rsconnect)

#shinysalmon <- readRDS("objects/salmon.rds") # For local use only, use the code
                                              # below for the shiny app
#shinyexploitation <- readRDS("objects/exploitation.rds")

shinysalmon <- readRDS('salmon.rds')
shinyexploitation <- readRDS('exploitation.rds')

# Part 1 - UI

ui <- fluidPage(
  titlePanel('Nass Salmon Explorer'),
  sidebarLayout(position = "left",
                sidebarPanel("Parameters", 
      
    # User input: Date range         
        sliderInput("RANGE", 
                label = "Date range:",
                min = 1954, max = 2022, value = c(1954, 2022), sep = ""),  
    # User input: Species select
        radioButtons("SPECIES",
                     "Select species:",
                     choices = c("Chinook", "Chum", "Coho", "Pink (odd years)", 
                                 "Pink (even years)", "Sockeye")),
                             ),
        
                    mainPanel(fluidRow(
                              verticalLayout(fluid = TRUE),
                              plotOutput("userselection1"),
                              plotOutput("userselection2")
                              ))
        )
)


# Part 2 - Server

server <- function(input, output, session){
  # Plot 1: escapements by year
  output$userselection1 <- renderPlot({

    shinyexploitation$Proportion <- gsub("TE", "Escaped", 
                                         shinyexploitation$Proportion)
    
    exploitationplot <- shinyexploitation %>%
      filter(`Species Name` == input$SPECIES, 
             Year >= input$RANGE[1] & Year <= input$RANGE[2])
    
    ggplot(exploitationplot, aes(x = Year, y = Fish/100000, 
                                 fill = Proportion)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("gray78", "gray46")) +
      labs(x = "Year", y = "x100,000 Salmon", title = "Area 3 Exploitation by Year") +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
    
    
  })
  
  # Plot 2
  output$userselection2 <- renderPlot({
    
    salmonplot <- shinysalmon %>%
      filter(`Species Name` == input$SPECIES,
             Year >= input$RANGE[1] & Year <= input$RANGE[2])
    
    ggplot(salmonplot, aes(x = `CU Name`, y = Fish/100000)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(y = "x100,000 Salmon", x = "Conservation Unit", title = "Breakdown by CU")
    
  })
  
}

# Part 3 - Run App

shinyApp(ui = ui, server = server)
