# ui.R
# author: Iga Korneta
# version: 1.0
# created: 08/24/2014
# visualisation of the 2014 West Africa Ebola outbreak: user interface

library(shiny)

shinyUI(fluidPage(
  titlePanel("The 2014 West Africa Ebola Outbreak"),  
  sidebarLayout(
    sidebarPanel(
      p("Visualise the 2014 West Africa Ebola outbreak."),
      p("Data source:",a(href="https://en.wikipedia.org/wiki/Ebola_virus_epidemic_in_West_Africa", "Wikipedia")),
      p("Author: Iga Korneta"),
      p("Date: Aug 24, 2014"),
      p("Code:", a(href="https://github.com/ikorneta/ebola", "Github")),
      p("Contact: iga.korneta@gmail.com"),
      br(),
      br(),
      uiOutput("slider"),
      br(),
      selectInput("cum.new", 
                  label="Visualise new/cumulative cases?",
                  choices=list("Average new", "Cumulative"), selected="Cumulative")      
    ),
    
    
    mainPanel(
      #tableOutput("total.cases"),
      plotOutput("cases.map"),
      plotOutput("deaths.map")
    )
  )
))
