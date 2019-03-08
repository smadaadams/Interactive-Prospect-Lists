
library(shiny)
library(DT)
library(tidyverse)
  
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(type='text/css', ".my_class .selectize-input { font-size: 20px; line-height: 20px;} .my_class .selectize-dropdown { font-size: 20px; line-height: 40px; }"),
  tags$div(id='my_div', class='my_class',
    selectInput("ListSelect", "Select List:", 
              choices = list("Top 100 Prospects", "Top 30 Prospects by Team", "Fantasy Top 300 Prospects", "Fantasy Top 100 Prospects w/ Projections", 
                             "Top 50 Prospects for Redraft", "First Year Player Draft", "Fantasy OBP Top 100" ),  
              selected = "Top 100 Prospects", width="450px")
  ),
  downloadButton("downloadData", "Download"),
  br(),
  br(),
  tabsetPanel(
    tabPanel(tags$b("Player List"),
      tags$b(tags$i(tags$h3(textOutput("Headline")))),
      div(style="display: inline-block;",tags$b(tags$i(textOutput("textlink")))),
      div(style="display: inline-block;",tags$b(tags$i(uiOutput("link")))),
      br(),
      br(),
      # div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("GradeFilter", "Grade Filter", 
      #            choices = list("All",80,75,70,65,60,55,50,45), 
      #            selected = "All")),
      div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("TeamFilter", "Team Filter", 
                 choices = list("All","A's","Angels","Astros","Blue Jays","Braves","Brewers","Cardinals",
                               "Cubs","Diamondbacks","Dodgers","Giants","Indians","Mariners","Marlins","Mets",
                               "Nationals","Orioles","Padres","Phillies","Pirates","Rangers","Rays","Red Sox","Reds",
                               "Rockies","Royals","Tigers","Twins","White Sox","Yankees"), 
                 selected = "All")),
      div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("PositionFilter", "Position Filter", 
                                                                                      choices = list("All","C","1B","2B","SS","3B","OF","RHP","LHP"), 
                                                                                      selected = "All")),
      div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("AgeFilter", "Age Filter", 
                 choices = list("All",16,17,18,19,20,21,22,23,24,25,26,27,28,30), 
                 selected = "All")),
      div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("ETAFilter", "ETA Filter", 
                  choices = list("All",2018,2019,2020,2021,2022,2023,2024), 
                  selected = "All")),
      div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput(outputId = 'dynamicGradeInput')),

      br(),
      column(12,
        dataTableOutput("mainTable")
      )
    ),
    tabPanel(tags$b("Ranker Breakdown"),
       tags$b(tags$i(tags$h3(textOutput("Headline2")))),
             br(),
       column(12,
              dataTableOutput("rankerTable")
       )
    )
  )
)