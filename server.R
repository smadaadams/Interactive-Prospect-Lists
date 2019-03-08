library(shiny)
library(DT)
library(tidyverse)

FYPD <- read.csv("data/FYPD2019_test.csv", header=T) %>% 
  mutate(ETA = as.character(ETA))
FYPDrank <- FYPD %>% 
  select(-c(Ralph, Matt))
FYPDrankers <- FYPD %>% 
  select(-c(Age,ETA)) %>% 
  mutate(Matt = as.character(Matt))

Top100 <- read.csv("data/Top_100.csv", header=T) 
Top100rank <- Top100 %>% 
  select(-c(Ralph, Matt, Jason, JP, Eddy, Lance, High, Low)) %>% 
  filter(Rank <101)  %>% 
  mutate(ETA = as.character(ETA))
Top100rankers <- Top100 %>% 
  select(-c(Age, ETA, Grade, Risk)) %>% 
  mutate(Low = as.numeric(replace(Low, Low == 148, ""))) %>% 
  mutate(Ralph = as.numeric(replace(Ralph, Ralph == 148, ""))) %>% 
  mutate(Jason = as.numeric(replace(Jason, Jason == 148, ""))) %>% 
  mutate(JP = as.numeric(replace(JP, JP == 148, ""))) %>% 
  mutate(Eddy = as.numeric(replace(Eddy, Eddy == 148, ""))) %>% 
  mutate(Matt = as.numeric(replace(Matt, Matt == 148, ""))) %>% 
  mutate(Lance = as.numeric(replace(Lance, Lance == 148, "")))
Top100rankF <- NULL

Top30s <- read.csv("data/Top30s.csv", header=T) %>% 
  mutate(ETA = as.character(ETA))
Top30_Links <- read.csv("data/Top30_links.csv", header=T) 


Top100Fantasy <- read.csv("data/Top100Fantasy.csv", header=T) 
Top100Fantasyrank <- Top100Fantasy %>% 
  select(-c(Ralph, Matt, Eddy, High, Low)) %>% 
  filter(Rank <101) %>% 
  mutate(K = as.character(K))
Top100Fantasyrankers <- Top100Fantasy %>% 
  select(-c(Age, ETA, Risk, AVG, OBP, SLG, HR, SB, ERA, WHIP, IP, K)) %>% 
  mutate(Low = as.numeric(replace(Low, Low == 124, ""))) %>% 
  mutate(Ralph = as.numeric(replace(Ralph, Ralph == 124, ""))) %>% 
  mutate(Eddy = as.numeric(replace(Eddy, Eddy == 124, ""))) %>% 
  mutate(Matt = as.numeric(replace(Matt, Matt == 124, ""))) %>% 
  mutate(Eddy = as.character(Eddy))

Top300Fantasy <- read.csv("data/Top300Fantasy.csv", header=T) %>% 
  mutate(ETA = as.character(ETA))

Top100OBP <- read.csv("data/Top100OBPHitters.csv", header=T) %>% 
  mutate(ETA = as.character(ETA))

Top50Redraft <- read.csv("data/Top50Redraft.csv", header=T) %>% 
  mutate(ETA = as.character(ETA))

server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  output$dynamicGradeInput <- renderUI({
    
    if (input$ListSelect == 'Top 100 Prospects') {
      selectInput("GradeFilter", "Grade Filter", 
                              choices = list("All",80,75,70,65,60,55,50,45), 
                             selected = "All")
    } else {
      return(NULL)
    }
    
  })
  
  datasetInput <- reactive({
    switch(input$ListSelect,
           "Top 100 Prospects" = Top100,
           "Top 30 Prospects by Team"= Top30s,
           "First Year Player Draft" = FYPD,
           "Fantasy Top 100 Prospects w/ Projections" = Top100Fantasy,
           "Top 100 Fantasy OBP" = Top100OBP,
           "Fantasy Top 300 Prospects" = Top300Fantasy,
           "Top 50 Prospects for Redraft" = Top50Redraft
           )
  })
  
  output$Headline <- renderText({
    switch(input$ListSelect,
           "Top 100 Prospects" = "Prospect Live's Top 100 Prospects",
           "Top 30 Prospects by Team"= "Prospect Live's Top 30 Prospects by Team",
           "First Year Player Draft" = "Prospect Live's Top 100 for First Year Player Draft",
           "Fantasy Top 100 Prospects w/ Projections" = "Prospect Live's Fantasy Top 100 Prospects",
           "Fantasy OBP Top 100" = "Prospect Live's Fantasy OBP Top 100 Prospects",
           "Fantasy Top 300 Prospects" = "Prospect Live's Fantasy Top 300 Prospects",
           "Top 50 Prospects for Redraft" = "Prospect Live's Top 50 Prospects for Redraft Leagues"
    )
  })
  
  output$Headline2 <- renderText({
    switch(input$ListSelect,
           "Top 100 Prospects" = "Prospect Live's Top 100 Prospects",
           "Top 30 Prospects by Team"= "Team Top 30s were created by one ranker",
           "First Year Player Draft" = "Prospect Live's Top 100 for First Year Player Draft",
           "Fantasy Top 100 Prospects w/ Projections" = "Prospect Live's Fantasy Top 100 Prospects",
           "Fantasy OBP Top 100" = "The Fantasy OBP Top 100 Hitter Prospects were created by one ranker",
           "Fantasy Top 300 Prospects" = "The Fantasy Top 300 Prospects List was developed as a team and is not a composite of multiple lists",
           "Top 50 Prospects for Redraft" = "The Top 50 Prospects for Redraft Leagues was created by one ranker"
    )
  })
  
  output$textlink <- renderText({
    if(input$ListSelect == "Top 30 Prospects by Team"){
      if(input$TeamFilter != "All"){
        Top30_LinksF <- Top30_Links %>% filter(Team == input$TeamFilter)
        paste("Read the full",Top30_LinksF$Team[1],"writeup by",Top30_LinksF$Ranker[1])
      } else {
        "Find all the Top 30 Team articles"
      }
    } else if(input$ListSelect == "Top 100 Prospects") {
      "Read the Top 100 Prospects article"
    } else if(input$ListSelect == "First Year Player Draft"){
      "Read the First Year Player Draft article"
    } else if(input$ListSelect == "Fantasy Top 100 Prospects w/ Projections"){
      "Read the Fantasy Top 100 article"
    # } else if(input$ListSelect == "Fantasy Top 300 Prospects"){
    #   "Read the Fantasy Top 300 article"
    } else if(input$ListSelect == "Fantasy OBP Top 100"){
      "Read the Fantasy Top 100 OBP Hitters article by Eddy Almaguer"
    } else if(input$ListSelect == "Top 50 Prospects for Redraft"){
      "Read the Top 50 Prospects for Redraft Leagues article by Eddy Almaguer"
    }
  })
  
  output$link <- renderUI({
    if(input$ListSelect == "Top 30 Prospects by Team"){
      if(input$TeamFilter != "All"){
        Top30_LinksF <- Top30_Links %>% filter(Team == input$TeamFilter)
        url <- a("HERE", href=Top30_LinksF$Link[1], target="_blank")
        tagList("", url)
      } else {
        url <- a("HERE", href="https://www.prospectslive.com/prospect-lists/", 
                 target="_blank")
        tagList("", url)
      }
    } else if(input$ListSelect == "Top 100 Prospects"){
      url <- a("HERE", href="https://www.prospectslive.com/lists/2019/1/24/the-top-100-prospects-for-2019", 
               target="_blank")
      tagList("", url)
    } else if(input$ListSelect == "First Year Player Draft"){
      url <- a("HERE", href="https://www.prospectslive.com/lists/2018/10/31/2018-first-year-player-draft-rankings-dynasty-leagues-zb3fh", 
               target="_blank")
      tagList("", url)
    } else if(input$ListSelect == "Fantasy Top 100 Prospects w/ Projections"){
      url <- a("HERE", href="https://www.prospectslive.com/lists/2019/1/27/top-100-fantasy-prospects-for-2019", 
               target="_blank")
      tagList("", url)
    # } else if(input$ListSelect == "Fantasy Top 300 Prospects"){
    #   url <- a("HERE", href="", 
    #            target="_blank")
    #   tagList("", url)
    } else if(input$ListSelect == "Fantasy OBP Top 100"){
      url <- a("HERE", href="https://www.prospectslive.com/lists/2019/2/25/top-100-fantasy-prospects-for-obp", 
               target="_blank")
      tagList("", url)
    } else if(input$ListSelect == "Top 50 Prospects for Redraft"){
      url <- a("HERE", href="https://www.prospectslive.com/lists/2019/3/3/top-50-prospects-for-2019-redraft-leagues", 
               target="_blank")
      tagList("", url)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ProspectsLive ", input$ListSelect, ".csv",sep="")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$mainTable = renderDataTable({
    
    if(input$ListSelect == "Top 100 Prospects"){
      
      if(is.null(input$GradeFilter)){
        return()
      } else {
      
      Top100rankF <- Top100rank
      
      if(input$AgeFilter != "All"){
        Top100rankF <- Top100rankF %>% filter(Age == input$AgeFilter)
      }
      if(input$ETAFilter != "All"){
        Top100rankF <- Top100rankF %>% filter(ETA == input$ETAFilter)
      }
      if(input$PositionFilter != "All"){
        Top100rankF <- Top100rankF %>% filter(grepl(input$PositionFilter, Position))
      }
      if(input$TeamFilter != "All"){
        Top100rankF <- Top100rankF %>% filter(Team == input$TeamFilter)
      }
      if(input$GradeFilter != "All"){
        Top100rankF <- Top100rankF %>% filter(Grade == input$GradeFilter)
      }
      
      
      datatable(Top100rankF, filter = 'bottom', rownames = FALSE, 
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  #columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  columnDefs = list(list(width = '40px', className = 'dt-center', targets = c(0,2,3,4,5,6)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )  
      ) %>%   
      formatStyle('Rank', backgroundColor = 'white', fontWeight = 'bold') %>% 
      formatStyle(c('Player','Team','Position','ETA','Grade','Risk','Age'), backgroundColor = 'white')
      }
      
    } else if (input$ListSelect == "First Year Player Draft"){
      
      FYPDrankF <- FYPDrank
      
      if(input$AgeFilter != "All"){
        FYPDrankF <- FYPDrankF %>% filter(Age == input$AgeFilter)
      }
      if(input$ETAFilter != "All"){
        FYPDrankF <- FYPDrankF %>% filter(ETA == input$ETAFilter)
      }
      if(input$PositionFilter != "All"){
        FYPDrankF <- FYPDrankF %>% filter(grepl(input$PositionFilter, Position))
      }
      if(input$TeamFilter != "All"){
        FYPDrankF <- FYPDrankF %>% filter(Team == input$TeamFilter)
      }
      
      datatable(FYPDrankF, filter = 'bottom', rownames = FALSE, 
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '40px', className = 'dt-center', targets = c(0,2,3,4)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle('Rank', backgroundColor = 'white', fontWeight = 'bold') %>% 
        formatStyle(c('Player','Team','Position','ETA','Age'), backgroundColor = 'white')
    } else if (input$ListSelect =="Top 30 Prospects by Team"){
      
      Top30sF <- Top30s
      
      if(input$AgeFilter != "All"){
        Top30sF <- Top30sF %>% filter(Age == input$AgeFilter)
      }
      if(input$ETAFilter != "All"){
        Top30sF <- Top30sF %>% filter(ETA == input$ETAFilter)
      }
      if(input$PositionFilter != "All"){
        Top30sF <- Top30sF %>% filter(grepl(input$PositionFilter, Position))
      }
      if(input$TeamFilter != "All"){
        Top30sF <- Top30sF %>% filter(Team == input$TeamFilter)
      }
      
      datatable(Top30sF, filter = 'bottom', rownames = FALSE, colnames = c('Team Rank' = 'Team.Rank'),
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '80px', className = 'dt-center', targets = c(0)),
                                    list(width = '40px', className = 'dt-center', targets = c(2,3,4)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle('Team Rank', backgroundColor = 'white', fontWeight = 'bold') %>% 
        formatStyle(c('Player','Team','Position','ETA','Age'), backgroundColor = 'white')
    } else if (input$ListSelect == "Fantasy Top 100 Prospects w/ Projections"){
        
        Top100FantasyrankF <- Top100Fantasyrank
        
        if(input$AgeFilter != "All"){
          Top100FantasyrankF <- Top100FantasyrankF %>% filter(Age == input$AgeFilter)
        }
        if(input$ETAFilter != "All"){
          Top100FantasyrankF <- Top100FantasyrankF %>% filter(ETA == input$ETAFilter)
        }
        if(input$PositionFilter != "All"){
          Top100FantasyrankF <- Top100FantasyrankF %>% filter(grepl(input$PositionFilter, Position))
        }
        if(input$TeamFilter != "All"){
          Top100FantasyrankF <- Top100FantasyrankF %>% filter(Team == input$TeamFilter)
        }
        if(input$GradeFilter != "All"){
          Top100FantasyrankF <- Top100FantasyrankF %>% filter(Grade == input$GradeFilter)
        }
        
        datatable(Top100FantasyrankF, filter = 'bottom', rownames = FALSE, 
                  options = list(
                    autoWidth = TRUE, 
                    paging = FALSE,
                    sDom  = '<"top">lrt<"bottom">ip',
                    columnDefs = list(list(width = '20px', className = 'dt-center', targets = c(0,2,3,4,5,6,7,8,9,10,11,12,13,14)), #'IP','K','ERA','WHIP'
                                      list(width = '135px', targets = c(1))),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  )  
        ) %>%   
          formatStyle('Rank', fontWeight = 'bold', backgroundColor = 'white') %>% 
         
          formatStyle(c('Player','Position','Team','ETA','Risk','Age',
                     'AVG','OBP','SLG','HR','SB','IP','K','ERA','WHIP'), backgroundColor = 'white') %>%  #
          formatRound(columns=c('AVG', 'OBP','SLG'), digits=3) %>% 
          formatRound(columns=c('ERA', 'WHIP'), digits=2) 
    } else if (input$ListSelect == "Fantasy OBP Top 100"){
      
      Top100OBPF <- Top100OBP
      
      if(input$AgeFilter != "All"){
        Top100OBPF <- Top100OBPF %>% filter(Age == input$AgeFilter)
      }
      if(input$ETAFilter != "All"){
        Top100OBPF <- Top100OBPF %>% filter(ETA == input$ETAFilter)
      }
      if(input$PositionFilter != "All"){
        Top100OBPF <- Top100OBPF %>% filter(grepl(input$PositionFilter, Position))
      }
      if(input$TeamFilter != "All"){
        Top100OBPF <- Top100OBPF %>% filter(Team == input$TeamFilter)
      }
      
      datatable(Top100OBPF, filter = 'bottom', rownames = FALSE,
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '80px', className = 'dt-center', targets = c(0)),
                                    list(width = '40px', className = 'dt-center', targets = c(2,3,4)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle(c('Rank','Player','Team','Position','ETA','Age'), backgroundColor = 'white')
    } else if (input$ListSelect == "Fantasy Top 300 Prospects"){
      
      Top300FantasyF <- Top300Fantasy
      
      if(input$AgeFilter != "All"){
        Top300FantasyF <- Top300FantasyF %>% filter(Age == input$AgeFilter)
      }
      if(input$ETAFilter != "All"){
        Top300FantasyF <- Top300FantasyF %>% filter(ETA == input$ETAFilter)
      }
      if(input$PositionFilter != "All"){
        Top300FantasyF <- Top300FantasyF %>% filter(grepl(input$PositionFilter, Position))
      }
      if(input$TeamFilter != "All"){
        Top300FantasyF <- Top300FantasyF %>% filter(Team == input$TeamFilter)
      }
      
      datatable(Top300FantasyF, filter = 'bottom', rownames = FALSE,
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '80px', className = 'dt-center', targets = c(0)),
                                    list(width = '40px', className = 'dt-center', targets = c(2,3,4)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle(c('Rank','Player','Team','Position','ETA','Age'), backgroundColor = 'white')
    } else if (input$ListSelect == "Top 50 Prospects for Redraft"){
      
      Top50RedraftF <- Top50Redraft
      
      if(input$AgeFilter != "All"){
        Top50RedraftF <- Top50RedraftF %>% filter(Age == input$AgeFilter)
      }
      if(input$ETAFilter != "All"){
        Top50RedraftF <- Top50RedraftF %>% filter(ETA == input$ETAFilter)
      }
      if(input$PositionFilter != "All"){
        Top50RedraftF <- Top50RedraftF %>% filter(grepl(input$PositionFilter, Position))
      }
      if(input$TeamFilter != "All"){
        Top50RedraftF <- Top50RedraftF %>% filter(Team == input$TeamFilter)
      }
      
      datatable(Top50RedraftF, filter = 'bottom', rownames = FALSE,
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '80px', className = 'dt-center', targets = c(0)),
                                    list(width = '40px', className = 'dt-center', targets = c(2,3,4)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle(c('Rank','Player','Team','Position','ETA','Age'), backgroundColor = 'white')
    }
  })
  
  output$rankerTable = renderDataTable({
    
    if(input$ListSelect == "Top 100 Prospects"){
      
      datatable(Top100rankers, filter = 'bottom', rownames = FALSE, 
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '40px', className = 'dt-center', targets = c(0,2,3,4,5,6,7,8,9,10)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle('Rank', backgroundColor = 'white', fontWeight = 'bold') %>% 
        formatStyle(c('Player','Team','Position','Lance','Jason','JP','Ralph','Matt','Eddy','High','Low'), backgroundColor = 'white')
      
    } else if (input$ListSelect == "First Year Player Draft"){
      
      datatable(FYPDrankers, filter = 'bottom', rownames = FALSE, 
                options = list(
                  autoWidth = TRUE, 
                  paging = FALSE,
                  sDom  = '<"top">lrt<"bottom">ip',
                  columnDefs = list(list(width = '40px', className = 'dt-center', targets = c(0,2,3,4)),
                                    list(width = '135px', targets = c(1))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
      ) %>%   
        formatStyle('Rank', backgroundColor = 'white', fontWeight = 'bold') %>% 
        formatStyle(c('Player','Team','Position','Ralph','Matt'), backgroundColor = 'white')
      
    } else if (input$ListSelect =="Team Top 30s"){
      
        NULL
      
    } else if (input$ListSelect == "Fantasy Top 100 Prospects w/ Projections"){

      datatable(Top100Fantasyrankers, filter = 'bottom', rownames = FALSE, 
               options = list(
                 autoWidth = TRUE, 
                 paging = FALSE,
                 sDom  = '<"top">lrt<"bottom">ip',
                 columnDefs = list(list(width = '40px', className = 'dt-center', targets = c(0,2,3,4,5,6,7)),
                                   list(width = '135px', targets = c(1))),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                   "}")
               )
      ) %>%   
        formatStyle('Rank', backgroundColor = 'white', fontWeight = 'bold') %>% 
        formatStyle(c('Player','Team','Position','Ralph','Matt','Eddy','High','Low'), backgroundColor = 'white')
      
      
    } else if (input$ListSelect == "Top 100 Fantasy OBP"){
      
      NULL
      
    }
  })
}