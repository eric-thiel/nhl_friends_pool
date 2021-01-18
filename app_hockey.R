### function to put stats online

### add passing attempts and rushing attempts????????????????
### big brain SZN


library("shiny")   
library(DT)
library(googlesheets4)
library(shinycssloaders)
library(dplyr)
library(rdrop2)
library(readr)
library(vroom)
library(tidyverse)


get_player_data = function()
{
  df = vroom::vroom("http://raw.githubusercontent.com/eric-thiel/nhl_friends_pool/master/updated_stats.csv")
  return(df)
}

get_teams = function()
{
  df = vroom::vroom("http://raw.githubusercontent.com/eric-thiel/nhl_friends_pool/master/updated_teams.csv", delim = ",")
  return(df)
}


header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
q = get_player_data()
b = get_teams()
# Encoding(q$Team) <- "UTF-8"
# q$Team = iconv(q$Team, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
team_names = c("All","Brett","Connor","Eric","Jake","Matt","Payton","Sam")


choices = c("Available Players","Current Teams")

#stats_to_choose = c("Targets","Passes Thrown","Runs","Ceilings")


ui = shinyUI(
  pageWithSidebar(
    headerPanel("Friends Fantasy Hockey :)")
    ,
    sidebarPanel(width=2,
                 wellPanel(
                   selectInput("Year", label = h3("Waiver Wire or Current Teams?"),
                               choices =(choices), 
                               hr(), ),
                 ),
                 wellPanel(
                   selectInput("Teams",label = h3("Manager Select"),
                               choices = (team_names),
                               hr(),)
                 ),

                 
    )
    ,
    
    mainPanel(
      DT::dataTableOutput("mytable"),
    )
    
  ))


server = shinyServer(
  function(input,output,session){
    
    df = get_player_data()
    df$PPG = round(df$`Total Points` / df$GP,2)
    df = df %>% rename("Points"="Total Points", "Assists"="Total Assists", "Pos"="Position")
    gf = get_teams()
    
    
 
    gf = gf %>% mutate(
      Player = case_when(
        Player == "Alexis Lafreniere" ~ "Alexis Lafrenière",
        
        
        
        
        
        TRUE ~ as.character(Player)
      )
    )
    
    missing = left_join(gf, df[c("Goals","Player")], by = c("Player"="Player"))
    missing = subset(missing, is.na(Goals))
    
    
    
    available_players_df = left_join(df, gf, by = c("Player"="Player"))
    non_available_players_df = subset(available_players_df, !is.na(available_players_df$Manager))
    available_players_df = subset(available_players_df, is.na(Manager))
    available_players_df$Manager = NULL
    
    missing$Goals = 0
    missing$Team = "???"
    missing$Position ="???"
    missing$`Assists` = 0
    missing$`Points` = 0
    missing$GP = 0
    missing$Points_Per_Game = 0
    
    
    non_available_players_df = rbind(non_available_players_df, missing)
    
    output$mytable = DT::renderDataTable({   
      if(input$Year == "Available Players"){
        joinerino = available_players_df
        joinerino = joinerino %>% arrange(-`Points`)
        datatable(joinerino, selection = "single",class = 'cell-border stripe',
                  options=list(autoWidth = TRUE, rownames = FALSE, pageLength = 25,
                               columnDefs = list(list(visible=FALSE)),
                               className = 'dt-center', targets = "_all"))
        
      }
      else{
        
        df_data = subset(non_available_players_df,non_available_players_df$Manager == input$Teams)
        if(input$Teams == "All"){
          df_data = non_available_players_df
        }
     
        df_data = df_data %>% arrange(-`Points`)
        

        
        
        datatable(df_data, selection = "single",class = 'cell-border stripe',
                  options=list(autoWidth = TRUE, rownames = FALSE, pageLength = 25,
                               columnDefs = list(list(visible=FALSE)),
                               className = 'dt-center', targets = "_all"))
      }
    })
  })


shinyApp(ui = ui, server = server)















