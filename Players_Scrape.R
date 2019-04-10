#Use Hockey Reference to find total shots on a given date
library(rvest)
library(XML)
library(tidyverse)
library(data.table)
library(ggplot2)


all.teams <-
  c("Anaheim Ducks", "Arizona Coyotes","Boston Bruins","Buffalo Sabres","Calgary Flames","Carolina Hurricanes",
    "Chicago Blackhawks","Colorado Avalanche","Columbus Blue Jackets","Dallas Stars","Detroit Red Wings","Edmonton Oilers",
    "Florida Panthers","Los Angeles Kings","Minnesota Wild","Montreal Canadiens","Nashville Predators","New Jersey Devils",
    "New York Islanders","New York Rangers","Ottawa Senators","Philadelphia Flyers","Pittsburgh Penguins","San Jose Sharks",
    "St. Louis Blues","Tampa Bay Lightning","Toronto Maple Leafs","Vancouver Canucks","Vegas Golden Knights",
    "Washington Capitals","Winnipeg Jets"
  )


seasons <- 

  as.Date(paste0(substr(Master_Table$Date, 1, 4), "/", 
         substr(Master_Table$Date, 5, 6), "/", 
         substr(Master_Table$Date, 7, 8)
         ))  
  
  
all.teams.list <- as.list(all.teams)

##### INPUTS ##############
Start_Date = "2019-03-01" 
End_Date   = "2019-03-08"   
###########################

#Create list of Dates between start_date and end_date
getDateList <- function(start_date, end_date){
  temp_list <- list()
  temp_dates <- seq(as.Date(start_date), as.Date(end_date), "days")
  
  temp_list <- list()
  for(i in 1:length(temp_dates)){
    temp_list[[i]] =
      data.frame(Year = substr(temp_dates[i], 1, 4),
                 Month = substr(temp_dates[i], 6, 7),
                 Day = substr(temp_dates[i], 9, 10))
    names(temp_list)[i] <- paste0(substr(temp_dates[i], 1, 4), 
                                  substr(temp_dates[i], 6, 7),
                                  substr(temp_dates[i], 9, 19) 
    )
  }
  return(temp_list)
}




#Write the link to boxscores of all games on each date between start_date and end_date
getDateLinks <- function(x){
  cbind(x, "Link" = paste0("https://www.hockey-reference.com/boxscores/?",
                           "year=",x[["Year"]],
                           "&month=",x[["Month"]],
                           "&day=",x[["Day"]])
  )
  
}



#get link for each boxscore of each game of each day between start_date and end_date
getGameLinks <- function(x){
  links_temp <-
    html_attr(html_nodes(read_html(as.character(x[["Link"]])), "a"), "href") %>% 
    as_tibble() %>% 
    filter(grepl(paste0("/boxscores/",x[["Year"]], 
                        x[["Month"]], 
                        x[["Day"]]), value)==TRUE) %>% 
    as.data.frame()
}


getStats <- function(x){
  boxscore = paste0("https://www.hockey-reference.com", x)
  
  #Get all tables
  players_extract <-
    boxscore %>% 
    read_html() %>% html_nodes(xpath = "//table")
  

  #Get Team Names
  teams_extract <-
    boxscore %>% 
    read_html() %>% html_nodes(xpath = "//div[contains(@class, 'section_heading')]/h2") %>% 
    html_text() %>%
    as_tibble()
  
  #Keep each team and add column for Team Name & Opponent
  stats_t1  <- html_table(players_extract[[3]])
  colnames(stats_t1) <- gsub("Scoring", "", paste0(colnames(stats_t1), stats_t1[1,]))
  stats_t1 = stats_t1[2:(nrow(stats_t1)-1), ]
  
  
  
  
  stats_t2  <- html_table(players_extract[[5]])  
  colnames(stats_t2) <- gsub("Scoring", "", paste0(colnames(stats_t2), stats_t2[1,]))
  stats_t2 = stats_t2[2:(nrow(stats_t2)-1), ]  
  
  
  stats_t1$Team <- c(rep(slice_(teams_extract, 3)[[1]], nrow(stats_t1)))
  stats_t1$Opp  <- c(rep(slice_(teams_extract, 5)[[1]], nrow(stats_t1)))
  stats_t1$Loc  <- c(rep("Away", nrow(stats_t1)))
  stats_t1$Game <- c(rep(paste0(stats_t1$Team, " @ ", stats_t1$Opp)))
  
  
  stats_t2$Team <- c(rep(slice_(teams_extract, 5)[[1]], nrow(stats_t2)))
  stats_t2$Opp  <- c(rep(slice_(teams_extract, 3)[[1]], nrow(stats_t2)))
  stats_t2$Loc  <- c(rep("Home", nrow(stats_t2)))
  stats_t2$Game <- c(rep(paste0(stats_t2$Opp, " @ ", stats_t2$Team)))
  
  stats_table <- rbind(stats_t1, stats_t2)

}


GetStatsTable <- function(){
  
  links_out <- lapply(lapply(getDateList(Start_Date, End_Date), getDateLinks), getGameLinks)
  
  final_list <- list()
  for(i in 1:length(links_out)){
    if(length(links_out[[i]][,]) ==0) next
    final_list[[i]] <- lapply(links_out[[i]][,], getStats)
    final_list[[i]] <- do.call(rbind, final_list[[i]])
    final_list[[i]]$Date <- names(links_out)[i]
    names(final_list)[i] <- names(links_out)[i]
  }
  
  final_shots <- do.call(rbind, final_list)
  return(final_shots)
}


Extract <- GetStatsTable()

#Convert Character to Numeric
cols <- c("G","A", "PTS","+/-","PIM",
          "GoalsEV","GoalsPP","GoalsSH","GoalsGW",
          "AssistsEV","AssistsPP","AssistsSH","S")  

for(col in cols){
  Extract[[col]] <- as.numeric(Extract[[col]])
}

#Turn TOI to numeric
Extract$TOI <- as.numeric(gsub(":", ".", Extract$TOI))
Extract$TOI <- Extract$TOI + (Extract$TOI - floor(Extract$TOI))*100/60

#Format Date and Add Season
Extract <-
  Extract %>% 
#  mutate(Date = as.Date(paste0(substr(Date, 1, 4), "/",
#                               substr(Date, 5, 6), "/", 
#                               substr(Date, 7, 8)))) %>% 
  mutate(Season = ifelse(Date < "2016-04-11", "Regular_1516",
                         ifelse(Date < "2016-10-12", "Playoffs_1516",
                         ifelse(Date < "2017-04-10", "Regular_1617",
                         ifelse(Date < "2017-10-04", "Playoffs_1617",
                         ifelse(Date < "2018-04-09", "Regular_1718",
                         ifelse(Date < "2018-10-03", "Playoffs_1718",
                         "Regular_1819")))))))


write_csv(Master_Table, "/Users/katerinalatour/Documents/JV/NHL/Scrape/NHL_15to19.csv")
Master_Table <- read_csv("/Users/katerinalatour/Documents/JV/NHL/Scrape/NHL_15to19.csv")



  
ggplot(Master_Table %>% 
         filter(str_detect(Season, 'Regular')) %>% 
         filter(Player == "Auston Matthews"), 
       aes(x = Cumulative_Game, y = Cumulative_Goals, color = Season)) + 
  geom_line()


ggplot(Master_Table %>% 
         filter(str_detect(Season, 'Regular')) %>% 
         group_by(Player, Season) %>% 
         summarise(Goals = max(Cumulative_Goals),
                   Points = max(Cumulative_Points)), 
       aes(x = Goals, color = Season)) +
  geom_density()





