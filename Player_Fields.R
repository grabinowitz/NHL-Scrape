#Calculate: Cumulative Stats, League Averages,
Master_Table <-
  Extract %>% 
  mutate(Game = 1) %>% 
  group_by(Season, Team, Player) %>% 
  mutate(Cumulative_Goals = cumsum(G),
         Cumulative_Points = cumsum(PTS),
         Cumulative_Game = cumsum(Game),
         Cumulative_PIM = cumsum(PIM),
         Cumulative_Shots = cumsum(S),
         G_p_Gm = sum(G)/n(),
         A_p_Gm = sum(A)/n(),
         Pts_p_Gm = sum(PTS)/n(),
         PIM_p_Gm = sum(PIM)/n(),
         Shots_p_Gm = sum(S)/n()) %>% 
  select(-Game) %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  mutate(G_Lg_Avg = sum(G)/n(),
         A_Lg_Avg = sum(A)/n(),
         Pts_Lg_Avg = sum(PTS)/n(),
         PIM_Lg_Avg = sum(PIM)/n(),
         Cumul_PPG = Cumulative_Points/Cumulative_Game,
         Cumul_GPG = Cumulative_Goals/Cumulative_Game,
         Cumul_SPG = Cumulative_Shots/Cumulative_Game
  ) %>% 
  mutate(Rel_Goals   = G_p_Gm/G_Lg_Avg,
         Rel_Assists = A_p_Gm/A_Lg_Avg,
         Rel_PIM     = PIM_p_Gm/PIM_Lg_Avg)




#Calculate each Player's Proportion of Team's Points by game
  #Team x Game Calcs
Master_Table <- 
  Master_Table %>% 
  group_by(Season, Team, Date) %>% 
  mutate(Team_Goals_Game = sum(G)) %>% 
  ungroup()



  #Player x Season Calcs
Master_Table <-
  Master_Table %>% 
  group_by(Season, Team, Player) %>% 
  mutate(Pts_Goals_Prop_Season = sum(PTS)/sum(Team_Goals_Game),
         Season_Goals = sum(G),
         Season_Points = sum(PTS),
         Season_Games = n()) %>% 
  ungroup() 
  
  #Final Calcs
Master_Table <-
  Master_Table %>% 
  mutate(Pts_Goals_Prop_Game = ifelse(Team_Goals_Game == 0, 0, PTS/Team_Goals_Game))


group_by(Season, Team, Player) %>% 
  mutate(Goals =  sum(G), 
         Games = sum(ifelse(G >= 0, 1, 0)))
ggplot(data = Test %>% filter(Player == "Sidney Crosby"), 
       aes(x = Cumulative_Game, y = Pts_Goals_Prop, color = Season)) +
  geom_point()


  

