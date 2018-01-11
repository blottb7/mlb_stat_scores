pitchers <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 8)
names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
                                           "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")
#all possible stats
#wins, saves, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs
pitchers <- pitchers %>%
  filter(ip > 1) %>%
  mutate(avg_p = round(hits / (3 * ip), 3)) 
