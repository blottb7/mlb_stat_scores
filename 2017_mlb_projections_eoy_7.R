pitchers <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 8)
names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
                     "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")
#all possible stats
#wins, saves, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs
pitchers <- pitchers %>%
  filter(ip > 1) %>%
  mutate(avg_p = round(hits / (3 * ip), 3),  #hits against rate
         win_rate = round(9 * wins / ip, 3),  #win rate per nine innings
         saves_rate = round(saves / ip, 3),  #save rate per inning
         hra_rate = round(9 * hra / ip, 3))  #home runs allowed rate per nine innings

#METHOD 1: separate starters and relievers
starters <- pitchers %>%
  filter(gs > 0)
relievers <- pitchers %>%
  filter(gs == 0)

z_score_starters <- function(df) {
  # df <- df %>% 
  #   filter(pa > 1) %>%  #the majority of the df's contain players with 1 pa, presumably for ratio data if they do get "called up"
  #   select(name, team, pos, games, pa, avg, runs, hr, rbi, sb_net, ops)  #select stats used for fantasy league
  
  #wins, saves, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs
  df$wins_z <- z_score(df$wins)
  df$era_z <- z_score(df$era) * -1
  df$whip_z <- z_score(df$whip) * -1
  df$avg_p_z <- z_score(df$avg_p) * -1
  df$k_rate_z <- z_score(df$k_rate)
  df$bb_rate_z <- z_score(df$bb_rate) * -1
  df$fip_z <- z_score(df$fip) * -1
  df$hra_rate_z <- z_score(df$hra) * -1
  #   
  # df$hit_z <- round(as.numeric(z_score(df$hit)), 3)
  # df$double_z <- round(as.numeric(z_score(df$double)), 3)
  # df$triple_z <- round(as.numeric(z_score(df$triple)), 3)
  # df$hr_z <- round(as.numeric(z_score(df$hr)), 3)
  # df$runs_z <- round(as.numeric(z_score(df$runs)), 3)
  # df$rbi_z <- round(as.numeric(z_score(df$rbi)), 3)
  # df$bb_z <- round(as.numeric(z_score(df$bb)), 3)
  # df$so_z <- round(as.numeric(z_score(df$so)), 3)
  # df$sb_z <- round(as.numeric(z_score(df$sb)), 3)
  # df$avg_z <- round(as.numeric(z_score(df$avg)), 3)
  # df$obp_z <- round(as.numeric(z_score(df$obp)), 3)
  # df$slg_z <- round(as.numeric(z_score(df$slg)), 3)
  # df$ops_z <- round(as.numeric(z_score(df$ops)), 3)
  # df$woba_z <- round(as.numeric(z_score(df$woba)), 3)
  # df$tb_z <- round(as.numeric(z_score(df$tb)), 3)
  # df$rbi_r_z <- round(as.numeric(z_score(df$rbi_r)), 3)
  # df$xbh_z <- round(as.numeric(z_score(df$xbh)), 3)
  #df$sb_net_z <- round(as.numeric(z_score(df$sb_net)), 3)
  
  df
  # df <- df %>%
  #   mutate(z_score = avg_z + runs_z + hr_z + rbi_z + ops_z) %>%
  #   arrange(desc(z_score)) %>%
  #   head(n_df)
}

starters <- z_score_starters(starters)
#METHOD 2: starters and relievers in same group

ggplot(pitchers, aes(k_rate)) + geom_histogram(binwidth = .25)
ggplot(starters, aes(hra_rate)) + geom_histogram(binwidth = .02)
ggplot(starters, aes(k_rate)) + geom_histogram(binwidth = .25)
ggplot(starters, aes(win_rate)) + geom_histogram(binwidth = .02) + xlim(.1, .9)
ggplot(starters, aes(wins)) + geom_histogram(binwidth = 1)
