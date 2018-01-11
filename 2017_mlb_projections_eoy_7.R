pitchers <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 8)
names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
                     "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")

#FUNCTIONS
z_total <- function(stat1, stat2, stat3, stat4, stat5, stat6) {
  z_tot <- stat1 + stat2 + stat3 + stat4 + stat5 + stat6
}

#all possible stats
#wins, saves, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs
pitchers <- pitchers %>%
  filter(ip > 1) %>%
  mutate(avg_p = round(hits / (bb + hits + 3 * ip), 3),  #hits against rate
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
  df$wins_z <- round(z_score(df$wins), 3)
  df$era_raw_z <- round(z_score(df$era) * -1, 3)
  df$whip_raw_z <- round(z_score(df$whip) * -1, 3)
  df$avg_p_raw_z <- round(z_score(df$avg_p) * -1, 3)
  df$k_rate_z <- round(z_score(df$k_rate), 3)
  df$bb_rate_z <- round(z_score(df$bb_rate) * -1, 3)
  df$fip_raw_z <- round(z_score(df$fip) * -1, 3)
  df$hra_rate_z <- round(z_score(df$hra_rate) * -1, 3)
  #   
  df
}

starters <- z_score_starters(starters)

starters$era_z <- starters$era_raw_z * starters$ip / mean(starters$ip)
starters$whip_z <- starters$whip_raw_z * starters$ip / mean(starters$ip)
starters$avg_p_z <- starters$avg_p_raw_z * starters$ip / mean(starters$ip)
starters$k_z <- starters$k_rate_z * starters$ip / mean(starters$ip)
starters$bb_z <- starters$bb_rate_z * starters$ip / mean(starters$ip)
starters$fip_z <- starters$fip_raw_z * starters$ip / mean(starters$ip)
starters$hra_z <- starters$hra_rate_z * starters$ip / mean(starters$ip)

starters$era_z <- z_score(starters$era_z)
starters$whip_z <- z_score(starters$whip_z)
starters$avg_p_z <- z_score(starters$avg_p_z)
starters$k_z <- z_score(starters$k_z)
starters$bb_z <- z_score(starters$bb_z)
starters$fip_z <- z_score(starters$fip_z)
starters$hra_z <- z_score(starters$hra_z)

starters$z_tot <- as.numeric(z_total(starters$wins_z, starters$era_z, starters$whip_z, starters$k_z, starters$hra_z, 0))

starters <- starters %>%
  arrange(desc(z_tot))
starters1 <- starters[1:104,]

starters1 <- z_score_starters(starters1)
#METHOD 2: starters and relievers in same group

ggplot(pitchers, aes(k_rate)) + geom_histogram(binwidth = .25)
ggplot(starters, aes(hra_rate)) + geom_histogram(binwidth = .02)
ggplot(starters, aes(k_rate)) + geom_histogram(binwidth = .25)
ggplot(starters, aes(win_rate)) + geom_histogram(binwidth = .02) + xlim(.1, .9)
ggplot(starters, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(starters, aes(era_z)) + geom_histogram(binwidth = .1)
ggplot(starters, aes(whip_z)) + geom_histogram(binwidth = .1)
