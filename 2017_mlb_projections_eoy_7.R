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
starters$pos <- "sp"
relievers <- pitchers %>%
  filter(gs == 0)
relievers$pos <- "rp"

z_score_starters <- function(df) {
  
  #wins, saves, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs
  df$wins_z <- round(as.numeric(z_score(df$wins)), 3)
  df$era_raw_z <- round(as.numeric(z_score(df$era) * -1), 3)
  df$whip_raw_z <- round(as.numeric(z_score(df$whip) * -1), 3)
  df$avg_p_raw_z <- round(as.numeric(z_score(df$avg_p) * -1), 3)
  df$k_rate_z <- round(as.numeric(z_score(df$k_rate)), 3)
  df$bb_rate_z <- round(as.numeric(z_score(df$bb_rate) * -1), 3)
  df$fip_raw_z <- round(as.numeric(z_score(df$fip) * -1), 3)
  df$hra_rate_z <- round(as.numeric(z_score(df$hra_rate) * -1), 3)
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

#####
#Now run the same process with the starting pitchers in the actual playing pool
starters1 <- z_score_starters(starters1)

starters1$era_z <- starters1$era_raw_z * starters1$ip / mean(starters1$ip)
starters1$whip_z <- starters1$whip_raw_z * starters1$ip / mean(starters1$ip)
starters1$avg_p_z <- starters1$avg_p_raw_z * starters1$ip / mean(starters1$ip)
starters1$k_z <- starters1$k_rate_z * starters1$ip / mean(starters1$ip)
starters1$bb_z <- starters1$bb_rate_z * starters1$ip / mean(starters1$ip)
starters1$fip_z <- starters1$fip_raw_z * starters1$ip / mean(starters1$ip)
starters1$hra_z <- starters1$hra_rate_z * starters1$ip / mean(starters1$ip)

starters1$era_z <- round(as.numeric(z_score(starters1$era_z)), 3)
starters1$whip_z <- round(as.numeric(z_score(starters1$whip_z)), 3)
starters1$avg_p_z <- round(as.numeric(z_score(starters1$avg_p_z)), 3)
starters1$k_z <- round(as.numeric(z_score(starters1$k_z)), 3)
starters1$bb_z <- round(as.numeric(z_score(starters1$bb_z)), 3)
starters1$fip_z <- round(as.numeric(z_score(starters1$fip_z)), 3)
starters1$hra_z <- round(as.numeric(z_score(starters1$hra_z)), 3)

starters1$z_tot <- as.numeric(z_total(starters1$wins_z, starters1$era_z, starters1$whip_z, starters1$k_z, starters1$hra_z, 0))

starters1 <- starters1 %>%
  arrange(desc(z_tot))

starters2 <- starters1 %>%
  select(name, team, pos, z_tot, wins_z, era_z, whip_z, k_z, hra_z) %>%
  arrange(desc(z_tot))
starters2$z_pos <- starters2$z_tot

#METHOD 2: starters and relievers in same group
z_score_relievers <- function(df) {
  
  #wins, saves, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs
  df$saves_z <- round(z_score(df$saves), 3)
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

relievers <- z_score_relievers(relievers)

relievers$era_z <- relievers$era_raw_z * relievers$ip / mean(relievers$ip)
relievers$whip_z <- relievers$whip_raw_z * relievers$ip / mean(relievers$ip)
relievers$avg_p_z <- relievers$avg_p_raw_z * relievers$ip / mean(relievers$ip)
relievers$k_z <- relievers$k_rate_z * relievers$ip / mean(relievers$ip)
relievers$bb_z <- relievers$bb_rate_z * relievers$ip / mean(relievers$ip)
relievers$fip_z <- relievers$fip_raw_z * relievers$ip / mean(relievers$ip)
relievers$hra_z <- relievers$hra_rate_z * relievers$ip / mean(relievers$ip)

relievers$era_z <- z_score(relievers$era_z)
relievers$whip_z <- z_score(relievers$whip_z)
relievers$avg_p_z <- z_score(relievers$avg_p_z)
relievers$k_z <- z_score(relievers$k_z)
relievers$bb_z <- z_score(relievers$bb_z)
relievers$fip_z <- z_score(relievers$fip_z)
relievers$hra_z <- z_score(relievers$hra_z)

relievers$z_tot <- as.numeric(z_total(relievers$saves_z, relievers$era_z, relievers$whip_z, relievers$k_z, relievers$hra_z, 0))

relievers <- relievers %>%
  arrange(desc(as.numeric(saves_z)), desc(z_tot))
relievers1 <- relievers[1:40,]

#####
#Now run the same process with the rlief pitchers in the actual playing pool
relievers1 <- z_score_relievers(relievers1)

relievers1$era_z <- relievers1$era_raw_z * relievers1$ip / mean(starters1$ip)
relievers1$whip_z <- relievers1$whip_raw_z * relievers1$ip / mean(starters1$ip)
relievers1$avg_p_z <- relievers1$avg_p_raw_z * relievers1$ip / mean(starters1$ip)
relievers1$k_z <- relievers1$k_rate_z * relievers1$ip / mean(starters1$ip)
relievers1$bb_z <- relievers1$bb_rate_z * relievers1$ip / mean(starters1$ip)
relievers1$fip_z <- relievers1$fip_raw_z * relievers1$ip / mean(starters1$ip)
relievers1$hra_z <- relievers1$hra_rate_z * relievers1$ip / mean(starters1$ip)

# relievers1$era_z <- round(z_score(relievers1$era_z), 3)
# relievers1$whip_z <- round(z_score(relievers1$whip_z), 3)
# relievers1$avg_p_z <- round(z_score(relievers1$avg_p_z), 3)
# relievers1$k_z <- round(z_score(relievers1$k_z), 3)
# relievers1$bb_z <- round(z_score(relievers1$bb_z), 3)
# relievers1$fip_z <- round(z_score(relievers1$fip_z), 3)
# relievers1$hra_z <- round(z_score(relievers1$hra_z), 3)

relievers1$z_tot <- as.numeric(z_total(relievers1$saves_z, relievers1$era_z, relievers1$whip_z, relievers1$k_z, relievers1$hra_z, 0))

relievers1 <- relievers1 %>%
  arrange(desc(z_tot))

relievers2 <- relievers1 %>%
  select(name, team, pos, z_tot, saves_z, era_z, whip_z, k_z, hra_z) %>%
  arrange(desc(as.numeric(saves_z)))
relievers2$z_pos <- relievers2$z_tot

#
ggplot(pitchers, aes(k_rate)) + geom_histogram(binwidth = .25)
ggplot(starters, aes(hra_rate)) + geom_histogram(binwidth = .02)
ggplot(starters1, aes(k_rate)) + geom_histogram(binwidth = .25)
ggplot(starters, aes(win_rate)) + geom_histogram(binwidth = .02) + xlim(.1, .9)
ggplot(starters, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(starters, aes(era_z)) + geom_histogram(binwidth = .1)
ggplot(starters, aes(whip_z)) + geom_histogram(binwidth = .1)
ggplot(relievers, aes(wins)) + geom_histogram(binwidth = 1) + xlim(0, max(relievers$wins))
ggplot(relievers1, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(relievers1, aes(saves_z)) + geom_histogram(binwidth = .1)
ggplot(relievers1, aes(saves)) + geom_histogram(binwidth = 1)
