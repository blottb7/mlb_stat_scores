#Description

#Set working directory
#setwd("~/Desktop/R_projects/baseball/eiflb")  #apple
#setwd("C:/Users/Ben/Desktop/FF/baseball")  #asus
setwd("C:/Users/Ben/Desktop/R projects")  #new toshiba working directory

#libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)
library(zoo)  #for na.locf fn

#user settings
#number of teams, and number of starters at each position for a given fantasy league
# n_teams <- 16
# starting_catchers <- 1
# starting_first_basemen <- 1
# starting_second_basemen <- 1
# starting_third_basemen <- 1
# starting_shortstops <- 1
# starting_outfielders <- 4
# starting_middle_infielders <- 1
# starting_corner_infielders <- 1
# starting_designated_hitters <- 1
# #pitchers
# starting_pitchers <- 6.5
# relief_pitchers <- 2.5
# #bench players
# bench_players <- 2
# #costs
# min_cost <- 3
# 
# #staring players per position for a fantasy league
# n_catchers <- n_teams * starting_catchers
# n_first_basemen <- n_teams * starting_first_basemen
# n_second_basemen <- n_teams * starting_second_basemen
# n_third_basemen <- n_teams * starting_third_basemen
# n_shortstops <- n_teams * starting_shortstops
# n_outfielders <- n_teams * starting_outfielders
# n_middle_infielders <- n_teams * starting_middle_infielders
# n_corner_infielders <- n_teams * starting_corner_infielders
# n_designated_hitters <- n_teams * starting_designated_hitters
# #pitchers
# n_starting_pitchers <- n_teams * starting_pitchers
# n_relief_pitchers <- n_teams * relief_pitchers
# #bench
# n_bench <- n_teams * bench_players

#FUNCTIONS
#z_score calculation for each selected stat
z_score <- function(stat){
  scale(stat)
}

#create general form of function for creating z-score for position players
z_score_hitter <- function(df) {
  #all hitter stats besides sb and sb_net
  df$hit_z <- round(as.numeric(z_score(df$hit)), 3)
  df$double_z <- round(as.numeric(z_score(df$double)), 3)
  df$triple_z <- round(as.numeric(z_score(df$triple)), 3)
  df$hr_z <- round(as.numeric(z_score(df$hr)), 3)
  df$runs_z <- round(as.numeric(z_score(df$runs)), 3)
  df$rbi_z <- round(as.numeric(z_score(df$rbi)), 3)
  df$bb_z <- round(as.numeric(z_score(df$bb)), 3)
  df$so_z <- round(as.numeric(z_score(df$so)), 3)
  df$avg_z <- round(as.numeric(z_score(df$avg)), 3)
  df$obp_z <- round(as.numeric(z_score(df$obp)), 3)
  df$slg_z <- round(as.numeric(z_score(df$slg)), 3)
  df$ops_z <- round(as.numeric(z_score(df$ops)), 3)
  df$woba_z <- round(as.numeric(z_score(df$woba)), 3)
  df$tb_z <- round(as.numeric(z_score(df$tb)), 3)
  df$rbi_r_z <- round(as.numeric(z_score(df$rbi_r)), 3)
  df$xbh_z <- round(as.numeric(z_score(df$xbh)), 3)
  
  df
}

#function for user selected hitter stats
z_total <- function(stat1, stat2, stat3, stat4, stat5, stat6) {
  z_tot <- (stat1 + stat2 + stat3 + stat4 + stat5 + stat6)
}

z_score_starters <- function(df) {
  
  #wins, era, saves, ip, hr (allowed), so, whip, k/9, bb/9, fip, k/bb, avg, hld (missing), qs (missing)
  df$wins_z <- round(as.numeric(z_score(df$wins)), 3)
  df$era_z <- round(as.numeric(z_score(df$era) * -1), 3)
  df$ip_z <- round(as.numeric(z_score(df$ip)), 3)
  df$hra_z <- round(as.numeric(z_score(df$hra) * -1), 3)
  df$k_z <- round(as.numeric(z_score(df$so)), 3)
  df$whip_z <- round(as.numeric(z_score(df$whip) * -1), 3)
  df$k_rate_z <- round(as.numeric(z_score(df$k_rate)), 3)
  df$bb_rate_z <- round(as.numeric(z_score(df$bb_rate) * -1), 3)
  df$fip_z <- round(as.numeric(z_score(df$fip) * -1), 3)
  df$kbb_rate_z <- round(as.numeric(z_score(df$kbb)), 3)
  df$avg_p_z <- round(as.numeric(z_score(df$avg_p) * -1), 3)
  df$hra_rate_z <- round(as.numeric(z_score(df$hra_rate) * -1), 3)
  #   
  df
}

#METHOD 2: starters and relievers in same group
z_score_relievers <- function(df) {
  
  #wins, era, saves, ip, hr (allowed), so, whip, k/9, bb/9, fip, k/bb, avg, hld (missing), qs (missing)
  df$saves_z <- round(as.numeric(z_score(df$saves)), 3)
  df$era_z <- round(as.numeric(z_score(df$era) * -1), 3)
  df$ip_z <- round(as.numeric(z_score(df$ip)), 3)
  df$hra_z <- round(as.numeric(z_score(df$hra) * -1), 3)
  df$k_z <- round(as.numeric(z_score(df$so)), 3)
  df$whip_z <- round(as.numeric(z_score(df$whip) * -1), 3)
  df$k_rate_z <- round(as.numeric(z_score(df$k_rate)), 3)
  df$bb_rate_z <- round(as.numeric(z_score(df$bb_rate) * -1), 3)
  df$fip_z <- round(as.numeric(z_score(df$fip) * -1), 3)
  df$kbb_rate_z <- round(as.numeric(z_score(df$kbb)), 3)
  df$avg_p_z <- round(as.numeric(z_score(df$avg_p) * -1), 3)
  df$hra_rate_z <- round(as.numeric(z_score(df$hra_rate) * -1), 3)
  #   
  df
}

#METHOD 2: starters and relievers in same group
z_score_pitchers <- function(df) {
  
  #wins, era, saves, ip, hr (allowed), so, whip, k/9, bb/9, fip, k/bb, avg, hld (missing), qs (missing)
  df$wins_z <- round(as.numeric(z_score(df$wins)), 3)
  df$saves_z <- round(as.numeric(z_score(df$saves)), 3)
  df$era_z <- round(as.numeric(z_score(df$era) * -1), 3)
  df$ip_z <- round(as.numeric(z_score(df$ip)), 3)
  df$hra_z <- round(as.numeric(z_score(df$hra) * -1), 3)
  df$k_z <- round(as.numeric(z_score(df$so)), 3)
  df$whip_z <- round(as.numeric(z_score(df$whip) * -1), 3)
  df$k_rate_z <- round(as.numeric(z_score(df$k_rate)), 3)
  df$bb_rate_z <- round(as.numeric(z_score(df$bb_rate) * -1), 3)
  df$fip_z <- round(as.numeric(z_score(df$fip) * -1), 3)
  df$kbb_rate_z <- round(as.numeric(z_score(df$kbb)), 3)
  df$avg_p_z <- round(as.numeric(z_score(df$avg_p) * -1), 3)
  df$hra_rate_z <- round(as.numeric(z_score(df$hra_rate) * -1), 3)
  #   
  df
}

#read in data
catchers <- read_excel("2018_fangraphs_projections.xlsx", sheet = 2)
first_basemen <- read_excel("2018_fangraphs_projections.xlsx", sheet = 3)
second_basemen <- read_excel("2018_fangraphs_projections.xlsx", sheet = 4)
third_basemen <- read_excel("2018_fangraphs_projections.xlsx", sheet = 5)
shortstops <- read_excel("2018_fangraphs_projections.xlsx", sheet = 6)
outfielders <- read_excel("2018_fangraphs_projections.xlsx", sheet = 7)
pitchers <- read_excel("2018_fangraphs_projections.xlsx", sheet = 8)

#rename position player vars
#name_vector for 2018 df; does not include column: "adp". Include "adp" if and when necessary.
name_vector <- c("name", "team", "games", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb", "so",
                 "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
                 "waste3", "offense", "defense", "war", "waste4", "adp", "playerid")

#assign the name vector to each position df
names(catchers) <- name_vector
names(first_basemen) <- name_vector
names(second_basemen) <- name_vector
names(shortstops) <- name_vector
names(third_basemen) <- name_vector
names(outfielders) <- name_vector

#the playerid reads in different classes depending on the position, so coerse the class to the same
catchers$playerid <- as.character(catchers$playerid)
first_basemen$playerid <- as.character(first_basemen$playerid)
second_basemen$playerid <- as.character(second_basemen$playerid)
third_basemen$playerid <- as.character(third_basemen$playerid)
shortstops$playerid <- as.character(shortstops$playerid)
outfielders$playerid <- as.character(outfielders$playerid)

#create a position var and assign it to each df
catchers$pos <- "2"
first_basemen$pos <- "3"
second_basemen$pos <- "4"
third_basemen$pos <- "5"
shortstops$pos <- "6"
outfielders$pos <- "7"

#combine all positions into a df
hitters <- catchers %>%
  full_join(first_basemen) %>%
  full_join(second_basemen) %>%
  full_join(third_basemen) %>%
  full_join(shortstops) %>%
  full_join(outfielders) #%>%
#filter(pa > 1)  #get this done out of the gate; removes players who have a "token" projection (not expected to play in MLB)

#removing the filter for combination with taken league players

#set positions for hitters in my 16 team league
hitters$pos <- ifelse(hitters$name == "Ian Desmond", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Rhys Hoskins", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Cody Bellinger", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Eric Thames", 7, hitters$pos)  #FG has him marked only has "3"
hitters$pos <- ifelse(hitters$name == "Matt Olson", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jose Ramirez", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Trey Mancini", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Brandon Moss", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Joey Gallo", 3, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Javier Baez", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Ian Happ", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jean Segura", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Wilmer Flores", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jedd Gyorko", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jose Peraza", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Matt Carpenter", 3, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Josh Harrison", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Eduardo Nunez", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Starlin Castro", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jonathan Villar", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Tim Beckham", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Ben Zobrist", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Ryan McMahon", 3, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Adam Frazier", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Brad Miller", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Brandon Drury", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Chad Pinder", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Chris Owings", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Colin Moran", 3, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Derek Dietrich", 5, hitters$pos)
hitters$pos <- ifelse(hitters$name == "J.P. Crawford", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Johan Camargo", 5, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jose Martinez", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jose Pirela", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Luis Valbuena", 3, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Steve Pearce", 7, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Travis Shaw", 5, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Austin Barnes", 2, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Dixon Machado", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Hernan Perez", 7, hitters$pos)
#hitters standard 12 team
#remove rows where the name AND position are duplicated
hitters <- unique(hitters)

hitters <- hitters %>%
  #build stat projections for all missing stats, i.e. sb_net
  mutate(tb = hit + double + 2 * triple + 3 * hr,  #total bases
         rbi_r = rbi + runs, #rbis plus runs
         xbh = double + triple + hr,  #extra base hits
         sb_net = sb - cs) %>%  #stolen bases - caught stealing
  select(-waste1, -waste2, -waste3,  #remove spacer cols
         -wrc_plus, -bsr, -fld, -offense, -defense, -war, -playerid) %>%
  arrange(name)

#read in pitchers, combine with hitters, combine with taken players, separate again
taken <- read_excel("eiflb_2018_draft_empty_slots.xlsx")

#name pitchers here
names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
                     "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "adp", "player_id")
pitchers <- pitchers %>% arrange(name)

#regulate these players names
hitters$name <- ifelse(hitters$name == "Nick Castellanos", "Nicholas Castellanos", hitters$name)
hitters$name <- ifelse(hitters$name == "Raul Mondesi", "Adalberto Mondesi", hitters$name)
pitchers$name <- ifelse(pitchers$name == "Jakob Junis", "Jake Junis", pitchers$name)
pitchers$name <- ifelse(pitchers$name == "Jacob deGrom", "Jacob DeGrom", pitchers$name)
#also move Greg Holland, who has no projections because he is not yet on a team, but is owned.

#first check for matching
taken1 <- taken %>%
  left_join(hitters) %>%
  left_join(pitchers, by = "name") %>%
  arrange(name)

#keep "regulars", those players who are going to start more days than not
hitters_reg <- hitters %>%
  filter(pa >= 250) %>%  #switch this to 250 so grandal is in
  arrange(name)

#how, anti-join taken players with hitters and pitchers
hitters_reg <- hitters_reg %>%
  anti_join(taken1, by = "name")

pitchers <- pitchers %>%
  anti_join(taken1, by = "name")

#do SB related stats across entire population; don't want position-relative scores for low sb positions like catcher.
#box-cox transformation of sb and sb_net; a normal distribution is more useful
hitters_reg$sb_z <- round(as.numeric(z_score(BoxCox(hitters_reg$sb, .45))), 3)
hitters_reg$sb_net_z <- round(as.numeric(z_score(BoxCox(hitters_reg$sb_net, .45))), 3)

#separate hitters by position
catchers1 <- hitters_reg %>% filter(pos == "2")
first_basemen1 <- hitters_reg %>% filter(pos == "3")
second_basemen1 <- hitters_reg %>% filter(pos == "4")
third_basemen1 <- hitters_reg %>% filter(pos == "5")
shortstops1 <- hitters_reg %>% filter(pos == "6")
outfielders1 <- hitters_reg %>% filter(pos == "7")

#run z-score pos on each position df
catchers1 <- z_score_hitter(catchers1)
first_basemen1 <- z_score_hitter(first_basemen1)
second_basemen1 <- z_score_hitter(second_basemen1)
third_basemen1 <- z_score_hitter(third_basemen1)
shortstops1 <- z_score_hitter(shortstops1)
outfielders1 <- z_score_hitter(outfielders1)

#select only the z_scores I want for the chosen league
#run for each position
df <- catchers1  #set dataframe var
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

#run for catchers
catchers1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)  #create z_tot
catchers2 <- catchers1 %>%
  arrange(desc(z_tot))  #arrange by descending z_tot
catchers2 <- catchers2[1:5,]  #keep only the amount of catchers warranted for the league

#repeat this process for each position
#first basemen
df <- first_basemen1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

first_basemen1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
first_basemen2 <- first_basemen1 %>%
  arrange(desc(z_tot))
first_basemen2 <- first_basemen2[1:1,]

#second basemen
df <- second_basemen1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

second_basemen1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
second_basemen2 <- second_basemen1 %>%
  arrange(desc(z_tot))
second_basemen2 <- second_basemen2[1:2,]

#third basemen
df <- third_basemen1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

third_basemen1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
third_basemen2 <- third_basemen1 %>%
  arrange(desc(z_tot))
third_basemen2 <- third_basemen2[1:3,]

#shortstops
df <- shortstops1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

shortstops1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
shortstops2 <- shortstops1 %>%
  arrange(desc(z_tot))
shortstops2 <- shortstops2[0,]

#outfielders
df <- outfielders1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

outfielders1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
outfielders2 <- outfielders1 %>%
  arrange(desc(z_tot))
outfielders2 <- outfielders2[1:7,]

#create middle infielders df
middle_infielders <- second_basemen1 %>%
  bind_rows(shortstops1) %>%  #combine ALL QUALIFIED (300+ PA) SS and 2B-men
  anti_join(second_basemen2, by = "name") %>%  #remove the 2B already selected
  anti_join(shortstops2, by = "name")  #remove the SS already selected
#run z-score on middle infielders after removing already used players
middle_infielders1 <- z_score_hitter(middle_infielders)

#run selection process on middle infielders
df <- middle_infielders1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

middle_infielders1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
middle_infielders2 <- middle_infielders1 %>%
  arrange(desc(z_tot))
middle_infielders2 <- middle_infielders2[1:4,]

#run same process on corner infielders as middle infielders
corner_infielders <- first_basemen1 %>%
  bind_rows(third_basemen1) %>%
  anti_join(first_basemen2, by = "name") %>%
  anti_join(third_basemen2, by = "name")
corner_infielders1 <- z_score_hitter(corner_infielders)

df <- corner_infielders1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

corner_infielders1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
corner_infielders2 <- corner_infielders1 %>%
  arrange(desc(z_tot))

#remove ineligible CI's (Pujols and Hanley Ramirez)
corner_infielders2 <- corner_infielders2[-c(6,9),]
corner_infielders2 <- corner_infielders2[1:8,]

#combine all selected players
hitters_no_dh <- bind_rows(catchers2, first_basemen2, second_basemen2, third_basemen2, shortstops2, outfielders2,
                           middle_infielders2, corner_infielders2) %>%
  arrange(name)

#anti_join remaining hitters to selected hitters for use with DH/util and bench players
remaining_hitters <- hitters_reg %>%
  anti_join(hitters_no_dh, by = "name")

#create designated hitters
#run z_score on designated hitters
designated_hitters <- z_score_hitter(remaining_hitters)

df <- designated_hitters
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

designated_hitters["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)

#designated hitters plus once bench hitter per team
designated_hitters1 <- designated_hitters %>%
  arrange(desc(z_tot))
designated_hitters1 <- designated_hitters1[1:(9 + 12),]  #adds 1 DH and 1 bench hitter per team

#keep remaning hitters in player pool:
saved_hitters <- designated_hitters %>%
  anti_join(designated_hitters1) %>%
  arrange(desc(z_tot))
##### #####
#Run numbers across all "selected" hitters
hitters1 <- bind_rows(hitters_no_dh, designated_hitters1)  #bind "designated hitters"/utility players/bench to rest of hitters
hitters1$sb_z <- round(as.numeric(z_score(BoxCox(hitters1$sb, .45))), 3)  #generate sb related z_score
hitters1$sb__net_z <- round(as.numeric(z_score(BoxCox(hitters1$sb_net, .45))), 3)  #generate sb related z_score
hitters1 <- z_score_hitter(hitters1)

df <- hitters1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

hitters1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
hitters2 <- hitters1 %>%
  arrange(desc(z_tot))

#position relative z_score
#generate a mean z_score for each position across all players at the position in the "league"
hitters_zpos <- hitters2 %>%
  group_by(pos) %>%
  summarize(z_pos_mean = round(mean(z_tot), 2)) %>%
  arrange(desc(z_pos_mean))

#join the hitters with the mean z_score/position: z_pos
hitters_zpos1 <- hitters2 %>%
  left_join(hitters_zpos, by = "pos") %>%
  mutate(z_pos = round(z_tot - z_pos_mean, 4)) %>%
  arrange(desc(z_pos))

#arrange by z_tot
hitters_zpos2 <- hitters_zpos1 %>%
  arrange(desc(z_tot))

#create final hitters df with only relevant stats
#name the stats to keep
df <- hitters_zpos2
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

#select the columns
hitters3 <- hitters_zpos2[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", 
                              names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]
#arrange the columns
hitters3 <- hitters3 %>%
  arrange(desc(z_pos))

#remove unncessary df's
rm(catchers1, catchers2, corner_infielders, corner_infielders1, corner_infielders2,
   designated_hitters1, first_basemen1, first_basemen2, hitters_no_dh, hitters1, hitters2,
   middle_infielders1, middle_infielders2, outfielders1, outfielders2, remaining_hitters,
   second_basemen1, second_basemen2, shortstops1, shortstops2, third_basemen1,
   third_basemen2)
rm(hitters_zpos, hitters_zpos1)
#remove replaceable stats
rm(df, stat1, stat2, stat3, stat4, stat5, stat6)

#keep only final hitters_df with the stats I want, "hitters_3" and hitters_zpos2
rm(catchers, designated_hitters, first_basemen, hitters, hitters_reg, middle_infielders,
   outfielders, second_basemen, shortstops, third_basemen)
#remove taken players df
rm(taken, taken1)

find_name <- function(name) {
  which(hitters3$name == name)
}

##### PITCHERS #####
#name pitchers
# names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
#                      "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "adp", "player_id")

#all possible pitcher stats
#wins, era, saves, ip, hr (allowed), so, whip, k/9, bb/9, fip, k/bb, avg, hld (missing), qs (missing); re-ordered
pitchers <- pitchers %>%
  filter(ip > 1) %>%  #filter for pitchers with more than 1 projected innings pitched
  mutate(kbb = round(so / bb, 3),  #strikeouts per walk rate
         avg_p = round(hits / (hits + 3 * ip), 3),  #hits against rate
         #win_rate = round(9 * wins / ip, 3),  #win rate per nine innings
         #saves_rate = round(saves / ip, 3),  #save rate per inning
         hra_rate = round(9 * hra / ip, 3))  #home runs allowed rate per nine innings
#no data on hld and qs

#METHOD 1: separate starters and relievers
#starters
starters <- pitchers %>%
  filter(gs > 0) %>%  #must be projected to start a game
  filter(ip >= 80)  #only want "regulars"; make cut off 100 projected innings pitched
#moved this to 80 bc league is so deep and so many need to be added
starters$pos <- "sp"  #designate position
#relievers
relievers <- pitchers %>%
  filter(gs == 0)  #all remaining pitchers
relievers$pos <- "rp"  #designate position

#apply z_score to starters
starters <- z_score_starters(starters)

#select starting pitcher stat categories
#because innings are already controlled for with a threshold, use counting stats, not rate stats for k and hra
#on this interation, this is sufficient for K's, but not not hra's, so go back to hra_rate
#no saves, which would be stat6

#select the stats you want, organize by largest to smallest Z_score, then cut down the list to number of league players
df <- starters
stat1 <- df["wins_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_rate_z"]
stat6 <- 0

starters["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
starters <- starters %>%
  arrange(desc(z_tot))

#remove untenable pitchers
starters1 <- starters[-c(11,43,47),]  #wilmer font (reliever), ubaldo jimenez (no team), yonny chirinos (c'mon)
#subset by number of starters in the league
starters1 <- starters1[1:(30 + 16),]  #include 1 bench pitcher per team

#####
#Now run the same process with the starting pitchers in the actual playing pool
starters1 <- z_score_starters(starters1)

df <- starters1
stat1 <- df["wins_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_rate_z"]
stat6 <- 0

starters1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
starters1 <- starters1 %>%
  arrange(desc(z_tot))

#set z-pos values for later joining with hitters
starters1$z_pos <- starters1$z_tot
starters1$z_pos_mean <- 0
#select columns to keep for joining with hitters
starters2 <- starters1[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", 
                           names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]

saved_starters <- starters %>%
  anti_join(starters2, by = "name") %>%
  arrange(desc(z_tot))
#RELIEVERS
relievers <- z_score_relievers(relievers)

#user selected vars
#use saves instead of wins
df <- relievers
stat1 <- df["saves_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_rate_z"]
stat6 <- 0

relievers["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)

#select top relievers based on most saves, with z_tot being the tie-breaker
relievers <- relievers %>%
  #arrange(desc(z_tot))
  arrange(desc(saves_z), desc(z_tot))

#select relievers manually
relievers1 <- relievers[c(1:3,7,17,9,15,16,47,68,64,12,6),]
relievers1 <- relievers1 %>%
  arrange(desc(z_tot))

# #arrange(desc(as.numeric(saves_z)), desc(z_tot))
# relievers1 <- relievers[1:13,]

#####
#Now run the same process with the rlief pitchers in the actual playing pool
relievers1 <- z_score_relievers(relievers1)

#select the desired stats again
#i missed the next 7 lines for the first time through so was getting inflated reliever scores
df <- relievers1
stat1 <- df["saves_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_rate_z"]
stat6 <- 0

relievers1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)

relievers1$z_pos <- relievers1$z_tot
relievers1$z_pos_mean <- 0
relievers2 <- relievers1[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", 
                             names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]

#organize relievers by z_pos for exploration
relievers2 <- relievers2 %>%
  arrange(desc(z_pos))

##### Finished initial groupings #####

#Trying a new method. Let's just combine the starters, relievers, and hitters with their z_scores as is.
#I will give them prices
#Then, I'll weight them by my own parameters.
#First I'll try 13, 7.5, and 2.5
#Then I'll reweight everything to equal 4400

#first, combine all players
all_players <- hitters3 %>%
  full_join(starters2) %>%
  full_join(relievers2) %>%
  arrange(desc(z_pos))

#Get work from other workbook
ap <- all_players
ggplot(ap, aes(rank(z_pos), z_pos)) + geom_point()

#subtract z_saves from starters and z_wins from relievers, ~.7

#available dollars - milb dollars - last 5 players @$3 each.
dollars <- 1103 - 85 - 18

ap <- head(ap, -6)  #remove last 6 ROWS (that's where it tails off)
ap$z_rank <- rank(ap$z_pos)  #rank var
ggplot(ap, aes(rank(z_pos), z_pos)) + geom_point()

# #Need to go back to two linear models
#   #Nope, not working
# model1 <- lm(z_pos ~ z_rank, ap[17:104,])
# model2 <- lm(z_pos ~ z_rank, ap[1:16,])
# 
# #price = intercept + $3
# ap$price <- (model1$coefficients[1] + 6.59) + model1$coefficients[2] * ap$z_rank
# ap$price1 <- (model2$coefficients[1] + 6.3326) + model2$coefficients[2] * ap$z_rank

#z_pos shift var
ap$z_shift <- ap$z_pos - min(ap$z_pos)

# #find equation for #1003, n = 99
# B <- 1.801
# ap$p1 <- B ^ ap$z_pos + 3
# sum(ap$p1)
# ggplot(ap, aes(z_pos, p1)) + geom_point()
# 
# C <- 1.04243
# ap$p2 <- C ^ ap$p1
# sum(ap$p2)

# #So, this give me what I want, but it WAY overvalues top players
# #(Greinke @56)
# ap$z_shift <- ap$z_pos - min(ap$z_pos)
# D <- 1.4
# ap$p3 <- D ^ ap$z_shift + 2
# sum(ap$p3)
# ggplot(ap, aes(z_pos, p3)) + geom_point()

# #let's try z_shift + X
# #this is almost linear; this is not what I want, though it's closer
# #plus, the min value is 6
# ap$z_shift1 <- ap$z_shift + 10
# E <- 1.1524
# ap$p4 <- E ^ ap$z_shift1 + 2
# sum(ap$p4)
# ggplot(ap, aes(z_pos, p4)) + geom_point()
#


#These are candiates for the final pricing model
#so now let's change the "+2"
#yeah, this pretty much works
# ap$z_shift1 <- ap$z_shift + 10
# 
# G <- 1.1903
# ap$p5 <- G ^ ap$z_shift1 - 2.7
# sum(ap$p5)
# ggplot(ap, aes(z_pos, p5)) + geom_point()
# 
# #shift between 6 and 10
# ap$z_shift2 <- ap$z_shift + 8
# 
# H <- 1.2124
# ap$p6 <- H ^ ap$z_shift2 - 1.5
# sum(ap$p6)
# ggplot(ap, aes(z_pos, p6)) + geom_point()

#now try 15
ap$z_shift3 <- ap$z_shift + 15

I <- 1.149514
ap$auction_price <- I ^ ap$z_shift3 - 5
sum(ap$auction_price)
ggplot(ap, aes(z_pos, auction_price)) + geom_point()

#observe
ggplot(ap, aes(rank(z_pos), z_pos)) + geom_point()
ggplot(ap, aes(z_pos, p5)) + geom_point()
ggplot(ap, aes(z_pos, p6)) + geom_point()
ggplot(ap, aes(z_pos, auction_price)) + geom_point()
ggplot(ap, aes(z_pos, p7)) + geom_point()
# 
# #let's try shifting by other numbers than 10
# ap$z_shift2 <- ap$z_shift + 3
# 
# H <- 1.327
# ap$p6 <- H ^ ap$z_shift2
# sum(ap$p6)
# ggplot(ap, aes(z_pos, p6)) + geom_point()

# #So I think 10 is too much and 3 is too little, so something in between
# #I'll make the top player 3X the least
# ap$z_shift3 <- ap$z_shift + 6
# 
# I <- 1.237
# ap$p7 <- I ^ ap$z_shift3
# sum(ap$p7)
# ggplot(ap, aes(z_pos, p7)) + geom_point()
# ggplot(ap, aes(rank(z_pos), z_pos)) + geom_point()

#try the above with the lowest value set closer to 3
ap$z_shift3 <- ap$z_shift + 6

I <- 1.2435
ap$auction_price <- I ^ ap$z_shift3 - .5
sum(ap$auction_price)
#ggplot(ap, aes(z_pos, p7)) + geom_point()
#this gives me a usable model using method = 'loess'
ggplot(ap, aes(rank(z_pos), z_pos)) + geom_point() +
  geom_smooth(method = 'loess', span = .1)

ggplot(ap, aes(rank(z_pos), z_pos)) + geom_point()
ggplot(ap, aes(z_pos, auction_price)) + geom_point()
ggplot(ap, aes(z_rank, z_pos)) + geom_point() + geom_line(aes(y=ap$auction_price))

#reduce price of relievers to account for less innings pitched in terms of whip, era, and hra_rate
  #don't do this; enough people bid on relievers that these prices seem normal

#add removed players back into df
ap_last <- all_players[106:110,]
ap_last$auction_price <- 3

ap <- ap %>%
  full_join(ap_last) %>%
  arrange(desc(auction_price))

#select variables to keep for spread sheet

#####
#now divide these into position groups
sp <- ap %>%
  filter(pos == "sp")
of <- ap %>%
  filter(pos == "7")

# #So, now try fitting a loess model
# loess1 <- loess(z_pos ~ z_rank, data = ap)
# ap$loess1 <- predict(loess1)
# 
# #want a highly fitted model, so make span close to 0
# loess2 <- loess(z_pos ~ z_rank, data = ap, span = .1)
# ap$loess2 <- predict(loess2)
# #see how they look
# ggplot(ap, aes(z_pos, loess2)) + geom_point()
# #try a linear model with everything below z_pos = 2.5
# 
# #probably have a separate break down for <0, and 0 - 2.5
# ap_lo <- ap[18:105,]
# #price <- m * z_pos
# #ap$rank <- rank(ap$z_pos)
# model <- lm(z_pos ~ rank(z_pos), ap_lo)
# ap_lo$price <- rank(ap_lo$z_pos) * model$coefficients[2] + 3 #+ model$coefficients[1]
# ggplot(ap_lo, aes(rank(z_pos), z_pos)) + geom_point() + 
#   geom_abline(intercept = -3.61, slope = model$coefficients[2])
# 
# # plot(ap_lo$z_pos, ap_lo$price)
# # abline(a = model$coefficients[1], b = model$coefficients[2])
# 
# #now do the same for 2.5 - 5, and greater than 5
# ap_mid <- ap[7:17,]
# model1 <- lm(z_pos ~ z_rank, ap_mid)
# ap_mid$price <- ap_mid$z_rank * model1$coefficients[2] + model1$coefficients[1]
# ggplot(ap_mid, aes(z_rank, z_pos)) + geom_point() +
#   geom_abline(intercept = -5.17813, slope = model1$coefficients[2])
# 
# #set up a best fit line or best fit lines to allocate $970 to 94
# #players, where all players cost at least $3

# ap$z_rank <- rank(ap$z_pos)

# fit <- lm(z_pos~bs(z_rank,knots=c(75,88)),data=ap)
# plot(z_rank,z_pos,col="grey",xlab="Rank",ylab="Z Pos Score")
# abline(v=c(75,88),lty=2,col="darkgreen")
# ranks<-data.frame(z_rank = ap$z_rank)
# points(z_rank,predict(fit,newdata=ranks),col="darkgreen",lwd=2,type="l")
# ranks$predicted <- predict(fit,newdata=ranks)



#next I'll price them
#weight functions for entire player pool pricing
all_players1 <- all_players

#all_players1$z_unscaled <- all_players1$z_pos - min(all_players1$z_pos)  #this is not how to unscale
#NEED TO UNSCALE Z_POS
#exploratory analysis
ggplot(all_players1, aes(rank(z_pos), z_pos)) + geom_point()
#
B <- 1.25375
all_players1$price <- B ^ all_players1$z_unscaled + 2
sum(all_players1$price)
ggplot(all_players1, aes(rank(price), price)) + geom_point()

#transform prices
ggplot(all_players1, aes(z_pos)) + geom_histogram()
ggplot(starters2, aes(z_pos)) + geom_histogram()
library(grt)
all_players1$z_new <- unscale(all_players1$z_pos)
all_players1$z_new <- unscale(all_players1$z_tot)

#find hypothetical standard deviation of price
B <- 1.009838
all_players1$price <- B ^ rank(all_players1$z_pos) + 2
sum(all_players1$price)
ggplot(all_players1, aes(rank(price), price)) + geom_point()

all_players1$new_price <- 9.471 * all_players1$z_pos + 11.957
ggplot(all_players1, aes(rank(price), new_price)) + geom_point()

#need a new function for combining pitchers and relievers
z_score_pitchers <- function(df) {
  
  #wins, era, saves, ip, hr (allowed), so, whip, k/9, bb/9, fip, k/bb, avg, hld (missing), qs (missing)
  df$wins_z <- round(as.numeric(z_score(df$wins)), 3)
  df$saves_z <- round(as.numeric(z_score(df$saves)), 3)
  #df$era_z <- round(as.numeric(z_score(df$era) * -1), 3)
  #df$ip_z <- round(as.numeric(z_score(df$ip)), 3)
  #df$hra_z <- round(as.numeric(z_score(df$hra) * -1), 3)
  df$k_z <- round(as.numeric(z_score(df$so)), 3)
  #df$whip_z <- round(as.numeric(z_score(df$whip) * -1), 3)
  #df$k_rate_z <- round(as.numeric(z_score(df$k_rate)), 3)
  #df$bb_rate_z <- round(as.numeric(z_score(df$bb_rate) * -1), 3)
  #df$fip_z <- round(as.numeric(z_score(df$fip) * -1), 3)
  #df$kbb_rate_z <- round(as.numeric(z_score(df$kbb)), 3)
  #df$avg_p_z <- round(as.numeric(z_score(df$avg_p) * -1), 3)
  #df$hra_rate_z <- round(as.numeric(z_score(df$hra_rate) * -1), 3)
  #   
  df
}

#reset reliever z_scores on "reverse" stats to "0"
#do this for combining with starters, so they'll be graded on an equal plane
relievers1$era_z <- 0
relievers1$whip_z <- 0
relievers1$hra_z <- 0

#COMBINE starters and relievers
#Need to do an exploratory analysis of relation between stat and dollars for whip, era, and hra
#ggplot(starters1, aes(x = era_z)) + geom_histogram(bins = 20)

#y = (1.0184 ^ rank) - 1
# copy <- starters1
# copy$era_price <- 1.0184 ^ rank(-copy$era) - 1
# copy$whip_price <- 1.0184 ^ rank(-copy$whip) - 1
# copy$hra_price <- 1.0184 ^ rank(-copy$hra) - 1

starters1$era_price <- 1.0184 ^ rank(-starters1$era) - 1
starters1$whip_price <- 1.0184 ^ rank(-starters1$whip) - 1
starters1$hra_price <- 1.0184 ^ rank(-starters1$hra) - 1

#####
#join starters and relievers
all_pitchers <- starters1 %>%
  full_join(relievers1)

#get relievers era, whip, and hra scores
# all_pitchers1 <- all_pitchers %>%
#   select(name, team, era, whip, hra, ip, pos, era_price, whip_price, hra_price) %>%
#   arrange(era)#%>%


#RUN Z_SCORE METHOD

#COMPONENT METHOD on price
all_pitchers1 <- all_pitchers  #make a copy of all_pitchers

#assing nearest starter era value for reliever era value
all_pitchers1$new <- ifelse(is.na(all_pitchers1$era_price), na.locf(all_pitchers1$era_price, fromLast = TRUE), NA)
all_pitchers1$new1 <- ifelse(is.na(all_pitchers1$era_price), na.locf(all_pitchers1$era_price), NA)
all_pitchers1$new2 <- ifelse(is.na(all_pitchers1$era_price), (all_pitchers1$new + all_pitchers1$new1) / 2 * all_pitchers1$ip / mean(starters1$ip), NA)
all_pitchers1$era_price_new <- ifelse(!is.na(all_pitchers1$era_price), all_pitchers1$era_price * sum(all_pitchers1$era_price, na.rm = TRUE) /
                                        (sum(all_pitchers1$era_price, na.rm = TRUE) + sum(all_pitchers1$new2, na.rm = TRUE)),
                                      all_pitchers1$new2 * sum(all_pitchers1$era_price, na.rm = TRUE) / 
                                        (sum(all_pitchers1$era_price, na.rm = TRUE) + sum(all_pitchers1$new2, na.rm = TRUE)))
#remove place holder vars
all_pitchers1$new <- NULL
all_pitchers1$new1 <- NULL
all_pitchers1$new2 <- NULL

#assing nearest value WHIP
all_pitchers1 <- all_pitchers1 %>%
  arrange(whip)
all_pitchers1$new <- ifelse(is.na(all_pitchers1$whip_price), na.locf(all_pitchers1$whip_price, fromLast = TRUE), NA)
all_pitchers1$new1 <- ifelse(is.na(all_pitchers1$whip_price), na.locf(all_pitchers1$whip_price), NA)
all_pitchers1$new2 <- ifelse(is.na(all_pitchers1$whip_price), (all_pitchers1$new + all_pitchers1$new1) / 2 * all_pitchers1$ip / mean(starters1$ip), NA)
all_pitchers1$whip_price_new <- ifelse(!is.na(all_pitchers1$whip_price), all_pitchers1$whip_price * sum(all_pitchers1$whip_price, na.rm = TRUE) /
                                         (sum(all_pitchers1$whip_price, na.rm = TRUE) + sum(all_pitchers1$new2, na.rm = TRUE)),
                                       all_pitchers1$new2 * sum(all_pitchers1$whip_price, na.rm = TRUE) / 
                                         (sum(all_pitchers1$whip_price, na.rm = TRUE) + sum(all_pitchers1$new2, na.rm = TRUE)))
#remove place holder vars
all_pitchers1$new <- NULL
all_pitchers1$new1 <- NULL
all_pitchers1$new2 <- NULL

#assing nearest value hra; need hra_rate OR! multiply and create a virtual weighted HRA
#create weighted HRA

#SCRATCH THIS; WEIGHTS LOW IP STARTING PITCHERS HIGHLY BASED SOLELY ON INNINGS
all_pitchers1$hra_w <- ifelse(all_pitchers1$pos == "rp", all_pitchers1$hra * mean(starters1$ip) / all_pitchers1$ip, all_pitchers1$hra)

all_pitchers1 <- all_pitchers1 %>%
  arrange(hra_w)
all_pitchers1$new <- ifelse(is.na(all_pitchers1$hra_price), na.locf(all_pitchers1$hra_price, fromLast = TRUE), NA)
all_pitchers1$new1 <- ifelse(is.na(all_pitchers1$hra_price), na.locf(all_pitchers1$hra_price), NA)
all_pitchers1$new2 <- ifelse(is.na(all_pitchers1$hra_price), (all_pitchers1$new + all_pitchers1$new1) / 2 * all_pitchers1$ip / mean(starters1$ip), NA)
all_pitchers1$hra_price_new <- ifelse(!is.na(all_pitchers1$hra_price), all_pitchers1$hra_price * sum(all_pitchers1$hra_price, na.rm = TRUE) /
                                        (sum(all_pitchers1$hra_price, na.rm = TRUE) + sum(all_pitchers1$new2, na.rm = TRUE)),
                                      all_pitchers1$new2 * sum(all_pitchers1$hra_price, na.rm = TRUE) / 
                                        (sum(all_pitchers1$hra_price, na.rm = TRUE) + sum(all_pitchers1$new2, na.rm = TRUE)))

#remove place holder vars
all_pitchers1$new <- NULL
all_pitchers1$new1 <- NULL
all_pitchers1$new2 <- NULL
all_pitchers1$hra_w <- NULL
#all_pitchers1$hra_price_new <- NULL

#might be more accurate to do hra on hra_rate, then weight it by innings
# all_pitchers1 <- all_pitchers1 %>%
#   arrange(hra_rate)  #Don't need to arrange first in the df; rank() does that.

#run the exponential funciton on hra_rate
all_pitchers1$hra_rate_price <- 1.01188 ^ rank(-all_pitchers1$hra_rate) - 1
all_pitchers1$hra_rate_price <- all_pitchers1$hra_rate_price * all_pitchers1$ip / mean(all_pitchers1$ip)
#THIS WORKS BUT NEED TO REWEIGHT IT BY DOLLARS, which are less than the allocation: a small % change
all_pitchers1$hra_rate_price <- all_pitchers1$hra_rate_price * 318.4035 / sum(all_pitchers1$hra_rate_price)
#wins, saves, k's
all_pitchers1$wins_price <- 1.01188 ^ rank(all_pitchers1$wins) - 1
all_pitchers1$saves_price <- 1.01188 ^ rank(all_pitchers1$saves) - 1
all_pitchers1$k_price <- 1.01188 ^ rank(all_pitchers1$so) - 1

#add pitcher vars together
all_pitchers1$price <- all_pitchers1$wins_price + all_pitchers1$saves_price + all_pitchers1$k_price +
  all_pitchers1$era_price_new + all_pitchers1$whip_price_new + all_pitchers1$hra_rate_price
all_pitchers1 <- all_pitchers1 %>%
  arrange(desc(price))

#START new hitter pricing
hitters <- hitters_zpos2  #make a copy of last full hitters df

#stats: avg, hr, rbi, sb_net, runs, ops
hitters$avg_price <- 1.00914 ^ rank(hitters$avg) - 1
hitters$hr_price <- 1.00914 ^ rank(hitters$hr) - 1
hitters$rbi_price <- 1.00914 ^ rank(hitters$rbi) - 1
hitters$sb_net_price <- 1.00914 ^ rank(hitters$sb_net) - 1
hitters$runs_price <- 1.00914 ^ rank(hitters$runs) - 1
hitters$ops_price <- 1.00914 ^ rank(hitters$ops) - 1
#sum stats
hitters$price <- hitters$avg_price + hitters$hr_price + hitters$rbi_price + hitters$sb_net_price +
  hitters$runs_price + hitters$ops_price
hitters <- hitters %>%
  arrange(desc(price))

#This gives me prices that I don't really want
#This makes sense because I solely used the rank of each stat cat.

#new pitcher value pricing
# ggplot(hitters, aes(rank(hr), hr)) + geom_jitter()
# ggplot(hitters, aes(rank(avg), avg)) + geom_point()
# ggplot(hitters, aes(rank(rbi), rbi)) + geom_jitter()
# ggplot(hitters, aes(rank(sb_net), sb_net)) + geom_jitter()  #change this to box cox
# ggplot(hitters, aes(rank(runs), runs)) + geom_jitter()
# ggplot(hitters, aes(rank(ops), ops)) + geom_jitter()
ggplot(hitters, aes(rank(price), price)) + geom_point()
# ggplot(all_pitchers1, aes(rank(wins), wins)) + geom_jitter()
# ggplot(all_pitchers1, aes(rank(-era), era)) + geom_point()
# ggplot(all_pitchers1, aes(rank(-whip), whip)) + geom_point()
# ggplot(all_pitchers1, aes(rank(-hra_rate), hra_rate)) + geom_point()
# ggplot(all_pitchers1, aes(rank(saves), saves)) + geom_jitter()
# ggplot(all_pitchers1, aes(rank(so), so)) + geom_point()
# 
# ggplot(all_pitchers1, aes(so, rank(so))) + geom_point()
# ggplot(all_pitchers1, aes(-rank(era), -era)) + geom_point()
# ggplot(all_pitchers1, aes(-rank(hra_rate), -hra_rate)) + geom_point()
# 
# hist(all_pitchers1$so)
# 
# sum(all_pitchers1$so)

#run exponential function on ACTUAL value, not the rank of the value; this should produce more desirable results.
#K's
all_pitchers1$k_value <- (1.00866^all_pitchers1$so) - 1
#sum(all_pitchers1$k_value)
#ggplot(all_pitchers1, aes(x=so, y=k_value)) + geom_point()

#Wins
all_pitchers1$win_value <- (1.1419^all_pitchers1$wins) - 1
#sum(all_pitchers1$win_value)
#ggplot(all_pitchers1, aes(x=wins, y=win_value)) + geom_jitter()

#Saves
all_pitchers1$save_value <- (1.0916^all_pitchers1$saves) - 1
#sum(all_pitchers1$save_value)
#ggplot(all_pitchers1, aes(x=saves, y=save_value)) + geom_jitter()

#all_pitchers1$era_value <- (1/(1.3 ^ -all_pitchers1$era)) - 1
# all_pitchers1$era_value <- (13.046 ^ 1/all_pitchers1$era)-1
# all_pitchers1$era_value <- (23 ^ 1/all_pitchers1$era) - 3.5
# all_pitchers1$era_value <- (50 ^ 1/all_pitchers1$era) - 10  #this works the best of what i've tried, and sets era of 5.00 to 0 value
#the above were all linear

#ERA
B <- 1500
all_pitchers1$era_value <- (B ^ (1/all_pitchers1$era)) - B^.2  #sets 5.00 era to 0 value
#sum(all_pitchers1$era_value)
#ggplot(all_pitchers1, aes(x=era, y=era_value)) + geom_jitter()

B <- 15
all_pitchers1$whip_value <- (B ^ (1/all_pitchers1$whip)) - B^(1/1.5)  #this works the best of what i've tried, and sets whip of 1.50 to 0 value
#sum(all_pitchers1$whip_value)
#ggplot(all_pitchers1, aes(x=whip, y=whip_value)) + geom_jitter()

B <- 6.564
all_pitchers1$hra_rate_value <- (B ^ (1/all_pitchers1$hra_rate)) - B^(1/1.6)  #this works the best of what i've tried, and sets hra_rate of 1.6 hr/9 to 0 value
#sum(all_pitchers1$hra_rate_value)
#ggplot(all_pitchers1, aes(x=hra_rate, y=hra_rate_value)) + geom_jitter()

#weight rate stats by innings pitched.
#NO!!!!! If I'm already deprecating value for roster construction (which I am),
#I am penalizing relievers twice!
#DO NOT WEIGHT RATE STATS!
# all_pitchers1$era_value <- all_pitchers1$era_value * all_pitchers1$ip / mean(all_pitchers1$ip)
# all_pitchers1$whip_value <- all_pitchers1$whip_value * all_pitchers1$ip / mean(all_pitchers1$ip)
# all_pitchers1$hra_rate_value <- all_pitchers1$hra_rate_value * all_pitchers1$ip / mean(all_pitchers1$ip)
# #re-price rate stats
# all_pitchers1$era_value <- all_pitchers1$era_value * 366.67 / sum(all_pitchers1$era_value)
# all_pitchers1$whip_value <- all_pitchers1$whip_value * 366.67 / sum(all_pitchers1$whip_value)
# all_pitchers1$hra_rate_value <- all_pitchers1$hra_rate_value * 366.67 / sum(all_pitchers1$hra_rate_value)

#calculate value
all_pitchers1$value <- all_pitchers1$k_value + all_pitchers1$win_value + all_pitchers1$save_value +
  all_pitchers1$era_value + all_pitchers1$whip_value + all_pitchers1$hra_rate_value
#arrange by value
all_pitchers1 <- all_pitchers1 %>%
  arrange(desc(value))

#So as of here, the top relievers are priced WAY higher than the top pitchers; this is as it should be:
#relievers perform better than starters on a per inning basis. They throw harder because they are in for shorter
#stints. This results in lower whip and era.

# all_pitchers1$save_value <- (.33*all_pitchers1$saves)
# sum(all_pitchers1$save_value)
# ggplot(all_pitchers1, aes(x=saves, y=save_value)) + geom_jitter()
# 
# hist(all_pitchers1$saves)

#hitter exploratory graphs

# ggplot(hitters, aes(rank(hr), hr)) + geom_jitter()
# ggplot(hitters, aes(rank(avg), avg)) + geom_point()
# ggplot(hitters, aes(rank(rbi), rbi)) + geom_jitter()
# ggplot(hitters, aes(rank(sb_net), sb_net)) + geom_jitter()  #change this to box cox
# ggplot(hitters, aes(rank(runs), runs)) + geom_jitter()
# ggplot(hitters, aes(rank(ops), ops)) + geom_jitter()

hitters$sb_net_BC <- round(as.numeric(BoxCox(hitters$sb_net, .45) + 2.223), 3)
ggplot(hitters, aes(rank(sb_net), as.numeric(sb_net_BC))) + geom_jitter()

B <- 596
hitters$avg_value <- (B^hitters$avg) - B^.208  #Mendoza Line! (though these days .191 or so might be more right)
#sum(hitters$avg_value)
#ggplot(hitters, aes(avg, avg_value)) + geom_point()
#gives surprisingly little value to batting average and actually looks more linear than exponential.
#however, we want this because BA is less sticky than other stats
#a lower mendoza line will devalue BA further

B <- 1.0525
hitters$hr_value <- (B^hitters$hr) - B ^ 5  #using the current sample min, 5, as 0-value
#sum(hitters$hr_value)
#ggplot(hitters, aes(hr, hr_value)) + geom_point()
#the lower you make the 0-value threshold, the less you're valuing the stat, and the flatter the graph

# B <- 1.016817
# hitters$rbi_value <- (B^hitters$rbi) - B ^ 25
# sum(hitters$rbi_value)
# ggplot(hitters, aes(rbi, rbi_value)) + geom_point()

B <- 1.01922
hitters$rbi_value <- (B^hitters$rbi) - B ^ 40
#sum(hitters$rbi_value)
#ggplot(hitters, aes(rbi, rbi_value)) + geom_point()

B <- 1.2862
hitters$sb_net_BC_value <- round((B^hitters$sb_net_BC) - 1, 3)
#sum(hitters$sb_net_BC_value)
#ggplot(hitters, aes(sb_net_BC, sb_net_BC_value)) + geom_point()

B <- 1.01968
hitters$runs_value <- (B^hitters$runs) - B ^ 40
#sum(hitters$runs_value)
#ggplot(hitters, aes(runs, runs_value)) + geom_point()

B <- 13
hitters$ops_value <- (B^hitters$ops) - B^.7  #sets .700 ops to 0-value
sum(hitters$ops_value)
ggplot(hitters, aes(ops, ops_value)) + geom_point()

#add hitter values together
hitters$value <- hitters$avg_value + hitters$hr_value + hitters$rbi_value + hitters$sb_net_BC_value +
  hitters$runs_value + hitters$ops_value
#arrange by value
hitters <- hitters %>%
  arrange(desc(value))

#old exploratory analysis
# ggplot(all_pitchers1, aes(rank(wins), wins)) + geom_jitter()
# ggplot(all_pitchers1, aes(rank(-era), era)) + geom_point()
# ggplot(all_pitchers1, aes(rank(-whip), whip)) + geom_point()
# ggplot(all_pitchers1, aes(rank(-hra_rate), hra_rate)) + geom_point()
# ggplot(all_pitchers1, aes(rank(saves), saves)) + geom_jitter()
# ggplot(all_pitchers1, aes(rank(so), so)) + geom_point()
# 
# ggplot(all_pitchers1, aes(so, rank(so))) + geom_point()
# ggplot(all_pitchers1, aes(-rank(era), -era)) + geom_point()
# ggplot(all_pitchers1, aes(-rank(hra_rate), -hra_rate)) + geom_point()
# 
# hist(all_pitchers1$so)
# 
# sum(all_pitchers1$so)

#duplicated code
# all_pitchers1$k_value <- (1.00866^all_pitchers1$so) - 1
# sum(all_pitchers1$k_value)
# ggplot(all_pitchers1, aes(x=so, y=k_value)) + geom_point()
# 
# all_pitchers1$win_value <- (1.1419^all_pitchers1$wins) - 1
# sum(all_pitchers1$win_value)
# ggplot(all_pitchers1, aes(x=wins, y=win_value)) + geom_jitter()
# 
# all_pitchers1$save_value <- (1.0916^all_pitchers1$saves) - 1
# sum(all_pitchers1$save_value)
# ggplot(all_pitchers1, aes(x=saves, y=save_value)) + geom_jitter()

#all_pitchers1$era_value <- (1/(1.3 ^ -all_pitchers1$era)) - 1
# all_pitchers1$era_value <- (13.046 ^ 1/all_pitchers1$era)-1
# all_pitchers1$era_value <- (23 ^ 1/all_pitchers1$era) - 3.5
# all_pitchers1$era_value <- (50 ^ 1/all_pitchers1$era) - 10  #this works the best of what i've tried, and sets era of 5.00 to 0 value
#the above were all linear
# B <- 1500
# all_pitchers1$era_value <- (B ^ (1/all_pitchers1$era)) - B^.2
# sum(all_pitchers1$era_value)
# ggplot(all_pitchers1, aes(x=era, y=era_value)) + geom_jitter()
# 
# B <- 15
# all_pitchers1$whip_value <- (B ^ (1/all_pitchers1$whip)) - B^(1/1.5)  #this works the best of what i've tried, and sets era of 5.00 to 0 value
# sum(all_pitchers1$whip_value)
# ggplot(all_pitchers1, aes(x=whip, y=whip_value)) + geom_jitter()
# 
# B <- 10.295
# all_pitchers1$hra_rate_value <- (B ^ (1/all_pitchers1$hra_rate)) - B^(1/1.6)  #this works the best of what i've tried, and sets era of 5.00 to 0 value
# sum(all_pitchers1$hra_rate_value)
# ggplot(all_pitchers1, aes(x=hra_rate, y=hra_rate_value)) + geom_jitter()
# 
# #weight rate stats
# all_pitchers1$era_value <- all_pitchers1$era_value * all_pitchers1$ip / mean(all_pitchers1$ip)
# all_pitchers1$whip_value <- all_pitchers1$whip_value * all_pitchers1$ip / mean(all_pitchers1$ip)
# all_pitchers1$hra_rate_value <- all_pitchers1$hra_rate_value * all_pitchers1$ip / mean(all_pitchers1$ip)
# #re-price rate stats
# all_pitchers1$era_value <- all_pitchers1$era_value * 366.67 / sum(all_pitchers1$era_value)
# all_pitchers1$whip_value <- all_pitchers1$whip_value * 366.67 / sum(all_pitchers1$whip_value)
# all_pitchers1$hra_rate_value <- all_pitchers1$hra_rate_value * 366.67 / sum(all_pitchers1$hra_rate_value)
# #calculate value
# all_pitchers1$value <- all_pitchers1$k_value + all_pitchers1$win_value + all_pitchers1$save_value +
#   all_pitchers1$era_value + all_pitchers1$whip_value + all_pitchers1$hra_rate_value
# #arrange by value
# all_pitchers1 <- all_pitchers1 %>%
#   arrange(desc(value))
# all_pitchers1$save_value <- (.33*all_pitchers1$saves)
# sum(all_pitchers1$save_value)
# ggplot(all_pitchers1, aes(x=saves, y=save_value)) + geom_jitter()
# 
# hist(all_pitchers1$saves)

#weight hitters, starting pitchers, and relievers by roster slots
hitters$value_new <- hitters$value * 13/23
all_pitchers1$value_new <- ifelse(all_pitchers1$pos == "sp", all_pitchers1$value * 7.5/23, all_pitchers1$value * 2.5/23)

#combine hitters and pitchers

#1.) value only for exploration
#subset to players I need
all_pitchers2 <- all_pitchers1 %>%
  select(name, team, pos, value_new)
hitters1 <- hitters %>%
  select(name, team, pos, value_new)
ap <- all_pitchers2 %>%
  full_join(hitters1) %>%
  mutate(auction_value = value_new * 4400 / sum(value_new)) %>%  #re-weight to field of prices
  arrange(desc(auction_value))

#exploratory analysis
sum(ap$auction_value)
ggplot(ap, aes(x = rank(auction_value), y = auction_value)) + geom_point()
#ggplot(all_pitchers1, aes(saves > 0)) + geom_histogram()

#2.) get original z_score pricing
#re-weight that pricing for $4400
#combine it with the component value pricng

#value players by position
first_basemen <- filter(ap, pos == "3")
second_basemen <- filter(ap, pos == "4")
names(ap)


#combine hitters and pitchers
#subset to players I need
all_pitchers2 <- all_pitchers1 %>%
  select(name, team, pos, price)
hitters1 <- hitters %>%
  select(name, team, pos, price)
ap <- all_pitchers2 %>%
  full_join(hitters1) %>%
  arrange(desc(price))

#find player function
fname <- function(name) {
  which(ap$name == name)
}

#exploratory analysis
ggplot(ap, aes(x = rank(price), y = price)) + geom_point()
ggplot(all_pitchers1, aes(saves > 0)) + geom_histogram()
#
df <- all_pitchers
stat1 <- df["wins_z"]
stat2 <- df["saves_z"]
stat3 <- df["k_z"]
stat4 <- 0
stat5 <- 0
stat6 <- 0

all_pitchers["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
all_pitchers$z_pos <- all_pitchers$z_tot + all_pitchers$z_pos_mean

all_pitchers <- all_pitchers %>%
  arrange(desc(z_tot))

#Here's where to do some dollar v stat analysis


#####
all_pitchers1 <- all_pitchers[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", 
                                  names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]

#combine pitchers and position players
all_players <- hitters3 %>%
  full_join(all_pitchers1) %>%
  arrange(desc(z_pos))

#remove old pitcher dfs
rm(relievers, relievers2, starters, starters2)
#remove coding dfs
rm(df, stat1, stat2, stat3, stat4, stat5, stat6)
#function for "finding names" in the full df
fname <- function(name){
  which(all_players$name == name)
}

#rank players in final league df
#subset out the last player on each team, then run rank function on the remaining players
all_players1 <- all_players[1:(nrow(all_players)-n_teams),]
all_players1$rank <- rank(all_players1$z_pos)
#join ranks with df
all_players <- all_players %>%
  left_join(all_players1)
rm(all_players1)

#cost function
#for $275 and 16 team league
cost_fn <- function(rank, min_cost = 3) {
  round(1.01187 ^ rank + (min_cost - 1), 2)
}
#assign cost to player ranks
all_players$rank_cost <- cost_fn(all_players$rank)
#assign min cost to last players
all_players$rank_cost <- ifelse(is.na(all_players$rank_cost), min_cost, all_players$rank_cost)

all_players$rank_cost_sc <- round(scale(all_players$rank_cost), 3)
all_players$z_pos_sc <-round(scale(all_players$z_pos), 3)

# ggplot(all_players, aes(rank_cost_sc, z_pos_sc)) + geom_point()
# ggplot(all_players, aes(z_pos, z_pos_sc)) + geom_point()
# ggplot(all_players, aes(rank, z_pos)) + geom_point()
# ggplot(all_players, aes(rank, rank_cost_sc)) + geom_point()
# ggplot(all_players, aes(rank_cost, new_cost)) + geom_point()
# ggplot(all_players, aes(new_cost, z_pos)) + geom_point()
# ggplot(all_pitchers, aes(whip_z)) + geom_histogram(bins = 25)
# ggplot(all_pitchers, aes(hra)) + geom_histogram(bins = 30)
# ggplot(all_pitchers, aes(hra_z)) + geom_histogram(bins = 30)
# ggplot(hitters_zpos2, aes(pa, z_pos)) + geom_point()
# ggplot(starters1, aes(ip, z_tot)) + geom_point()
#notes need to separate starters and relievrs as i've done to cut down to ~6 starters and 3 relievers per team
#then, combine the starters and relievers and run the z_stats
#right now, degrom and kimbrel have the same k_z scores even after weighting relievers by the mean starters.
#kimbrel is projected for 100 K's and degrom is projected for ~220, so they should have drastically different scores.
#might need to again use k_rate in the combined df, then weight by innings pitched.
#as of now, i am essentially creating two different stats, relievers_k_z and starters_k_z, and calling them the same thing.

all_players1 <- all_players %>%
  filter(z_pos_sc > 0, z_pos_sc < 2.5)
lm(all_players1$rank_cost_sc ~ all_players1$z_pos_sc)

#y = mx + b
#rank_cost_sc <- -.1168 + 1.1994 * z_pos_sc

new_fn <- function(z_pos_sc) {
  round(-.1748 + 1.2430 * z_pos_sc, 3)
}

all_players$new_rank_cost_sc <- new_fn(all_players$z_pos_sc)

all_players$new_cost <- round(mean(all_players$rank_cost) + all_players$new_rank_cost_sc * sd(all_players$rank_cost), 2)
all_players$new_cost <- ifelse(all_players$rank_cost_sc <= 0, all_players$rank_cost, all_players$new_cost)

all_players1 <- all_players %>%
  select(name, team, pos, z_pos, z_pos_mean, z_tot, new_cost)
# all_players1$new_cost <- round(all_players1$new_cost, 2)

# b = exp(log(y) / x)
#make equation of actual var names for b_coef
sum(all_players1$new_cost)

#saveRDS(all_players1, file = "~/Desktop/R_projects/baseball/eiflb/projections_2018_0126")


catchers1 <- all_players %>% filter(pos == 2) %>% select(-c(wins_z:saves_z))
first_basemen1 <- all_players %>% filter(pos == 3) %>% select(-c(wins_z:saves_z))
second_basemen1 <- all_players %>% filter(pos == 4) %>% select(-c(wins_z:saves_z))
third_basemen1 <- all_players %>% filter(pos == 5) %>% select(-c(wins_z:saves_z))
shortstops1 <- all_players %>% filter(pos == 6) %>% select(-c(wins_z:saves_z))
outfielders1 <- all_players %>% filter(pos == 7) %>% select(-c(wins_z:saves_z))
middle_infielders1 <- all_players %>% filter(pos == 4 | pos == 6) %>% select(-c(wins_z:saves_z))
corner_infielders1 <- all_players %>% filter(pos == 3 | pos == 5) %>% select(-c(wins_z:saves_z))

find_name <- function(name) {
  which(all_players1$name == name)
}
