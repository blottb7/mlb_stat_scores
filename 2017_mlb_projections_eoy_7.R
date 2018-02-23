
#Code for generalized yearlong category data

#TO DO
#Z_TOT function: add all possible stats; will need to build in zero's when an individual stat not selected
#z_stat: allow user define. start ~line 215
#user selected stats
#add bench players? how to weight them?
#what to do when two similar positions (i.e. SS and 2B) have too many players assigned to them
#best gamma parameter for SB and SB_net
#I'm going to need two sets of pitcher dfs... one for leagues that require sp and rp, and one that does not have designations, but has inning requirements
#remove "old" df's as you go along

#Set working directory
#setwd("~/Desktop/R_projects/baseball/eiflb")  #apple
#setwd("C:/Users/Ben/Desktop/FF/baseball")  #asus
#setwd("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl")  #working directory for toshiba laptop
setwd("C:/Users/Ben/Desktop/R projects")  #new toshiba working directory

#libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)

#user settings
#number of teams, and number of starters at each position for a given fantasy league
n_teams <- 16
starting_catchers <- 1
starting_first_basemen <- 1
starting_second_basemen <- 1
starting_third_basemen <- 1
starting_shortstops <- 1
starting_outfielders <- 4
starting_middle_infielders <- 1
starting_corner_infielders <- 1
starting_designated_hitters <- 1
#pitchers
starting_pitchers <- 6.5
relief_pitchers <- 2.5
#bench players
bench_players <- 2
#costs
min_cost <- 3

#staring players per position for a fantasy league
n_catchers <- n_teams * starting_catchers
n_first_basemen <- n_teams * starting_first_basemen
n_second_basemen <- n_teams * starting_second_basemen
n_third_basemen <- n_teams * starting_third_basemen
n_shortstops <- n_teams * starting_shortstops
n_outfielders <- n_teams * starting_outfielders
n_middle_infielders <- n_teams * starting_middle_infielders
n_corner_infielders <- n_teams * starting_corner_infielders
n_designated_hitters <- n_teams * starting_designated_hitters
#pitchers
n_starting_pitchers <- n_teams * starting_pitchers
n_relief_pitchers <- n_teams * relief_pitchers
#bench
n_bench <- n_teams * bench_players

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
  full_join(outfielders) %>%
  filter(pa > 1)  #get this done out of the gate; removes players who have a "token" projection (not expected to play in MLB)

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
hitters$pos <- ifelse(hitters$name == "Matt Carpenter", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Josh Harrison", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Eduardo Nunez", 6, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Starlin Castro", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Jonathan Villar", 4, hitters$pos)
hitters$pos <- ifelse(hitters$name == "Tim Beckham", 4, hitters$pos)
# hitters$pos <- ifelse(hitters$name == "Ben Zobrist", 4, hitters$pos)
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

#keep "regulars", those players who are going to start more days than not
hitters_reg <- hitters %>%
  filter(pa >= 300) %>%  #will not want players with less than half a season of at bats, so filter for this
  arrange(name)

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
catchers2 <- catchers2[1:n_catchers,]  #keep only the amount of catchers warranted for the league

#repeat this process for each position

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
first_basemen2 <- first_basemen2[1:n_first_basemen,]

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
second_basemen2 <- second_basemen2[1:n_second_basemen,]

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
third_basemen2 <- third_basemen2[1:n_third_basemen,]

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
shortstops2 <- shortstops2[1:n_shortstops,]

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
outfielders2 <- outfielders2[1:n_outfielders,]

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
middle_infielders2 <- middle_infielders2[1:n_middle_infielders,]

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
corner_infielders2 <- corner_infielders2[1:n_corner_infielders,]

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
designated_hitters1 <- designated_hitters1[1:(n_designated_hitters + n_bench / 2),]

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
  arrange(desc(z_tot))

#remove unncessary df's
rm(catchers1, catchers2, corner_infielders, corner_infielders1, corner_infielders2,
   designated_hitters1, first_basemen1, first_basemen2, hitters_no_dh, hitters1, hitters2,
   middle_infielders1, middle_infielders2, outfielders1, outfielders2, remaining_hitters,
   second_basemen1, second_basemen2, shortstops1, shortstops2, third_basemen1,
   third_basemen2)

find_name <- function(name) {
  which(hitters3$name == name)
}

##### PITCHERS #####

names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
                     "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "adp", "player_id")

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
starters <- pitchers %>%
  filter(gs > 0) %>%
  filter(ip >= 100)
starters$pos <- "sp"
relievers <- pitchers %>%
  filter(gs == 0)
relievers$pos <- "rp"

#apply z_score to starters
starters <- z_score_starters(starters)

df <- starters
stat1 <- df["wins_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_z"]
stat6 <- 0

starters["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
starters <- starters %>%
  arrange(desc(z_tot))
starters1 <- starters[1:n_starting_pitchers,]

#####
#Now run the same process with the starting pitchers in the actual playing pool
starters1 <- z_score_starters(starters1)

df <- starters1
stat1 <- df["wins_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_z"]
stat6 <- 0

starters1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
starters1 <- starters1 %>%
  arrange(desc(z_tot))

starters1$z_pos <- starters1$z_tot
starters1$z_pos_mean <- 0
starters2 <- starters1[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", 
                           names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]


relievers <- z_score_relievers(relievers)

#user selected vars
df <- relievers
stat1 <- df["saves_z"]
stat2 <- df["era_z"]
stat3 <- df["whip_z"]
stat4 <- df["k_z"]
stat5 <- df["hra_z"]
stat6 <- 0

relievers["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)

#select top relievers based on most saves, with z_tot being the tie-breaker
relievers <- relievers %>%
  #arrange(desc(z_tot))
  arrange(desc(saves_z), desc(z_tot))
#arrange(desc(as.numeric(saves_z)), desc(z_tot))
relievers1 <- relievers[1:n_relief_pitchers,]

#####
#Now run the same process with the rlief pitchers in the actual playing pool
relievers1 <- z_score_relievers(relievers1)

relievers1$z_pos <- relievers1$z_tot
relievers1$z_pos_mean <- 0
relievers2 <- relievers1[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", 
                             names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]


#COMBINE HITTERS WITH PITCHERS
all_pitchers <- starters1 %>%
  full_join(relievers1)
all_pitchers <- z_score_pitchers(all_pitchers)

df <- all_pitchers
stat1 <- df["wins_z"]
stat2 <- df["saves_z"]
stat3 <- df["era_z"]
stat4 <- df["whip_z"]
stat5 <- df["k_z"]
stat6 <- df["hra_rate_z"]

all_pitchers["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
all_pitchers$z_pos <- all_pitchers$z_tot + all_pitchers$z_pos_mean

all_pitchers <- all_pitchers %>%
  arrange(desc(z_tot))

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

