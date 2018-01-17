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
setwd("~/Desktop/R_projects/baseball/eiflb")  #apple
#setwd("C:/Users/Ben/Desktop/FF/baseball")  #asus
#setwd("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl")  #working directory for toshiba laptop

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
starting_pitchers <- 7
relief_pitchers <- 2.5

#user selected stats
#from fangraphs:
#avg, rbi, r, sb, hr, obp, slg, ops, h, so, 2b, 3b, tb, bb, rbi+r, xBH, sb-cs, woba (hitter possibilities)
#w, sv, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs (pitcher possibilities)

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

#FUNCTIONS
#z_score calculation for each selected stat
z_score <- function(stat){
  scale(stat)
}

#read in data
catchers <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 2)
first_basemen <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 3)
second_basemen <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 4)
third_basemen <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 5)
shortstops <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 6)
outfielders <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 7)
# pitchers <- read_excel("2018_fangraphs_projections_2018_0110.xlsx", sheet = 8)

#rename position player vars
#name_vector for 2018 df; does not include column: "adp". Include "adp" if and when necessary.
name_vector <- c("name", "team", "games", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb", "so",
                 "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
                 "waste3", "offense", "defense", "war", "playerid")

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

#start searching for multi-position players
hitters_names <- as.data.frame(hitters$name)  #creates a single col df of all starters' names
names(hitters_names) <- "name"  #names the col in above df

#creates an Nx1 df of duplicated names
duplicated_names <- as.data.frame(hitters_names[duplicated(hitters_names),])
names(duplicated_names) <- "name"  #names the col in above df

#creates full duplicated names df
duplicated_names1 <- hitters %>%
  right_join(duplicated_names, by = "name")
#make a copy of this for anti_join with full hitters
duplicated_names_copy <- duplicated_names1
duplicated_names1$pos <- as.numeric(duplicated_names1$pos)

duplicated_names1 <- unique(duplicated_names1)  #this removes rows where the name AND position are duplicated

#create vector of position rankings, with the least productive position (catcher) having the highest relative value
#and assiging the most valuable position to each duplicate player
#call that new var "pos_rank"
duplicated_names1$pos_rank <- ifelse(duplicated_names1$pos == 2, 1,
                                     ifelse(duplicated_names1$pos == 6, 2, 
                                            ifelse(duplicated_names1$pos == 4, 3,
                                                   ifelse(duplicated_names1$pos == 7, 4,
                                                          ifelse(duplicated_names1$pos == 3, 5, 6)))))
#assign pos_rank to each duplicate player
duplicated_names2 <- duplicated_names1 %>%
  select(name, pos_rank) %>%
  group_by(name) %>%
  summarize(pos_rank = min(pos_rank))
#join the position rank df with the now singular duplicated player df
duplicated_names3 <- duplicated_names1 %>%
  right_join(duplicated_names2) %>%
  select(-pos_rank)  #remove the no longer needed pos_rank var
duplicated_names3$pos <- as.character(duplicated_names3$pos)  #switch back to char class for joining with rest of hitters
#combine dfs so there is only one line for each player; each player now has the most valuable position
hitters <- hitters %>%
  anti_join(duplicated_names_copy) %>%
  bind_rows(duplicated_names3) %>%
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
#save the discarded hitters for later comparison, i.e. for guys you may want to stream or add later in the year
hitters_res <- hitters %>%
  filter(pa < 300) %>%
  arrange(desc(woba))

#do SB related stats across entire population; don't want position-relative scores for low sb positions like catcher.
hitters_reg$sb_z <- round(as.numeric(z_score(BoxCox(hitters_reg$sb, .45))), 3)
hitters_reg$sb_net_z <- round(as.numeric(z_score(BoxCox(hitters_reg$sb_net, .45))), 3)

#separate hitters by position
catchers1 <- hitters_reg %>% filter(pos == "2")
first_basemen1 <- hitters_reg %>% filter(pos == "3")
second_basemen1 <- hitters_reg %>% filter(pos == "4")
third_basemen1 <- hitters_reg %>% filter(pos == "5")
shortstops1 <- hitters_reg %>% filter(pos == "6")
outfielders1 <- hitters_reg %>% filter(pos == "7")

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

#run z-score pos on each position df
catchers1 <- z_score_hitter(catchers1)
first_basemen1 <- z_score_hitter(first_basemen1)
second_basemen1 <- z_score_hitter(second_basemen1)
third_basemen1 <- z_score_hitter(third_basemen1)
shortstops1 <- z_score_hitter(shortstops1)
outfielders1 <- z_score_hitter(outfielders1)

#function for user selected hitter stats
# z_total <- function(hit_stat1, hit_stat2, hit_stat3, hit_stat4, hit_stat5, hit_stat6) {
#   z_tot <- hit_stat1 + hit_stat2 + hit_stat3 + hit_stat4 + hit_stat5 + hit_stat6
# }

z_total <- function(stat1, stat2, stat3, stat4, stat5, stat6) {
  z_tot <- (stat1 + stat2 + stat3 + stat4 + stat5 + stat6)
}

df <- catchers1
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

catchers1["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)
catchers2 <- catchers1 %>%
  arrange(desc(z_tot))
catchers2 <- catchers2[1:n_catchers,]

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

#create middle infielders df and run z-score on middle infielders after removing already used players
middle_infielders <- second_basemen1 %>%
  bind_rows(shortstops1) %>%
  anti_join(second_basemen2, by = "name") %>%
  anti_join(shortstops2, by = "name")
middle_infielders1 <- z_score_hitter(middle_infielders)

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

#create corner infielders df and run z-score on corner infielders after removing already used players
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
                           middle_infielders2, corner_infielders2)

remaining_hitters <- hitters_reg %>%
  anti_join(hitters_no_dh, by = "name")

#create designated hitters
#this needs to be fixed; "sb" not found √
designated_hitters <- z_score_hitter(remaining_hitters)

df <- designated_hitters
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

designated_hitters["z_tot"] <- z_total(stat1, stat2, stat3, stat4, stat5, stat6)

#designated_hitters$z_tot <- z_total(designated_hitters$hr_z, designated_hitters$runs_z, designated_hitters$rbi_z, designated_hitters$avg_z, 0, designated_hitters$sb_z)
designated_hitters1 <- designated_hitters %>%
  arrange(desc(z_tot))
designated_hitters1 <- designated_hitters1[1:n_designated_hitters,]

# #remove dummy df
# rm(df)
##### #####
hitters1 <- bind_rows(hitters_no_dh, designated_hitters1)  #bind "designated hitters"/utility players to rest of hitters
hitters1$sb_z <- round(as.numeric(z_score(BoxCox(hitters1$sb, .45))), 3)  #generate sb related z_score
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
hitters_zpos <- hitters2 %>%
  group_by(pos) %>%
  summarize(z_pos_mean = round(mean(z_tot), 2)) %>%
  arrange(desc(z_pos_mean))

hitters_zpos1 <- hitters2 %>%
  left_join(hitters_zpos, by = "pos") %>%
  mutate(z_pos = round(z_tot - z_pos_mean, 4)) %>%
  arrange(desc(z_pos))

hitters_zpos2 <- hitters_zpos1 %>%
  arrange(desc(z_tot))

df <- hitters_zpos2
stat1 <- df["hr_z"]
stat2 <- df["runs_z"]
stat3 <- df["rbi_z"]
stat4 <- df["avg_z"]
stat5 <- df["sb_net_z"]
stat6 <- df["ops_z"]

hitters3 <- hitters_zpos2[, c("name", "team", "pos", "z_pos", "z_pos_mean", "z_tot", names(stat1), names(stat2), names(stat3), names(stat4), names(stat5), names(stat6))]

#remove unneeded df's
rm(df)
rm(stat1, stat2, stat3, stat4, stat5, stat6)
rm(catchers1, first_basemen1, second_basemen1, third_basemen1, shortstops1, outfielders1, middle_infielders1, corner_infielders1,
   designated_hitters1, hitters1)
rm(catchers2, first_basemen2, second_basemen2, third_basemen2, shortstops2, outfielders2, middle_infielders2, corner_infielders2,
   hitters2)
#FINISHED hitters for shiny
#####
first_basemen3 <- hitters3 %>%
  filter(pos == 3)
second_basemen3 <- hitters3 %>%
  filter(pos == 4)
shortstops3 <- hitters3 %>%
  filter(pos == 6)

#COMBINE HITTERS WITH PITCHERS
all_pitchers <- starters2 %>%
  full_join(relievers2)
all_players <- hitters3 %>%
  full_join(all_pitchers) %>%
  arrange(desc(z_pos))
all_players$z_pos <- round(all_players$z_pos, 3)
all_players$z_tot <- round(all_players$z_tot, 3)

# fname <- function(name){
#   which(all_players$name == name)
# }
# 
