#Code for generalized yearlong category data

#TO DO

#do any stat mutations before z_score_position √
#check for duplicate names √
  #create a dataframe of duplicates √
    #need to select out duplicate players at the beginning √
      #for now, due this by position: 5 > 3 > 7 > > 4 > 6 > 2 √
#merge duplicated_names3 (unique names) with hitters_new (in progress) √
  #probably need to remove all the duplicated names from hitters_new, then full_join it with the duplicate names √
#get rid of pos = "10" (DH); matchup players selected as designated hitters by their actual positions √

#pitchers: next!

#create criteria to assign a multi-position player to his most valuable position
#user selected stats
#convert
#add bench players? how to weight them?
#explore other scale_ fn's
#all stat distributions look like some type of poisson distribution:
  #find the R function for self selection of poisson distribution parameters

setwd("~/Desktop/R_projects/baseball/eiflb")  #set wd

#libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

#user settings
#number of teams, and number of starters at each position
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

#user selected stats
#i.e. avg, runs, hr, rbi, ops, sb_net

#staring players per position
n_catchers <- n_teams * starting_catchers
n_first_basemen <- n_teams * starting_first_basemen
n_second_basemen <- n_teams * starting_second_basemen
n_third_basemen <- n_teams * starting_third_basemen
n_shortstops <- n_teams * starting_shortstops
n_outfielders <- n_teams * starting_outfielders
n_middle_infielders <- n_teams * starting_middle_infielders
n_corner_infielders <- n_teams * starting_corner_infielders
n_designated_hitters <- n_teams * starting_designated_hitters

#FUNCTIONS
#z_score calculation for each selected stat
z_score <- function(stat){
  scale(stat)
}

#sb_net calculation
sb_net_fn <- function(sb, cs) {
  sb_net = sb - cs
}

#create general form of function for creating z-score position
z_score_position <- function(df, n_df) {
  df <- df %>% 
    filter(pa > 1) %>%  #the majority of the df's contain players with 1 pa, presumably for ratio data if they do get "called up"
    #mutate(sb_net = sb - cs) %>%
    select(name, team, pos, games, pa, avg, runs, hr, rbi, sb_net, ops)
  
  df$avg_z <- as.numeric(z_score(df$avg))
  df$runs_z <- as.numeric(z_score(df$runs))
  df$hr_z <- as.numeric(z_score(df$hr))
  df$rbi_z <- as.numeric(z_score(df$rbi))
  df$ops_z <- as.numeric(z_score(df$ops))
  
  df <- df %>%
    mutate(z_score = avg_z + runs_z + hr_z + rbi_z + ops_z) %>%
    arrange(desc(z_score)) %>%
    head(n_df)
}

#read in data frames (from fangraphs steamer projections via excel)
#all_hitters <- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls")
catchers <- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls", sheet = 2)
first_basemen <- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls", sheet = 3)
second_basemen<- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls", sheet = 4)
third_basemen <- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls", sheet = 5)
shortstops <- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls", sheet = 6)
outfielders <- read_excel("~/Desktop/R_projects/baseball/eiflb/2017_mlb_projections_eoy.xls", sheet = 7)

#rename the vars
name_vector <- c("name", "team", "games", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb", "so",
                 "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
                 "waste3", "offense", "defense", "war", "waste4", "adp", "playerid")

#assign the name vector to each position df
#names(all_hitters) <- name_vector
names(catchers) <- name_vector
names(first_basemen) <- name_vector
names(second_basemen) <- name_vector
names(shortstops) <- name_vector
names(third_basemen) <- name_vector
names(outfielders) <- name_vector

#the playerid reads in different classes depending on the position, so coerse the class to the same
#all_hitters$playerid <- as.character(all_hitters$playerid)
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
hitters_new <- catchers %>%
  full_join(first_basemen) %>%
  full_join(second_basemen) %>%
  full_join(third_basemen) %>%
  full_join(shortstops) %>%
  full_join(outfielders)

#start searching for multi-position players
hitters_names <- as.data.frame(hitters_new$name)  #creates a single col df of all starters' names
names(hitters_names) <- "name"  #names the col in above df
hitters_names <- hitters_names %>%  #arrange alphabetically so you can look for duplicates
  arrange(name)

duplicated_names <- as.data.frame(hitters_names[duplicated(hitters_names),])  #creates a df of duplicated names
names(duplicated_names) <- "name"  #names the col in above df

duplicated_names1 <- hitters_new %>%
  right_join(duplicated_names, by = "name")
duplicated_names_copy <- duplicated_names1  #save this for anti_join with full hitters
duplicated_names1$pos <- as.numeric(duplicated_names1$pos)

duplicated_names1 <- unique(duplicated_names1)  #this removes rows where the name AND position are duplicated

#create vector of position rankings, with the least productive position, catcher, having the most value
duplicated_names1$pos_rank <- ifelse(duplicated_names1$pos == 2, 1,
                                     ifelse(duplicated_names1$pos == 6, 2, 
                                            ifelse(duplicated_names1$pos == 4, 3,
                                                   ifelse(duplicated_names1$pos == 7, 4,
                                                          ifelse(duplicated_names1$pos == 3, 5, 6)))))

duplicated_names2 <- duplicated_names1 %>%
  select(name, pos_rank) %>%
  group_by(name) %>%
  summarize(pos_rank = min(pos_rank))

duplicated_names3 <- duplicated_names1 %>%
  right_join(duplicated_names2) %>%
  select(-pos_rank)
duplicated_names3$pos <- as.character(duplicated_names3$pos)  #switch back to char class for joining with rest of hitters

hitters_new <- hitters_new %>%
  anti_join(duplicated_names_copy) %>%
  bind_rows(duplicated_names3) %>%
  filter(pa > 1)
  arrange(name)

#duplicated_names4 <- duplicated_names3[duplicated(duplicated_names3),]
#duplicated_names5 <- duplicated_names3 %>%
#  anti_join(duplicated_names4) %>%
#  full_join(duplicated_names4)
###

catchers1 <- hitters_new %>% filter(pos == "2")
first_basemen1 <- hitters_new %>% filter(pos == "3")
second_basemen1 <- hitters_new %>% filter(pos == "4")
third_basemen1 <- hitters_new %>% filter(pos == "5")
shortstops1 <- hitters_new %>% filter(pos == "6")
outfielders1 <- hitters_new %>% filter(pos == "7")

#run sb_net fn for each position
catchers1$sb_net <- sb_net_fn(catchers1$sb, catchers1$cs)
first_basemen1$sb_net <- sb_net_fn(first_basemen1$sb, first_basemen1$cs)
second_basemen1$sb_net <- sb_net_fn(second_basemen1$sb, second_basemen1$cs)
third_basemen1$sb_net <- sb_net_fn(third_basemen1$sb, third_basemen1$cs)
shortstops1$sb_net <- sb_net_fn(shortstops1$sb, shortstops1$cs)
outfielders1$sb_net <- sb_net_fn(outfielders1$sb, outfielders1$cs)
hitters_new$sb_net <- sb_net_fn(hitters_new$sb, hitters_new$cs)

#remove dual position players from "weaker" position category; i.e. remove Starlin Castro from "2B" and leave in "SS"
#first_basemen <- first_basemen %>%
#  filter(name != "Brandon Moss", name != "Steve Pearce")
#second_basemen <- second_basemen %>%
#  filter(name != "Brad Miller", name != "Jean Segura", name != "Starlin Castro", name != "Jonathan Villar", name != "Javier Baez")
#third_basemen <- third_basemen %>%
#  filter(name != "Jedd Gyorko", name != "Jose Ramirez", name != "Matt Carpenter", name != "Yangervis Solarte")
#outfielders <- outfielders %>%
#  filter(name != "Ben Zobrist", name != "Ian Desmond", name != "Howie Kendrick")




#run z-score pos on each position df
catchers2 <- z_score_position(catchers1, n_catchers)
first_basemen2 <- z_score_position(first_basemen1, n_first_basemen)
second_basemen2 <- z_score_position(second_basemen1, n_second_basemen)
third_basemen2 <- z_score_position(third_basemen1, n_third_basemen)
shortstops2 <- z_score_position(shortstops1, n_shortstops)
outfielders2 <- z_score_position(outfielders1, n_outfielders)
hitters_new1 <- z_score_position(hitters_new, nrow(hitters_new))

#create middle infielders df and run z-score on middle infielders after removing already used players
middle_infielders <- second_basemen1 %>%
  bind_rows(shortstops1) %>%
  anti_join(second_basemen2, by = "name") %>%
  anti_join(shortstops2, by = "name")
middle_infielders1 <- z_score_position(middle_infielders, n_middle_infielders)

#create corner infielders df and run z-score on corner infielders after removing already used players
corner_infielders <- first_basemen1 %>%
  bind_rows(third_basemen1) %>%
  anti_join(first_basemen2, by = "name") %>%
  anti_join(third_basemen2, by = "name")
corner_infielders1 <- z_score_position(corner_infielders, n_corner_infielders)
#combine all selected players
hitters <- bind_rows(catchers2, first_basemen2, second_basemen2, third_basemen2, shortstops2, outfielders2,
                     middle_infielders1, corner_infielders1)

remaining_hitters <- hitters_new1 %>%
  anti_join(hitters, by = "name") %>%
  arrange(desc(z_score))
#create designated hitters

#this needs to be fixed; "sb" not found √
designated_hitters <- z_score_position(remaining_hitters, n_designated_hitters)
##### #####
hitters1 <- bind_rows(hitters, designated_hitters)
hitters2 <- z_score_position(hitters1, nrow(hitters1))
#hitters3 <- hitters2 %>%  #this makes it easy to visually look at duplicated names
#  arrange(name)

#position relative z_score
hitters_zpos <- hitters2 %>%
  group_by(pos) %>%
  summarize(z_pos_mean = round(mean(z_score), 2)) %>%
  arrange(desc(z_pos_mean))

hitters_zpos1 <- hitters2 %>%
  left_join(hitters_zpos, by = "pos") %>%
  mutate(z_pos = round(z_score - z_pos_mean, 4)) %>%
  arrange(desc(z_pos))

#hitters_zpos2 <- hitters_zpos1 %>%
#  arrange(name)  #alphabetized to check for duplicate names or to look at particular player

#model the stats in the population
  #all look like some type of poisson distribution
    #find the R function for self selection of poisson distribution parameters
ggplot(hitters_zpos1, aes(avg)) + geom_histogram(binwidth = .003)
ggplot(hitters_zpos1, aes(runs)) + geom_histogram(binwidth = 2)
ggplot(hitters_zpos1, aes(hr)) + geom_histogram(binwidth = 1)
ggplot(hitters_zpos1, aes(rbi)) + geom_histogram(binwidth = 2)
ggplot(hitters_zpos1, aes(ops)) + geom_histogram(binwidth = .01)
ggplot(hitters_zpos1, aes(sb_net)) + geom_histogram(binwidth = 1)

