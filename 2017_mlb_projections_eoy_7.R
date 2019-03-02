#set for 2019
setwd("C:/Users/Ben/Desktop/baseball")

#libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)  #for BoxCox
library(zoo)  #for na.locf fn
library(scales)  #for rescale()
library(broom)  #for augment()

#read in data
#fangraphs projections
pitchers <- read_excel("projections.xlsx", sheet = 4)
hitters <- read_excel("projections.xlsx", sheet = 3)
#read in nfbc position eligibility
nfbc <- read_excel("projections.xlsx", sheet = 2)

#select nfbc cols
nfbc <- nfbc %>%
        select(1:4)

#CLEAN DATA
#NFBC
#name cols
nfbc_names <- c("player", "injury", "pos", "team")
names(nfbc) <- nfbc_names
#separate position by comma
nfbc <- nfbc %>%
        separate(pos, into = c("pos", "pos1", "pos2", "pos3"), sep = ",")
#separate and concatenate player names
#clean incorrectly used double commas
nfbc$player <- ifelse(nfbc$player == "Guerrero, Jr., Vladimir", "Guerrero Jr., Vladimir", nfbc$player)
nfbc$player <- ifelse(nfbc$player == "Tatis, Jr., Fernando", "Tatis Jr., Fernando", nfbc$player)
#separate player names by comma
nfbc <- nfbc %>%
        separate(player, into = c("last_name", "first_name"), sep = ",")
#paste player names together in the right order
nfbc$player <- trimws(paste(nfbc$first_name, nfbc$last_name))
#select columns to keep
nfbc <- nfbc %>%
        select(player, injury, pos, pos1, pos2, pos3, team)

#FANGRAPHS
#Hitter names
#Use this when ADP included
hitter_names <- c("player", "team", "games.h", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb.h", "so.h",
                  "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
                  "waste3", "offense", "defense", "war", "waste4", "adp", "playerid")

#Use this when ADP not included
# hitter_names <- c("player", "team", "games.h", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb.h", "so.h",
#                   "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
#                   "waste3", "offense", "defense", "war", "playerid")
# 
names(hitters) <- hitter_names

#Pitcher names
#Use this when ADP included
pitcher_names <- c("player", "team", "wins", "losses", "era", "gs", "games.p", "saves", "ip", "hits", "er", "hra", "so.p", "bb.p",
                   "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "adp", "player_id")

#Use this when ADP not included
# pitcher_names <- c("player", "team", "wins", "losses", "era", "gs", "games.p", "saves", "ip", "hits", "er", "hra", "so.p", "bb.p",
#                    "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")

names(pitchers) <- pitcher_names

#Clean fangraphs data
hitters <- hitters %>%
        select(-waste1, -waste2, -waste3,  #remove spacer cols
               -wrc_plus, -bsr, -fld, -offense, -defense, -war, -playerid) %>%  #remove unneeded obs
        select(-waste4, -adp)  #sometimes need to remove adp

pitchers <- pitchers %>%
        select(-war, -ra9_war, -player_id) %>%  #remove uneeded obs
        select(-adp)  #sometimes need to remove adp

#remove obs that will be duplicated when joining with nfbc
hitters <- hitters %>%
        select(-team)
pitchers <- pitchers %>%
        select(-team)

#remove hitters and pitchers with 0 or 1 plate and appearance and innings pitched, respectively
hitters <- hitters %>%
        filter(pa > 1)
pitchers <- pitchers %>%
        filter(ip > 1)

#remove specific instances if necessary
pitchers <- pitchers %>%
        filter(player != "Shohei Ohtani")  #do not want him in 600 ratings because his UCL is toast

#get nfbc adp rankings
#join adp rankings with nfbc eligibility

#Join nfbc data with fangraphs data
nfbc <- nfbc %>%
        left_join(hitters, by = "player") %>%
        left_join(pitchers, by = "player")

#separate nfbc into hitters and pitchers
hitters <- nfbc %>%
        filter(pos != "P" & !is.na(pa))
pitchers <- nfbc %>%
        filter(pos == "P" & !is.na(ip))

#numbers of starting players
n_teams <- 15
starting_catchers <- 2
starting_first_basemen <- 1
starting_second_basemen <- 1
starting_third_basemen <- 1
starting_shortstops <- 1
starting_outfielders <- 5
starting_middle_infielders <- 1
starting_corner_infielders <- 1
starting_designated_hitters <- 1
#pitchers
pitchers_starting <- 9

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

# #pitchers
n_pitchers <- n_teams * pitchers_starting

#FUNCTIONS
#Z-Score FUNCTION for hitters
z_score_hitters <- function(df) {
        
        #nfbc stats
        df$hr_z <- round(as.numeric(scale(df$hr)), 3)
        df$runs_z <- round(as.numeric(scale(df$runs)), 3)
        df$rbi_z <- round(as.numeric(scale(df$rbi)), 3)
        df$avg_z <- round(as.numeric(scale(df$avg)), 3)
        df$sb_z <- round(as.numeric(scale(BoxCox(df$sb, .45))), 3)
        
        df$z_total <- df$hr_z + df$runs_z + df$rbi_z + df$avg_z + df$sb_z
        
        #return df
        df
}

#SCALE HITTER STATS
#Rescale function for hitters
rescale_hitters <- function(df) {
        
        df$hr_pts <- round(rescale(df$hr_z), 3)
        df$runs_pts <- round(rescale(df$runs_z), 3)
        df$rbi_pts <- round(rescale(df$rbi_z), 3)
        df$avg_pts <- round(rescale(df$avg_z, to = c(0, 2/3)), 3)
        df$sb_pts <- round(rescale(df$sb_z), 3)
        
        df$pts_total <- df$hr_pts + df$runs_pts + df$rbi_pts + df$avg_pts + df$sb_pts
        
        df
}

#WEIGHT HITTER RATIO STATS
#function for position weighting
weight_hitter_stats <- function(df, df_pop) {  #take in two args, the sample we are interested in (df), and the population (df_pop)
        
        #weight a stat for the position group by the entire population of eligible hitters
        df$hr_pts_weighted <- round(df$hr_pts * mean(df$hr) / mean(df_pop$hr), 3)
        df$runs_pts_weighted <- round(df$runs_pts * mean(df$runs) / mean(df_pop$runs), 3)
        df$rbi_pts_weighted <- round(df$rbi_pts * mean(df$rbi) / mean(df_pop$rbi), 3)
        #df$avg_pts_weighted <- round(df$avg_pts * mean(df$ab) / mean(df_pop$ab), 3)
        df$avg_pts_weighted <- round(df$avg_pts * mean(df$ab) * df$ab / (mean(df_pop$ab) * mean(df$ab)), 3)
        df$sb_pts_weighted <- round(df$sb_pts * mean(df$sb) / mean(df_pop$sb), 3)
        
        df$weighted_pts_total <- round(df$hr_pts_weighted + df$runs_pts_weighted + df$rbi_pts_weighted + df$avg_pts_weighted + df$sb_pts_weighted, 3)
        
        df
}

weight_hitter_rate_stats <- function(df) {
        
        df$hr_pts <- round(rescale(df$hr_z), 3)
        df$runs_pts <- round(rescale(df$runs_z), 3)
        df$rbi_pts <- round(rescale(df$rbi_z), 3)
        #df$avg_pts <- round(rescale(df$avg_z, to = c(0, 2/3)), 3)
        df$avg_pts <- round(rescale(df$avg_pts * df$ab / mean(df$ab), to = c(0, 2/3)), 3)
        df$sb_pts <- round(rescale(df$sb_z), 3)
        
        df$pts_total <- df$hr_pts + df$runs_pts + df$rbi_pts + df$avg_pts + df$sb_pts
        
        df
        
        #df$avg_pts_weighted <- round(df$avg_pts_weighted * df$ba / mean(df$ab), 3)
}

#GRAPH stat, z_score stat, rescaled stat, and weighted stat to see the distributions.
# hitters1 <- rescale_hitters(z_score_hitters(hitters)) %>% arrange(desc(pts_total))
# ggplot(data = hitters1, aes(x = hr_z, y = hr_pts)) + geom_point()
#yes; looks great

#Assign main position to multi-position players
#Initialize
hitters$main_pos <- hitters$pos
hitters$main_pos1 <- hitters$pos1
hitters$main_pos2 <- hitters$pos2
hitters$main_pos3 <- hitters$pos3

# #Individual players
# #first position
hitters$main_pos <- ifelse(hitters$player == "Lourdes Gurriel Jr.", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Marwin Gonzalez", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Asdrubal Cabrera", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Max Muncy", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Eduardo Escobar", "SS", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Chris Taylor", "OF", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Dee Gordon", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Ian Desmond", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Trey Mancini", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Ketel Marte", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Jake Bauers", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Matt Carpenter", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Wil Myers", "OF", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Yuli Gurriel", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Travis Shaw", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Whit Merrifield", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Joey Gallo", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Kris Bryant", "OF", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Cody Bellinger", "1B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Javier Baez", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Gleyber Torres", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Alex Bregman", "3B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Adam Frazier", "2B", hitters$main_pos)
hitters$main_pos <- ifelse(hitters$player == "Jurickson Profar", "1B", hitters$main_pos)

#                 #adjusted in 600 ratings
# hitters$main_pos <- ifelse(hitters$player == "Hernan Perez", "2B", hitters$main_pos)
# hitters$main_pos <- ifelse(hitters$player == "Ian Happ", "3B", hitters$main_pos)
# hitters$main_pos <- ifelse(hitters$player == "Aledmys Diaz", "SS", hitters$main_pos)
# hitters$main_pos <- ifelse(hitters$player == "Eduardo Nunez", "2B", hitters$main_pos)
# hitters$main_pos <- ifelse(hitters$player == "Eric Thames", "1B", hitters$main_pos)
# 
# #                 #second position
hitters$main_pos1 <- ifelse(hitters$player == "Lourdes Gurriel Jr.", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Marwin Gonzalez", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Asdrubal Cabrera", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Max Muncy", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Eduardo Escobar", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Chris Taylor", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Dee Gordon", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Ian Desmond", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Trey Mancini", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Ketel Marte", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Jake Bauers", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Matt Carpenter", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Wil Myers", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Yuli Gurriel", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Travis Shaw", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Whit Merrifield", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Joey Gallo", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Kris Bryant", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Cody Bellinger", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Javier Baez", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Gleyber Torres", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Alex Bregman", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Adam Frazier", NA, hitters$main_pos1)
hitters$main_pos1 <- ifelse(hitters$player == "Jurickson Profar", NA, hitters$main_pos1)

#         #adjusted in 600 ratings
# hitters$main_pos1 <- ifelse(hitters$player == "Hernan Perez", NA, hitters$main_pos1)
# hitters$main_pos1 <- ifelse(hitters$player == "Ian Happ", NA, hitters$main_pos1)
# hitters$main_pos1 <- ifelse(hitters$player == "Aledmys Diaz", NA, hitters$main_pos1)
# hitters$main_pos1 <- ifelse(hitters$player == "Eduardo Nunez", NA, hitters$main_pos1)
# hitters$main_pos1 <- ifelse(hitters$player == "Eric Thames", NA, hitters$main_pos1)
# 
# #                 #third position
hitters$main_pos2 <- ifelse(hitters$player == "Lourdes Gurriel Jr.", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Marwin Gonzalez", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Asdrubal Cabrera", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Max Muncy", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Eduardo Escobar", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Chris Taylor", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Dee Gordon", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Ian Desmond", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Trey Mancini", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Ketel Marte", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Jake Bauers", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Matt Carpenter", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Wil Myers", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Yuli Gurriel", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Travis Shaw", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Whit Merrifield", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Joey Gallo", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Kris Bryant", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Cody Bellinger", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Javier Baez", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Gleyber Torres", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Alex Bregman", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Adam Frazier", NA, hitters$main_pos2)
hitters$main_pos2 <- ifelse(hitters$player == "Jurickson Profar", NA, hitters$main_pos2)

#         #adjusted in 600 ratings
# hitters$main_pos2 <- ifelse(hitters$player == "Hernan Perez", NA, hitters$main_pos2)
# hitters$main_pos2 <- ifelse(hitters$player == "Ian Happ", NA, hitters$main_pos2)
# hitters$main_pos2 <- ifelse(hitters$player == "Aledmys Diaz", NA, hitters$main_pos2)
# hitters$main_pos2 <- ifelse(hitters$player == "Eduardo Nunez", NA, hitters$main_pos2)
# hitters$main_pos2 <- ifelse(hitters$player == "Eric Thames", NA, hitters$main_pos2)
# 
# #                 #fourth position
hitters$main_pos3 <- ifelse(hitters$player == "Lourdes Gurriel Jr.", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Marwin Gonzalez", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Asdrubal Cabrera", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Max Muncy", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Eduardo Escobar", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Chris Taylor", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Dee Gordon", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Ian Desmond", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Trey Mancini", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Ketel Marte", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Jake Bauers", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Matt Carpenter", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Wil Myers", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Yuli Gurriel", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Travis Shaw", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Whit Merrifield", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Joey Gallo", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Kris Bryant", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Cody Bellinger", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Javier Baez", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Gleyber Torres", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Alex Bregman", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Adam Frazier", NA, hitters$main_pos3)
hitters$main_pos3 <- ifelse(hitters$player == "Jurickson Profar", NA, hitters$main_pos3)

#         #adjusted in 600 ratings
# hitters$main_pos3 <- ifelse(hitters$player == "Hernan Perez", NA, hitters$main_pos3)
# hitters$main_pos3 <- ifelse(hitters$player == "Ian Happ", NA, hitters$main_pos3)
# hitters$main_pos3 <- ifelse(hitters$player == "Aledmys Diaz", NA, hitters$main_pos3)
# hitters$main_pos3 <- ifelse(hitters$player == "Eduardo Nunez", NA, hitters$main_pos3)
# hitters$main_pos3 <- ifelse(hitters$player == "Eric Thames", NA, hitters$main_pos3)

#subset hitters into position groups
catchers <- hitters %>%
        filter(main_pos == "C" | main_pos1 == "C" | main_pos2 == "C" | main_pos3 == "C")
first_basemen <- hitters %>%
        filter(main_pos == "1B" | main_pos1 == "1B" | main_pos2 == "1B" | main_pos3 == "1B")
second_basemen <- hitters %>%
        filter(main_pos == "2B" | main_pos1 == "2B" | main_pos2 == "2B" | main_pos3 == "2B")
third_basemen <- hitters %>%
        filter(main_pos == "3B" | main_pos1 == "3B" | main_pos2 == "3B" | main_pos3 == "3B")
shortstops <- hitters %>%
        filter(main_pos == "SS" | main_pos1 == "SS" | main_pos2 == "SS" | main_pos3 == "SS")
outfielders<- hitters %>%
        filter(main_pos == "OF" | main_pos1 == "OF" | main_pos2 == "OF" | main_pos3 == "OF")
utility <- hitters %>%
        filter(main_pos == "UT" | main_pos1 == "UT" | main_pos2 == "UT" | main_pos3 == "UT")

#Run z_score on hitters
catchers1 <- z_score_hitters(catchers) %>% arrange(desc(z_total))
first_basemen1 <- z_score_hitters(first_basemen) %>% arrange(desc(z_total))
second_basemen1 <- z_score_hitters(second_basemen) %>% arrange(desc(z_total))
third_basemen1 <- z_score_hitters(third_basemen) %>% arrange(desc(z_total))
shortstops1 <- z_score_hitters(shortstops) %>% arrange(desc(z_total))
outfielders1 <- z_score_hitters(outfielders) %>% arrange(desc(z_total))

#rescale position group hitter stats
catchers1 <- rescale_hitters(catchers1) %>% arrange(desc(pts_total))
first_basemen1 <- rescale_hitters(first_basemen1) %>% arrange(desc(pts_total))
second_basemen1 <- rescale_hitters(second_basemen1) %>% arrange(desc(pts_total))
third_basemen1 <- rescale_hitters(third_basemen1) %>% arrange(desc(pts_total))
shortstops1 <- rescale_hitters(shortstops1) %>% arrange(desc(pts_total))
outfielders1 <- rescale_hitters(outfielders1) %>% arrange(desc(pts_total))

#weight all stats
catchers1 <- weight_hitter_stats(catchers1, hitters) %>% arrange(desc(weighted_pts_total))
first_basemen1 <- weight_hitter_stats(first_basemen1, hitters) %>% arrange(desc(weighted_pts_total))
second_basemen1 <- weight_hitter_stats(second_basemen1, hitters) %>% arrange(desc(weighted_pts_total))
third_basemen1 <- weight_hitter_stats(third_basemen1, hitters) %>% arrange(desc(weighted_pts_total))
shortstops1 <- weight_hitter_stats(shortstops1, hitters) %>% arrange(desc(weighted_pts_total))
outfielders1 <- weight_hitter_stats(outfielders1, hitters) %>% arrange(desc(weighted_pts_total))

#select the top from each position
catchers2 <- catchers1[1:n_catchers,]
first_basemen2 <- first_basemen1[1:n_first_basemen,]
second_basemen2 <- second_basemen1[1:n_second_basemen,]
third_basemen2 <- third_basemen1[1:n_third_basemen,]
shortstops2 <- shortstops1[1:n_shortstops,]
outfielders2 <- outfielders1[1:n_outfielders,]

#create middle infielders df
middle_infielders <- second_basemen1 %>%
        bind_rows(shortstops1) %>%  #combine ALL SS and 2B-men
        anti_join(second_basemen2, by = "player") %>%  #remove the 2B already selected
        anti_join(shortstops2, by = "player")  #remove the SS already selected
#run z-score on middle infielders after removing already used players
middle_infielders1 <- z_score_hitters(middle_infielders) %>% arrange(desc(z_total))
middle_infielders1 <- rescale_hitters(middle_infielders1) %>% arrange(desc(pts_total))
middle_infielders1 <- weight_hitter_stats(middle_infielders1, hitters) %>% arrange(desc(pts_total))

# #run same process on corner infielders as middle infielders
corner_infielders <- first_basemen1 %>%
        bind_rows(third_basemen1) %>%
        anti_join(first_basemen2, by = "player") %>%
        anti_join(third_basemen2, by = "player")
#run functions
corner_infielders1 <- z_score_hitters(corner_infielders)
corner_infielders1 <- rescale_hitters(corner_infielders1) %>% arrange(desc(pts_total))
corner_infielders1 <- weight_hitter_stats(corner_infielders1, hitters) %>% arrange(desc(pts_total))
#subset middle and corner infielders
middle_infielders2 <- middle_infielders1[1:n_middle_infielders,]
corner_infielders2 <- corner_infielders1[1:n_corner_infielders,]
#
#combine all selected players
position_players <- bind_rows(catchers2, first_basemen2, second_basemen2, third_basemen2, shortstops2, outfielders2,
                              middle_infielders2, corner_infielders2)

#anti_join remaining hitters to selected hitters for use with DH/util and bench players
#get utlility/designated players
hitters_remaining <- hitters %>%
        anti_join(position_players, by = "player")

hitters_remaining <- z_score_hitters(hitters_remaining) %>% arrange(desc(z_total))
hitters_remaining <- rescale_hitters(hitters_remaining) %>% arrange(desc(pts_total))
hitters_remaining <- weight_hitter_stats(hitters_remaining, hitters) %>% arrange(desc(pts_total))

utility_players <- hitters_remaining[1:n_designated_hitters,]

#combine utility players with position players
all_hitters <- position_players %>%
        bind_rows(utility_players)

all_hitters <- z_score_hitters(all_hitters) %>% arrange(desc(z_total))
all_hitters <- rescale_hitters(all_hitters) %>% arrange(desc(pts_total))
# all_hitters1 <- all_hitters
all_hitters <- weight_hitter_rate_stats(all_hitters) %>% arrange(desc(pts_total))

#remove unwanted dfs
rm(catchers1, catchers2, corner_infielders1, corner_infielders2, first_basemen1, first_basemen2, middle_infielders1, middle_infielders2,
   outfielders1, outfielders2, second_basemen1, second_basemen2, shortstops1, shortstops2, third_basemen1, third_basemen2, position_players, utility_players)
rm(catchers, corner_infielders, first_basemen, middle_infielders, outfielders, second_basemen, shortstops, third_basemen, utility, hitters_remaining)

#add hitters rank to df
all_hitters$hitter_rank <- as.integer(rank(-all_hitters$pts_total))

# #position relative z_score
# #generate a mean z_score for each position across all players at the position in the "league"
# z_pos_means <- all_hitters %>%
#         group_by(pos) %>%
#         summarize(z_pos_mean = round(mean(z_total), 2)) %>%
#         arrange(desc(z_pos_mean))
# 
# #join the hitters with the mean z_score/position: z_pos
# all_hitters1 <- all_hitters %>%
#         left_join(z_pos_means, by = "pos") %>%
#         mutate(z_pos = round(z_total - z_pos_mean, 4)) %>%
#         arrange(desc(z_pos))

#METHOD 2: starters and relievers in same group
z_score_pitchers <- function(df) {
        
        df$wins_z <- round(as.numeric(scale(df$wins)), 3)
        df$saves_z <- round(as.numeric(scale(df$saves)), 3)
        df$era_z <- round(as.numeric(scale(df$era) * -1), 3)
        df$so_z <- round(as.numeric(scale(df$so.p)), 3)
        df$whip_z <- round(as.numeric(scale(df$whip) * -1), 3)
        
        df$z_total <- df$wins_z + df$saves_z + df$era_z + df$so_z + df$whip_z
        #
        df
}

#rescale function for pitchers
rescale_pitchers <- function(df) {
        
        df$wins_pts <- round(rescale(df$wins_z), 3)
        df$saves_pts <- round(rescale(df$saves_z), 3)
        df$era_pts <- round(rescale(df$era_z, to = c(0, 2/3)), 3)
        df$so_pts <- round(rescale(df$so_z), 3)
        df$whip_pts <- round(rescale(df$whip_z), 3)
        
        df$pts_total <- df$wins_pts + df$saves_pts + df$era_pts + df$so_pts + df$whip_pts
        
        df
}

weight_pitcher_rate_stats <- function(df) {
        
        df$wins_pts <- df$wins_pts
        df$saves_pts <- df$saves_pts
        #df$era_pts <- round(df$era_pts * df$ip / mean(df$ip), 3)
        df$era_pts <- round(rescale(df$era_pts * df$ip / mean(df$ip), to = c(0, 2/3)), 3)
        df$so_pts <- df$so_pts
        #df$whip_pts <- round(df$whip_pts * df$ip / mean(df$ip), 3)
        df$whip_pts <- round(rescale(df$whip_pts * df$ip / mean(df$ip), to = c(0, 1)), 3)
        
        df$pts_total <- round(df$wins_pts + df$saves_pts + df$era_pts + df$so_pts + df$whip_pts, 3)
        
        df
        
}

pitchers1 <- z_score_pitchers(pitchers) %>% arrange(desc(z_total))
pitchers1 <- rescale_pitchers(pitchers1) %>% arrange(desc(pts_total))
pitchers1 <- weight_pitcher_rate_stats(pitchers1) %>% arrange(desc(pts_total))
pitchers1 <- pitchers1[1:n_pitchers,]

#run numbers on only pitchers who will be drafted
all_pitchers <- z_score_pitchers(pitchers1) %>% arrange(desc(z_total))
all_pitchers <- rescale_pitchers(all_pitchers) %>% arrange(desc(pts_total))
all_pitchers <- weight_pitcher_rate_stats(all_pitchers) %>% arrange(desc(pts_total))

rm(pitchers1)
#add pitcher rank
all_pitchers$pitcher_rank <- as.integer(rank(-all_pitchers$pts_total))

#combine pitchers and catchers
all_players <- all_hitters %>%
        bind_rows(all_pitchers) %>%
        arrange(desc(pts_total))

#UGH! Fangraphs has different number of download columns sometimes, so need a few of these
# all_players <- all_players[,-c(55:60)]
all_players <- all_players[,-c(59:64)]

#Generate 100 point scale
all_players <- all_players %>%
        mutate(rank100 = round(rescale(pts_total, to = c(0, 100)), 0))

#Duplicates
#assign a "main position" to players with multiple positions based on position strength, in order to add another for one who is in two or three times.
#generate a mean z_score for each position across all players at the position in the "league"

#There are up to 4 positions possible, so need to group by all of them
z_pos_means <- all_players %>%
        group_by(pos, pos1, pos2, pos3) %>%
        summarize(z_pos_mean = round(mean(z_total), 2)) %>%
        arrange(desc(z_pos_mean))

z_pos_means1 <- all_players %>%
        group_by(pos, pos1, pos2, pos3) %>%
        tally()

z_pos_means <- z_pos_means %>%
        left_join(z_pos_means1) %>%
        mutate(total_weight = n * z_pos_mean)

rm(z_pos_means1)
#separate by columns
position <- z_pos_means[,c(1,5:7)]
position1 <- z_pos_means[,c(2,5:7)] %>%
        filter(!is.na(pos1))
position2 <- z_pos_means[,c(3,5:7)] %>%
        filter(!is.na(pos2))
position3 <- z_pos_means[,c(4:7)] %>%
        filter(!is.na(pos3))

rm(z_pos_means)
#now group by again
position <- position %>%
        group_by(pos) %>%
        summarize(total_weight_new = sum(total_weight), total_n = sum(n))
position1 <- position1 %>%
        group_by(pos1) %>%
        summarize(total_weight_new = sum(total_weight), total_n = sum(n))
position2 <- position2 %>%
        group_by(pos2) %>%
        summarize(total_weight_new = sum(total_weight), total_n = sum(n))
position3 <- position3 %>%
        group_by(pos3) %>%
        summarize(total_weight_new = sum(total_weight), total_n = sum(n))
#bind these groups together
#There is a faster way, but for now just change column name to "pos"
colnames(position1)[1] <- "pos"
colnames(position2)[1] <- "pos"
colnames(position3)[1] <- "pos"
#bind
position <- position %>%
        bind_rows(position1) %>%
        bind_rows(position2) %>%
        bind_rows(position3)
#remove binded dfs
rm(position1, position2, position3)
#run same process on the combined position groupings
position_strength <- position %>%
        group_by(pos) %>%
        summarize(total_weight = sum(total_weight_new), total_n = sum(total_n), mean_pos_strength = round(total_weight / total_n, 3)) %>%
        arrange(desc(mean_pos_strength))

rm(position)

#add a rank column
all_players$overall_rank <- as.integer(rank(-all_players$pts_total))

#NEXT! start position assigning based on position strength
which(duplicated(all_players$player))

# find_name <- function(player) {
#         which(all_players$player == player)
# }

#read in and match adp
adp <- read.delim2(file = "nfbc_adp.txt")

names_adp <- c("adp_rank", "player", "team", "positions", "adp", "min_pick", "max_pick", "difference", "n_picks", "team1", "team_pick")

names(adp) <- names_adp

#clean df
#set correct classes
adp$player <- as.character(adp$player)
adp$team <- as.character(adp$team)
adp$adp <- as.numeric(as.character(adp$adp))

#separate and concatenate player names
#clean incorrectly used double commas
adp$player <- ifelse(adp$player == "Guerrero, Jr., Vladimir", "Guerrero Jr., Vladimir", adp$player)
adp$player <- ifelse(adp$player == "Tatis, Jr., Fernando", "Tatis Jr., Fernando", adp$player)
#separate player names by comma
adp <- adp %>%
        separate(player, into = c("last_name", "first_name"), sep = ",")
#paste player names together in the right order
adp$player <- trimws(paste(adp$first_name, adp$last_name))

#select columns to keep/discard
adp <- adp %>%
        select(adp_rank, player, adp, min_pick, max_pick, n_picks)

all_players <- all_players %>%
        left_join(adp, by = "player")

#separate into position groups
pitchers_final <- all_players %>%
        filter(pos == "P" | pos1 == "P" | pos2 == "P" | pos3 == "P")

hitters_final <- all_players %>%
        anti_join(pitchers_final, by = "player")

#remove hitters stats and generally clean
pitchers_final1 <- pitchers_final %>%
        select(-c("pos1":"pos3", "games.h":"woba", "main_pos":"sb_z", "hr_pts":"sb_pts", "hitter_rank"))
pitchers_final2 <- pitchers_final1 %>%
        select(overall_rank, player, pos, team, adp_rank, adp, injury, wins, era, 
               gs, games.p, saves, ip, so.p, whip, rank100)

#hitters stuff
hitters_final1 <- hitters_final %>%
        select(-c("wins":"fip", "main_pos1":"main_pos3", "wins_z":"pitcher_rank"))
hitters_final2 <- hitters_final1 %>%
        select(overall_rank, player, hitter_rank, main_pos, team, adp_rank, adp, injury, games.h, pa, hr,
               runs, rbi, sb, avg, pos, pos1, pos2, pos3, rank100)

#position groups for exploration
# catchers <- all_players %>%
#         filter(pos == "C" | pos1 == "C" | pos2 == "C" | pos3 == "C")
# first_basemen <- all_players %>%
#         filter(pos == "1B" | pos1 == "1B" | pos2 == "1B" | pos3 == "1B")
# second_basemen <- all_players %>%
#         filter(pos == "2B" | pos1 == "2B" | pos2 == "2B" | pos3 == "2B")
# third_basemen <- all_players %>%
#         filter(pos == "3B" | pos1 == "3B" | pos2 == "3B" | pos3 == "3B")
# shortstops <- all_players %>%
#         filter(pos == "SS" | pos1 == "SS" | pos2 == "SS" | pos3 == "SS")
# outfielders <- all_players %>%
#         filter(pos == "OF" | pos1 == "OF" | pos2 == "OF" | pos3 == "OF")
# middle_infielders <- all_players %>%
#         filter(pos == "2B" | pos1 == "2B" | pos2 == "2B" | pos3 == "2B" | pos == "SS" | pos1 == "SS" | pos2 == "SS" | pos3 == "SS")
# corner_infielders <- all_players %>%
#         filter(pos == "1B" | pos1 == "1B" | pos2 == "1B" | pos3 == "1B" | pos == "3B" | pos1 == "3B" | pos2 == "3B" | pos3 == "3B")

#position groups
catchers <- hitters_final2 %>%
        filter(pos == "C" | pos1 == "C" | pos2 == "C" | pos3 == "C")
first_basemen <- hitters_final2 %>%
        filter(pos == "1B" | pos1 == "1B" | pos2 == "1B" | pos3 == "1B")
second_basemen <- hitters_final2 %>%
        filter(pos == "2B" | pos1 == "2B" | pos2 == "2B" | pos3 == "2B")
third_basemen <- hitters_final2 %>%
        filter(pos == "3B" | pos1 == "3B" | pos2 == "3B" | pos3 == "3B")
shortstops <- hitters_final2 %>%
        filter(pos == "SS" | pos1 == "SS" | pos2 == "SS" | pos3 == "SS")
outfielders <- hitters_final2 %>%
        filter(pos == "OF" | pos1 == "OF" | pos2 == "OF" | pos3 == "OF")
middle_infielders <- hitters_final2 %>%
        filter(pos == "2B" | pos1 == "2B" | pos2 == "2B" | pos3 == "2B" | pos == "SS" | pos1 == "SS" | pos2 == "SS" | pos3 == "SS")
corner_infielders <- hitters_final2 %>%
        filter(pos == "1B" | pos1 == "1B" | pos2 == "1B" | pos3 == "1B" | pos == "3B" | pos1 == "3B" | pos2 == "3B" | pos3 == "3B")

#use this equation for each position group and measure distance from regression line
ggplot(data = outfielders, aes(x = adp_rank, y = log(rank100))) + geom_point() + geom_smooth(method = "lm", se = FALSE)

mod <- lm((rank100) ~ adp_rank, data = outfielders)
summary(mod)
aug_mod <- augment(mod)
ggplot(aug_mod, aes(x = x, y = y, color = ))
predict(mod)

# #exploratory graphs
# ggplot(data = all_players, aes(x = adp_rank, y = overall_rank, color = pos)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# ggplot(data = first_basemen, aes(x = adp_rank, y = overall_rank)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(data = all_players, aes(x = adp_rank, y = rank100, color = pos)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# first_basemen1 <- first_basemen %>%
#         filter(player != "John Hicks")
# ggplot(data = first_basemen1, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# 
# second_basemen1 <- second_basemen %>%
#         filter(player != "Isiah Kiner-Falefa")
# ggplot(data = second_basemen1, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# 
ggplot(data = outfielders, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# ggplot(data = outfielders, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(se = FALSE)
ggplot(data = outfielders, aes(x = adp_rank, y = log(rank100))) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# ggplot(data = outfielders, aes(x = adp_rank, y = sqrt(rank100))) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# ggplot(data = outfielders, aes(x = adp_rank, y = sqrt(rank100))) + geom_point()# + geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(data = pitchers_final2, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(data = hitters_final2, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# # 
# # ggplot(data = hitters_final2, aes(x = adp_rank, y = rank100)) + geom_point() + geom_smooth(se = FALSE)
# ggplot(data = hitters_final2, aes(x = adp_rank, y = rank100, color = pos)) + geom_point() + geom_smooth(se = FALSE)
hitters_final3 <- hitters_final2 %>%
        filter(pos != "UT")
ggplot(data = hitters_final3, aes(x = adp_rank, y = rank100, color = pos)) + geom_point() + geom_smooth(se = FALSE)
ggplot(data = hitters_final3, aes(x = adp_rank, y = rank100, color = pos)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# 
# # change ratio stat weights to a function of mean/sd
# # change all stats to reflect variance
