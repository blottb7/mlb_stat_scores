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

#read in data
        #fangraphs projections
pitchers <- read_excel("projections.xlsx", sheet = 8)
hitters <- read_excel("projections.xlsx", sheet = 9)
        #read in nfbc position eligibility
nfbc <- read_excel("projections.xlsx", sheet = 10)

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
# hitter_names <- c("player", "team", "games.h", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb.h", "so.h",
#                  "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
#                  "waste3", "offense", "defense", "war", "waste4", "adp", "playerid")

#Use this when ADP not included
hitter_names <- c("player", "team", "games.h", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb.h", "so.h",
                 "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
                 "waste3", "offense", "defense", "war", "playerid")

names(hitters) <- hitter_names

        #Pitcher names
#Use this when ADP included
# names(pitchers) <- c("player", "team", "wins", "losses", "era", "gs", "games.p", "saves", "ip", "hits", "er", "hra", "so.p", "bb.p",
#                      "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "adp", "player_id")

#Use this when ADP not included
pitcher_names <- c("player", "team", "wins", "losses", "era", "gs", "games.p", "saves", "ip", "hits", "er", "hra", "so.p", "bb.p",
                     "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")

names(pitchers) <- pitcher_names

#Clean fangraphs data
hitters <- hitters %>%
        select(-waste1, -waste2, -waste3,  #remove spacer cols
               -wrc_plus, -bsr, -fld, -offense, -defense, -war, -playerid)  #remove unneeded obs

pitchers <- pitchers %>%
        select(-war, -ra9_war, -player_id)  #remove uneeded obs

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

#add total pitcher if and when necessary

pitchers_starting <- 9
# relief_pitchers <- 2.5
# #bench players
# bench_players <- 7

#plate appearances

#decide if I need min_pa and min_ip_sp

# min_pa <- 100  #the minimum number of remaining plate appearances
# min_ip_sp <- 50  #the minimum number of remaining innings pitched for starters

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

# weight_hitter_rate_stats <- function(df) {
#         df$avg_pts_weighted <- round(df$avg_pts_weighted * df$ba / mean(df$ab), 3)
# }

#GRAPH stat, z_score stat, rescaled stat, and weighted stat to see the distributions.
# hitters1 <- rescale_hitters(z_score_hitters(hitters)) %>% arrange(desc(pts_total))
# ggplot(data = hitters1, aes(x = hr_z, y = hr_pts)) + geom_point()
        #yes; looks great

#subset hitters into position groups
catchers <- hitters %>%
        filter(pos == "C" | pos1 == "C" | pos2 == "C" | pos3 == "C")
first_basemen <- hitters %>%
        filter(pos == "1B" | pos1 == "1B" | pos2 == "1B" | pos3 == "1B")
second_basemen <- hitters %>%
        filter(pos == "2B" | pos1 == "2B" | pos2 == "2B" | pos3 == "2B")
third_basemen <- hitters %>%
        filter(pos == "3B" | pos1 == "3B" | pos2 == "3B" | pos3 == "3B")
shortstops <- hitters %>%
        filter(pos == "SS" | pos1 == "SS" | pos2 == "SS" | pos3 == "SS")
outfielders<- hitters %>%
        filter(pos == "OF" | pos1 == "OF" | pos2 == "OF" | pos3 == "OF")
utility <- hitters %>%
        filter(pos == "UT" | pos1 == "UT" | pos2 == "UT" | pos3 == "UT")

#So, doing this by position gives equal z-weighting to sb's, which catchers don't really have, as to home runs.
        #Put another way, 5 sb are equal to about 16 home runs, which is off
                #Maybe try the rescale function, then weighting the sb_z by mean of entire population of hitters, regardless of position
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
        bind_rows(shortstops1) %>%  #combine ALL QUALIFIED (300+ PA) SS and 2B-men
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

#remove unwanted dfs
rm(catchers1, catchers2, corner_infielders1, corner_infielders2, first_basemen1, first_basemen2, middle_infielders1, middle_infielders2,
   outfielders1, outfielders2, second_basemen1, second_basemen2, shortstops1, shortstops2, third_basemen1, third_basemen2, position_players, utility_players)
rm(catchers, corner_infielders, first_basemen, middle_infielders, outfielders, second_basemen, shortstops, third_basemen, utility, hitters_remaining)

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

#function for pitcher weighting
weight_pitcher_stats <- function(df) { 
        
        #weight a stat for the position group by the entire population of eligible hitters
        # df$wins_pts_weighted <- round(df$wins_pts * mean(df$wins) / mean(df$wins), 3)
        # df$saves_pts_weighted <- round(df$saves_pts * mean(df$saves) / mean(df$saves), 3)
        df$wins_pts_weighted <- df$wins_pts
        df$saves_pts_weighted <- df$saves_pts
        df$era_pts_weighted <- round(df$era_pts * df$ip / mean(df$ip), 3)
        # df$so_pts_weighted <- round(df$so_pts * mean(df$so) / mean(df$so), 3)
        df$so_pts_weighted <- df$so_pts
        df$whip_pts_weighted <- round(df$whip_pts * df$ip / mean(df$ip), 3)
        
        df$weighted_pts_total <- round(df$wins_pts_weighted + df$saves_pts_weighted + df$era_pts_weighted + df$so_pts_weighted + df$whip_pts_weighted, 3)
        
        df
}

# #pitchers rescale function
# rescale_pitchers <- function(df) {
#         
#         df$wins_pts <- rescale(df$wins_z)
#         df$saves_pts <- rescale(df$saves_z)
#         df$era_pts <- rescale(rescale(df$era_z) * df$ip / mean(df$ip))
#         df$so_pts <- rescale(df$so_z)
#         df$whip_pts <- rescale(rescale(df$whip_z) * df$ip / mean(df$ip))
#         
#         df$pts <- df$wins_pts + df$saves_pts + df$era_pts + df$so_pts + df$whip_pts
#         
#         df
# }

pitchers1 <- z_score_pitchers(pitchers) %>% arrange(desc(z_total))
pitchers1 <- rescale_pitchers(pitchers1) %>% arrange(desc(pts_total))
pitchers1 <- weight_pitcher_stats(pitchers1) %>% arrange(desc(weighted_pts_total))
pitchers1 <- pitchers1[1:n_pitchers,]

#run numbers on only pitchers who will be drafted
all_pitchers <- z_score_pitchers(pitchers1) %>% arrange(desc(z_total))
all_pitchers <- rescale_pitchers(all_pitchers) %>% arrange(desc(pts_total))

rm(pitchers1)
#combine pitchers and catchers
all_players <- all_hitters %>%
        bind_rows(all_pitchers) %>%
        arrange(desc(pts_total))

all_players <- all_players[,-c(55:60,71:75)]

#Generate 100 point scale
all_players <- all_players %>%
        mutate(rank100 = round(rescale(pts_total, to = c(0, 100)), 0))
#THIS IS ADEQUATE

#Start filtering out duplicates
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

#NEXT, go back and weight whip and era, Edwin Diaz is way too high. Also, ERA
        #check on batting average to make sure that is weighted properly
##### ##### #####

# #set NA's to zero for stat categories
# all_players$
# 
# #function for all nfbc stat categories
# 
# z_score_all <- function(df) {
#         
#         df$hr_z <- round(as.numeric(scale(df$hr)), 3)
#         df$runs_z <- round(as.numeric(scale(df$runs)), 3)
#         df$rbi_z <- round(as.numeric(scale(df$rbi)), 3)
#         df$avg_z <- round(as.numeric(scale(df$avg)), 3)
#         df$sb_z <- round(as.numeric(scale(BoxCox(df$sb, .45))), 3)
#         
#         df$wins_z <- round(as.numeric(scale(df$wins)), 3)
#         df$saves_z <- round(as.numeric(scale(df$saves)), 3)
#         df$era_z <- round(as.numeric(scale(df$era) * -1), 3)
#         df$so_z <- round(as.numeric(scale(df$so.p)), 3)
#         df$whip_z <- round(as.numeric(scale(df$whip) * -1), 3)
#         
#         df$z_total <- df$hr_z + df$runs_z + df$rbi_z + df$avg_z + df$sb_z + df$wins_z + df$saves_z + df$era_z + df$so_z + df$whip_z
#         
#         df
# }
# 
# all_players1 <- z_score_all(all_players) %>% arrange(desc(z_total))


find_name <- function(name) {
        which(hitters3$name == name)
}


#first, combine all players
all_players <- hitters3 %>%
        full_join(starters2) %>%
        full_join(relievers2) %>%
        arrange(desc(z_pos))

find_name <- function(name) {
        which(all_players$name == name)
}
#next I'll price them
#weight functions for entire player pool pricing
all_players1 <- all_players

# #all_players1$z_unscaled <- all_players1$z_pos - min(all_players1$z_pos)  #this is not how to unscale
# #NEED TO UNSCALE Z_POS
# 
# B <- 1.25375
# all_players1$price <- B ^ all_players1$z_unscaled + 2
# sum(all_players1$price)
# ggplot(all_players1, aes(rank(price), price)) + geom_point()
# 
# #transform prices
# ggplot(all_players1, aes(z_pos)) + geom_histogram()
# ggplot(starters2, aes(z_pos)) + geom_histogram()
# library(grt)
# all_players1$z_new <- unscale(all_players1$z_pos)
# all_players1$z_new <- unscale(all_players1$z_tot)

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
