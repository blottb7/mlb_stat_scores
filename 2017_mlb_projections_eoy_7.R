#Code for generalized yearlong category data

#TO DO
#pitchers: next! (done)
#1.) fn for sb_net distribution

#create criteria to assign a multi-position player to his most valuable position
#user selected stats
#convert
#add bench players? how to weight them?
#explore other scale_ fn's
#all stat distributions look like some type of poisson distribution:
#find the R function for self selection of poisson distribution parameters

#Set working directory
setwd("~/Desktop/R_projects/baseball/eiflb")  #apple
#setwd("C:/Users/Ben/Desktop/FF/baseball")  #asus
#setwd("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl")  #working directory for toshiba laptop

#libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

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
relief_pitchers <- 3

#user selected stats
  #from fangraphs:
#avg, rbi, r, sb, hr, obp, slg, ops, h, so, 2b, 3b, tb, bb, rbi+r, xBH, sb-cs, woba
#w, sv, era, whip, so, avg, k/9, bb/9, k/bb, fip, ip, hr, hld, qs

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
# z_score <- function(stat){
#   scale(stat)
# }
# 
# z_total <- function(df, hit_stat1, hit_stat2, hit_stat3, hit_stat4, hit_stat5) {
#   df$zcore = df$hit_stat1 + df$hit_stat2 + df$hit_stat3 + df$hit_stat4 + df$hit_stat5
# }

# #z_score function for starting pitchers
# z_score_sp <- function(df, n_df) {
#   df <- df %>%
#     filter(games > 1,  #many players have a default of "1" game played for "just in case they get moved to the majors" and other scenarios
#            gs > 0) %>%  #filter for pitchers who project to start at least one game
#     select(name, team, gs, games, ip, wins, era, saves, hra, so, whip)
#   
#   df$wins_z <- as.numeric(z_score(df$wins))
#   df$era_z <- as.numeric(z_score(df$era) * -1)
#   df$hra_z <- as.numeric(z_score(df$hra) * -1)
#   df$so_z <- as.numeric(z_score(df$so))
#   df$whip_z <- as.numeric(z_score(df$whip) * -1)
#   
#   df <- df %>%
#     mutate(z_score = wins_z + era_z + hra_z + so_z + whip_z) %>%
#     arrange(desc(z_score)) %>%
#     head(n_df)
# }

#read in data
catchers <- read_excel("2018_fangraphs_projections.xlsx", sheet = 2)
first_basemen <- read_excel("2018_fangraphs_projections.xlsx", sheet = 3)
second_basemen <- read_excel("2018_fangraphs_projections.xlsx", sheet = 4)
third_basemen <- read_excel("2018_fangraphs_projections.xlsx", sheet = 5)
shortstops <- read_excel("2018_fangraphs_projections.xlsx", sheet = 6)
outfielders <- read_excel("2018_fangraphs_projections.xlsx", sheet = 7)
# pitchers <- read_excel("2018_fangraphs_projections.xlsx", sheet = 8)
# 
# #pitcher names need to be re-edited because there is a different length than in the 2017 df
# #these are the names for 2018 df (does not include column: "adp")
# names(pitchers) <- c("name", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hra", "so", "bb",
#                      "whip", "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")
# 
# #for now, treat starters and relievers as two completely separate categories
# starters <- pitchers %>%
#   arrange(desc(wins), fip)
# starters <- starters[1:n_starting_pitchers,]
# starters <- z_score_sp(pitchers, n_starting_pitchers)
# starters1 <- z_score_sp(starters, n_starting_pitchers)
# 
# #all pitchers
# all_pitchers <- z_score_all_p(pitchers, n_starting_pitchers + n_relief_pitchers)
# all_pitchers1 <- z_score_all_p(all_pitchers, n_starting_pitchers + n_relief_pitchers)
# 
# #z_score function for relief pitchers
# z_score_rp <- function(df, n_df) {
#   df <- df %>%
#     filter(games > 1,  #many players have a default of "1" game played for "just in case they get moved to the majors" and other scenarios
#            gs == 0) %>%  #filter for pitchers who project to start at least one game
#     select(name, team, gs, games, ip, wins, era, saves, hra, so, whip)
#   
#   df$wins_z <- as.numeric(z_score(df$wins) * df$wins / mean(starters$wins))
#   df$saves_z <- as.numeric(z_score(df$saves))
#   df$era_z <- as.numeric(z_score(df$era) * -1 * df$ip / mean(starters1$ip))
#   df$hra_z <- as.numeric(z_score(df$hra) * -1)
#   df$so_z <- as.numeric(z_score(df$so) * df$so / mean(starters1$so))
#   df$whip_z <- as.numeric(z_score(df$whip) * -1 * df$ip / mean(starters1$ip))
#   
#   df <- df %>%
#     mutate(z_score = saves_z + era_z + hra_z + so_z + whip_z) %>%
#     arrange(desc(z_score)) %>%
#     head(n_df)
# }
# 
# #relief pitchers dfs
# relievers <- pitchers %>%
#   arrange(desc(saves))
# relievers <- relievers[1:48,]
# #relievers <- z_score_rp(pitchers, n_relief_pitchers)
# relievers1 <- z_score_rp(relievers, n_relief_pitchers)
# 
# #NOTES
# #a one unit change in a reliever counting stat category should be the same as a one unit change in a starter counting stat cat.
# #likewise, there should be a regulator for rate stats base on ip or pa.
# #So! maybe need to change everthing into weights or everything into counting stats
# #What would W/IP look like? No, each win should be worth a certain amount of Z.
# 
# #probably first need a straight scale. then have a second statistic that describes how it affects team outcome
# #this second stat will be weighted with innings
# 
# #combine starters and relievers
# pitchers1 <- starters1 %>%
#   full_join(relievers1) %>%
#   arrange(desc(z_score))

#rename position player vars
#name_vector for 2018 df; does not include column: "adp"
name_vector <- c("name", "team", "games", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb", "so",
                 "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
                 "waste3", "offense", "defense", "war", "playerid")
#name_vector for 2017 df; includes column: "adp"
#name_vector <- c("name", "team", "games", "pa", "ab", "hit", "double", "triple", "hr", "runs", "rbi", "bb", "so",
#                 "hbp", "sb", "cs", "waste1", "avg", "obp", "slg", "ops", "woba", "waste2", "wrc_plus", "bsr", "fld",
#                 "waste3", "offense", "defense", "war", "waste4", "adp", "playerid")

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

#combine all positions into a single df
hitters <- catchers %>%
  full_join(first_basemen) %>%
  full_join(second_basemen) %>%
  full_join(third_basemen) %>%
  full_join(shortstops) %>%
  full_join(outfielders) %>%
  filter(pa > 1)  #get this done out of the gate

#SEARCH FOR MULTI-POSITION PLAYERS AND DUPLICATE NAMES
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

#create vector of position rankings, with the least productive position, catcher, having the most relative value
  #and assiging the most valuable position to each duplicate player
    #call that new var "pos_rank"
#This ranks position, assigning the least productive position to each player to increase value
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
#combine dfs so there is only one line for each player
  #each player has the most valuable position
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
#clean working space
rm(hitters_names, duplicated_names, duplicated_names_copy, duplicated_names1, duplicated_names2, duplicated_names3)
rm(catchers, first_basemen, second_basemen, shortstops, third_basemen, outfielders)

#subset by "regulars"; define as 300 or more plate appearances
hitters_regulars <- hitters %>%
  filter(pa >= 300) %>%  #will not want players with less than half a season of at bats, so filter for this
  arrange(name)
#save the reserve hitters for later comparison, i.e. for guys you may want to stream or add later in the year
hitters_reserves <- hitters %>%
  filter(pa < 300) %>%
  arrange(desc(woba))

#create general form of function for creating z-score for position players
#avg, rbi, r, sb, hr, obp, slg, ops, h, so, 2b, 3b, tb, bb, rbi+r, xBH, sb-cs, woba
z_score_position <- function(df) {
  # df <- df %>% 
  #   filter(pa > 1) %>%  #the majority of the df's contain players with 1 pa, presumably for ratio data if they do get "called up"
  #   select(name, team, pos, games, pa, avg, runs, hr, rbi, sb_net, ops)  #select stats used for fantasy league
  
  #create a z_score for all possible statistics:
  df$hit_z <- as.numeric(z_score(df$hit))
  df$double_z <- as.numeric(z_score(df$double))
  df$triple_z <- as.numeric(z_score(df$triple))
  df$hr_z <- as.numeric(z_score(df$hr))
  df$runs_z <- as.numeric(z_score(df$runs))
  df$rbi_z <- as.numeric(z_score(df$rbi))
  df$bb_z <- as.numeric(z_score(df$bb))
  df$so_z <- as.numeric(z_score(df$so))
  df$sb_z <- as.numeric(z_score(df$sb))
  df$avg_z <- as.numeric(z_score(df$avg))
  df$obp_z <- as.numeric(z_score(df$obp))
  df$slg_z <- as.numeric(z_score(df$slg))
  df$ops_z <- as.numeric(z_score(df$ops))
  df$woba_z <- as.numeric(z_score(df$woba))
  df$tb_z <- as.numeric(z_score(df$tb))
  df$rbi_r_z <- as.numeric(z_score(df$rbi_r))
  df$xbh_z <- as.numeric(z_score(df$xbh))
  df$sb_net_z <- as.numeric(z_score(df$sb_net))
  
  df
  # df <- df %>%
  #   mutate(z_score = avg_z + runs_z + hr_z + rbi_z + ops_z) %>%
  #   arrange(desc(z_score)) %>%
  #   head(n_df)
}

#run z_score across all hitters
hitters_regulars <- z_score_position(hitters_regulars) %>%  #want this for now
  arrange(desc(woba_z))
#hitters_new1 <- z_score_position(hitters_new)

#filter each position and run z-score pos on each position df
catchers1 <- hitters_regulars %>% filter(pos == "2")
first_basemen1 <- hitters_regulars %>% filter(pos == "3")
second_basemen1 <- hitters_regulars %>% filter(pos == "4")
third_basemen1 <- hitters_regulars %>% filter(pos == "5")
shortstops1 <- hitters_regulars %>% filter(pos == "6")
outfielders1 <- hitters_regulars %>% filter(pos == "7")

#create positive-relative z_scores
catchers1 <- z_score_position(catchers1) %>%
  mutate(z_pos = avg_z + hr_z + runs_z + rbi_z + sb_net_z + ops_z) %>%
  arrange(desc(z_pos))
first_basemen1 <- z_score_position(first_basemen1)
second_basemen1 <- z_score_position(second_basemen1)
third_basemen1 <- z_score_position(third_basemen1)
shortstops1 <- z_score_position(shortstops1)
outfielders1 <- z_score_position(outfielders1)

##### Here is where to input user selected hitter stats. #####

catchers1 <- 

hitters_position_relative_zscores <- catchers1 %>%
  full_join(first_basemen1) %>%
  full_join(second_basemen1) %>%
  full_join(third_basemen1) %>%
  full_join(shortstops1) %>%
  full_join(outfielders1) %>%
  arrange(desc(woba_z))

#####
#Start work on pitchers here
  #I'm going to need two sets of pitcher dfs... one for leagues that require sp and rp, and one that does not have designations, but has inning requirements

#####

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

hitters_zpos2 <- hitters_zpos1 %>%
  arrange(desc(z_score))
# 
# ##### ##### ##### ##### #####
# 
# #global vars and fns
# gms_played <- 115  #approximate number of games played for each mlb team
# gms_tot <- 162  #total number of single team games in a seson
# 
# z_score <- function(stat){  #function to calculate z_score
#   scale(stat)
# }
# 
# #pois <- function(sb_net){
# #  ppois(sb_net, 4.5)
# #}
# 
# pc <- .95 * 200 / 65  #reliever to starter multiplier (keeps 95% of "positive" stats)
# pc2 <- 1.05  #starter to reliever weight (gives 5% more credit to reliever)
# 
# #gather by player, join with salaries
# df_new <- df_grid %>%
#   gather(squad, player, assistant_to_the_traveling_secretary:tarrington_johsenators)  #gather the data by player from squad columns
# df <- df_new %>%
#   left_join(df_sal, by = "player") %>%  #join new df to player salaries
#   select(player, squad, position, salary) %>%  #select col's
#   arrange(player)  #alphabatize by player

#clean player names: dff poisson distribution parameters
ggplot(hitters_zpos1, aes(avg)) + geom_histogram(binwidth = .003)
ggplot(hitters_zpos1, aes(runs)) + geom_histogram(binwidth = 2)
ggplot(hitters_zpos1, aes(hr)) + geom_histogram(binwidth = 1)
ggplot(hitters_zpos1, aes(rbi)) + geom_histogram(binwidth = 2)
ggplot(hitters_zpos1, aes(ops)) + geom_histogram(binwidth = .01)
ggplot(hitters_zpos1, aes(sb_net)) + geom_histogram(binwidth = 1)

ggplot(starters1, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(relievers1, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(pitchers1, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(all_pitchers1, aes(wins)) + geom_histogram(binwidth = 1)
ggplot(all_pitchers1, aes(whip)) + geom_histogram(binwidth = .025)
ggplot(pitchers, aes(whip)) + geom_histogram(binwidth = .025)
#write.csv(hitters_zpos_samp, file = "C:/Users/Ben/Desktop/FF/baseball/hitters_zpos_samp.csv")
#write.csv(hitters_zscore_samp, file = "C:/Users/Ben/Desktop/FF/baseball/hitters_zscore_samp.csv")

pois <- function(stat){
  ppois(stat, 4.5)
}

sb_pois <- ppois(hitters_zpos1$sb_net, 3.5)
hist(sb_pois)

dpois(hitters_zpos1$sb_net, 3.5)
ppois(hitters_zpos1$sb_net, 3.5)
qpois(hitters_zpos1$sb_net, 3.5)
rpois(hitters_zpos1$sb_net, 3.5)

hist(ppois(hitters_zpos1$sb_net, mean(hitters_zpos1$sb_net)))
mean(hitters_zpos1$sb_net)
sd(hitters_zpos1$sb_net)
