
#setwd("~/Desktop/R_projects")
setwd("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl")  #working directory for toshiba laptop

#libraries
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

#read in data
#df_sal <- read_excel("~/Desktop/R_projects/eiflb_rosters_2017.xlsx", sheet = 2)  #salaries entered manually in this df
#dfh <- read_excel("~/Desktop/R_projects/eiflb_rosters_2017.xlsx", sheet = 3)  #read in hitter 600 proj
#dfp <- read_excel("~/Desktop/R_projects/eiflb_rosters_2017.xlsx", sheet = 4)  #read in pitcher 200/65 proj

#df_grid <- read_excel("~/Desktop/R_projects/eiflb_rosters_2017.xlsx")  #rosters from cbs in grid format

df_sal <- read_excel("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/eiflb_rosters_2017.xlsx", sheet = 2)
dfh <- read_excel("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/eiflb_rosters_2017.xlsx", sheet = 3)
dfp <- read_excel("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/eiflb_rosters_2017.xlsx", sheet = 4)

##### ##### ##### ##### #####

df_grid <- read_excel("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/eiflb_rosters_2017.xlsx")

#global vars and fns
gms_played <- 115  #approximate number of games played for each mlb team
gms_tot <- 162  #total number of single team games in a seson

z_score <- function(stat){  #function to calculate z_score
  scale(stat)
}

#pois <- function(sb_net){
#  ppois(sb_net, 4.5)
#}

pc <- .95 * 200 / 65  #reliever to starter multiplier (keeps 95% of "positive" stats)
pc2 <- 1.05  #starter to reliever weight (gives 5% more credit to reliever)

#gather by player, join with salaries
df_new <- df_grid %>%
  gather(squad, player, assistant_to_the_traveling_secretary:tarrington_johsenators)  #gather the data by player from squad columns
df <- df_new %>%
  left_join(df_sal, by = "player") %>%  #join new df to player salaries
  select(player, squad, position, salary) %>%  #select col's
  arrange(player)  #alphabatize by player

#clean player names: df
df$player <- gsub("\\.", "", df$player)  #remove periods from player names
df$player <- gsub("\\-", " ", df$player)  #remove hyphens from player names
df$player <- gsub("\\'", "", df$player)  #remove apostrophes from player names
df$player <- tolower(df$player)  #convert to lowercase

#hitter column names
colnames(dfh) <- c("player", "team", "g", "pa", "ab", "hits", "doubles", "triples", "hr", "runs", "rbis", "bb", "so", "hbp", "sb", "cs",
                   "void1", "avg", "obp", "slg", "ops", "woba", "void2", "wrc_plus", "bsr", "fld", "void3", "off", "def", "war", "id")

#clean hitter names: dfh
dfh$player <- gsub("\\.", "", dfh$player)  #remove periods from player names
dfh$player <- gsub("\\-", " ", dfh$player)  #remove hyphens from player names
dfh$player <- gsub("\\'", "", dfh$player)  #remove apostrophes from player names
dfh$player <- tolower(dfh$player)  #convert to lowercase
dfh$player <- sub(pattern = " jr$", "", dfh$player)  #remove "jr's" from names

#select hitter columns
dfh1 <- dfh %>%
  select(player, team, avg, runs, hr, rbis, sb, cs, ops) %>%
  filter(!is.na(team)) %>%  #remove players not on a team (assumption)
  mutate(sb_net = sb - cs) %>%  #create stolen base net
  select(-sb, -cs) %>%  #remove individual stolen bases and caught stealing
  arrange(player)  #alphabatize

#pitcher column names
colnames(dfp) <- c("player", "team", "wins", "losses", "era", "gs", "games", "saves", "ip", "hits", "er", "hr", "so", "bb", "whip",
                   "k_rate", "bb_rate", "fip", "war", "ra9_war", "player_id")

#clean pitcher names
dfp$player <- gsub("\\.", "", dfp$player)  #remove periods from player names
dfp$player <- gsub("\\-", " ", dfp$player)  #remove hyphens from player names
dfp$player <- gsub("\\'", "", dfp$player)  #remove apostrophes from player names
dfp$player <- tolower(dfp$player)  #convert to lowercase
dfp$player <- sub(pattern = " jr$", "", dfp$player)  #remove "jr's" from names

#select pitcher columns
dfp1 <- dfp %>%
  select(player, team, ip, wins, so, era, saves, hr, whip) %>%
  filter(!is.na(team)) %>%  #remove players not currently on a team
  mutate(hra = 9 * hr / ip) %>%  #create home runs against rate
  select(-hr) %>%  #remove individual home runs
  arrange(player)  #alphabatize
#change to correct spellings

#merge rosters with hitter and pitcher data
hitters <- df %>%
  left_join(dfh1, by = "player") %>%
  filter(position != "pitcher")
pitchers <- df %>%
  left_join(dfp1, by = "player") %>%
  filter(position == "pitcher")

#generate z-scores for hitters; none yet for sb_net because not normally distributed
hitters$hr_z <- as.numeric(z_score(hitters$hr))
hitters$runs_z <- as.numeric(z_score(hitters$runs))
hitters$rbis_z <- as.numeric(z_score(hitters$rbis))
hitters$avg_z <- as.numeric(z_score(hitters$avg))
hitters$ops_z <- as.numeric(z_score(hitters$ops))
#sum individual z scores into total z score and arrange df
hitters$z_tot <- hitters$hr_z + hitters$runs_z + hitters$rbis_z + hitters$avg_z + hitters$ops_z
hitters <- as.tbl(hitters) %>%
  arrange(desc(z_tot))

#generate z-scores for pitchers;
starters <- pitchers %>%
  filter(saves == 0)
relievers <- pitchers %>%
  filter(saves > 0)

starters$wins_z <- as.numeric(z_score(starters$wins))
starters$so_z <- as.numeric(z_score(starters$so))
starters$era_z <- as.numeric(z_score(starters$era) * -1)
starters$whip_z <- as.numeric(z_score(starters$whip) * -1)
starters$hra_z <- as.numeric(z_score(starters$hra) * -1)

starters$z_tot <- starters$wins_z + starters$so_z + starters$era_z + starters$whip_z + starters$hra_z  #totals: so, hra
starters <- as.tbl(starters) %>%
  arrange(desc(z_tot))

relievers$saves_z <- as.numeric(z_score(relievers$saves))
relievers$so_z <- as.numeric(z_score(relievers$so)) * (65 / 200)
relievers$era_z <- as.numeric(z_score(relievers$era) * -1) * (65 / 200)

relievers$whip_z <- as.numeric(z_score(relievers$whip) * -1) * (65 / 200)
relievers$hra_z <- as.numeric(z_score(relievers$hra) * -1) * (65 / 200)

relievers$z_tot <- relievers$saves_z + relievers$so_z + relievers$era_z + relievers$whip_z + relievers$hra_z
relievers <- as.tbl(relievers) %>%
  arrange(desc(z_tot))

hitters1 <- hitters %>%
  select(player, squad, position, salary, team, hr_z, runs_z, avg_z, rbis_z, ops_z, sb_net, z_tot)
starters1 <- starters %>%
  select(player, squad, position, salary, team, wins_z, so_z, era_z, whip_z, hra_z, z_tot)
relievers1 <- relievers %>%
  select(player, squad, position, salary, team, saves_z, so_z, era_z, whip_z, hra_z, z_tot)

all_players <- hitters1 %>%
  bind_rows(starters1, relievers1) %>%
  arrange(desc(z_tot))

new_df <- all_players
new_df$position <- ifelse(new_df$position == "second_base", "middle_if", new_df$position)
new_df$position <- ifelse(new_df$position == "short_stop", "middle_if", new_df$position)
new_df$position <- ifelse(new_df$position == "second_base", "middle_if", new_df$position)

all_players2 <- new_df %>%
  group_by(position) %>%
  summarize(z_pos_mean = round(mean(z_tot), 2)) %>%
  arrange(desc(z_pos_mean))
all_players3 <- new_df %>%
  left_join(all_players2, by = "position") %>%
  mutate(z_pos = z_tot - z_pos_mean) %>%
  arrange(desc(z_tot))
all_players_ab <- all_players3 %>%  #cut down width of df for exploratory analysis
  select(player, squad, position, salary, z_pos) %>%
  arrange(desc(z_pos))

find_name <- function(name){
  which(all_players$player == name)
}

team_totals <- all_players %>%
  group_by(squad) %>%
  summarize(z_team = sum(z_tot)) %>%
  arrange(desc(z_team))

df_ben2 <- all_players3 %>%
  filter(squad == "moline_belly_itchers") %>%
  arrange(desc(z_tot))

team_totals1 <- all_players3 %>%
  group_by(squad) %>%
  summarize(z_team = sum(z_pos),
            hr_z_team = sum(hr_z, na.rm = TRUE),
            runs_z_team = sum(runs_z, na.rm = TRUE),
            avg_z_team = sum(avg_z, na.rm = TRUE),
            rbis_z_team = sum(rbis_z, na.rm = TRUE),
            ops_z_team = sum(ops_z, na.rm = TRUE),
            sb_net_team = sum(sb_net, na.rm = TRUE),  #added sb_net, no z-score
            wins_z_team = sum(wins_z, na.rm = TRUE),
            so_z_team = sum(so_z, na.rm = TRUE),
            era_z_team = sum(era_z, na.rm = TRUE),
            whip_z_team = sum(whip_z, na.rm = TRUE),
            hra_z_team = sum(hra_z, na.rm = TRUE),
            saves_z_team = sum(saves_z, na.rm = TRUE)) %>%
  arrange(desc(z_team))
#RANKS

#general form: team_totals1$hr_rank <- rank(team_totals1$hr_z_team)

stat_rank <- function(x){
  rank(x)
}
team_totals1$hr_rank <- stat_rank(team_totals1$hr_z_team)
team_totals1$runs_rank <- stat_rank(team_totals1$runs_z_team)
team_totals1$avg_rank <- stat_rank(team_totals1$avg_z_team)
team_totals1$rbis_rank <- stat_rank(team_totals1$rbis_z_team)
team_totals1$ops_rank <- stat_rank(team_totals1$ops_z_team)
team_totals1$sb_rank <- stat_rank(team_totals1$sb_net_team)
team_totals1$wins_rank <- stat_rank(team_totals1$wins_z_team)
team_totals1$so_rank <- stat_rank(team_totals1$so_z_team)
team_totals1$era_rank <- stat_rank(team_totals1$era_z_team)
team_totals1$whip_rank <- stat_rank(team_totals1$whip_z_team)
team_totals1$hra_rank <- stat_rank(team_totals1$hra_z_team)
team_totals1$saves_rank <- stat_rank(team_totals1$saves_z_team)

#total score
team_totals2 <- team_totals1 %>%
  select(hr_rank:saves_rank)
team_totals1$proj_pts <- rowSums(team_totals2)

team_totals3 <- team_totals1 %>%
  select(squad, hr_rank:proj_pts) %>%
  arrange(squad)

#current ranks
team_totals3$pts_curr <- 0

team_totals3$pts_curr <- ifelse(team_totals3$squad == "assistant_to_the_traveling_secretary", 110, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "berteau_bombers", 148, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "chicago_blues", 93.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "chicos_bail_bonds", 60, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "detroit_whole_foods", 98.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "hiawatha_hitmen", 109.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "iowa_falls_fighting_frogs", 113.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "magnolia_maulers", 125, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "marion_revival", 128, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "moline_belly_itchers", 130, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "motown_tigers", 72, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "ravenswood", 71.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "river_north_chavez", 76.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "shellsburg_steamrollers", 118.5, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "st_petersburg_rusty_kuntz", 122, team_totals3$pts_curr)
team_totals3$pts_curr <- ifelse(team_totals3$squad == "tarrington_johsenators", 54.5, team_totals3$pts_curr)

team_totals3$pts_proj_fn <- round((team_totals3$proj_pts * (gms_tot - gms_played) + team_totals3$pts_curr * gms_played) / gms_tot, 
                                  digits = 1)
team_totals3 <- arrange(team_totals3, desc(pts_proj_fn))

#########################

roto_ranks <- read_excel("C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/eiflb_rosters_2017.xlsx", sheet = 7, skip = 1)

avg_ranks <- roto_ranks[1:16, 1:3]
era_ranks <- roto_ranks[1:16, 5:7]
hr_ranks <- roto_ranks[18:33, 1:3]
hra_ranks <- roto_ranks[18:33, 5:7]
ops_ranks <- roto_ranks[35:50, 1:3]
so_ranks <- roto_ranks[35:50, 5:7]
runs_ranks <- roto_ranks[52:67, 1:3]
saves_ranks <- roto_ranks[52:67, 5:7]
rbis_ranks <- roto_ranks[69:84, 1:3]
wins_ranks <- roto_ranks[69:84, 5:7]
sb_net_ranks <- roto_ranks[86:101, 1:3]
whip_ranks <- roto_ranks[86:101, 5:7]

colnames(avg_ranks) <- c("squad", "avg", "avg_rank_y")
colnames(era_ranks) <- c("squad", "era", "era_rank_y")
colnames(hr_ranks) <- c("squad", "hr", "hr_rank_y")
colnames(hra_ranks) <- c("squad", "hra", "hra_rank_y")
colnames(ops_ranks) <- c("squad", "ops", "ops_rank_y")
colnames(so_ranks) <- c("squad", "so", "so_rank_y")
colnames(runs_ranks) <- c("squad", "runs", "runs_rank_y")
colnames(saves_ranks) <- c("squad", "saves", "saves_rank_y")
colnames(rbis_ranks) <- c("squad", "rbis", "rbis_rank_y")
colnames(wins_ranks) <- c("squad", "wins", "wins_rank_y")
colnames(sb_net_ranks) <- c("squad", "sb_net", "sb_net_rank_y")
colnames(whip_ranks) <- c("squad", "whip", "whip_rank_y")

roto_ranks1 <- avg_ranks %>%
  left_join(era_ranks) %>%
  left_join(hr_ranks) %>%
  left_join(hra_ranks) %>%
  left_join(ops_ranks) %>%
  left_join(so_ranks) %>%
  left_join(runs_ranks) %>%
  left_join(saves_ranks) %>%
  left_join(rbis_ranks) %>%
  left_join(wins_ranks) %>%
  left_join(sb_net_ranks) %>%
  left_join(whip_ranks) %>%
  select(squad, hr_rank_y, runs_rank_y, avg_rank_y, rbis_rank_y, ops_rank_y, sb_net_rank_y, wins_rank_y, so_rank_y, 
         era_rank_y, whip_rank_y, hra_rank_y, saves_rank_y)

roto_ranks1$squad <- tolower(roto_ranks1$squad)  #change squad names to lowercase

roto_ranks1$squad <- gsub(" ", "_", roto_ranks1$squad)

#remove punctuation from squad names in roto_ranks1
roto_ranks1$squad <- gsub("\\.", "", roto_ranks1$squad)  #remove periods from player names
roto_ranks1$squad <- gsub("\\-", "", roto_ranks1$squad)  #remove hyphens from player names
roto_ranks1$squad <- gsub("\\'", "", roto_ranks1$squad)  #remove apostrophes from player names

roto_ranks1$hr_rank_y <- as.numeric(roto_ranks1$hr_rank_y)
roto_ranks1$runs_rank_y <- as.numeric(roto_ranks1$runs_rank_y)
roto_ranks1$avg_rank_y <- as.numeric(roto_ranks1$avg_rank_y)
roto_ranks1$rbis_rank_y <- as.numeric(roto_ranks1$rbis_rank_y) 
roto_ranks1$ops_rank_y <- as.numeric(roto_ranks1$ops_rank_y)
roto_ranks1$sb_net_rank_y <- as.numeric(roto_ranks1$sb_net_rank_y)
roto_ranks1$wins_rank_y <- as.numeric(roto_ranks1$wins_rank_y)
roto_ranks1$so_rank_y <- as.numeric(roto_ranks1$so_rank_y)
roto_ranks1$era_rank_y <- as.numeric(roto_ranks1$era_rank_y)
roto_ranks1$whip_rank_y <- as.numeric(roto_ranks1$whip_rank_y)
roto_ranks1$hra_rank_y <- as.numeric(roto_ranks1$hra_rank_y)
roto_ranks1$saves_rank_y <- as.numeric(roto_ranks1$saves_rank_y)

team_totals4 <- team_totals3 %>%
  left_join(roto_ranks1, by = "squad") %>%
  mutate(hr_n = ((gms_tot - gms_played) * hr_rank + gms_played * hr_rank_y) / gms_tot,
         runs_n = ((gms_tot - gms_played) * runs_rank + gms_played * runs_rank_y) / gms_tot,
         avg_n = ((gms_tot - gms_played) * avg_rank + gms_played * avg_rank_y) / gms_tot,
         rbis_n = ((gms_tot - gms_played) * rbis_rank + gms_played * rbis_rank_y) / gms_tot,
         ops_n = ((gms_tot - gms_played) * ops_rank + gms_played * ops_rank_y) / gms_tot,
         sb_n = ((gms_tot - gms_played) * sb_rank + gms_played * sb_net_rank_y) / gms_tot,
         wins_n = ((gms_tot - gms_played) * wins_rank + gms_played * wins_rank_y) / gms_tot,
         so_n = ((gms_tot - gms_played) * so_rank + gms_played * so_rank_y) / gms_tot,
         era_n = ((gms_tot - gms_played) * era_rank + gms_played * era_rank_y) / gms_tot,
         whip_n = ((gms_tot - gms_played) * whip_rank + gms_played * whip_rank_y) / gms_tot,
         hra_n = ((gms_tot - gms_played) * hra_rank + gms_played * hra_rank_y) / gms_tot,
         saves_n = ((gms_tot - gms_played) * saves_rank + gms_played * saves_rank_y) / gms_tot) %>%
  select(squad, hr_n, runs_n, avg_n, rbis_n, ops_n, sb_n, wins_n, so_n, era_n, whip_n, hra_n, saves_n)

team_totals4$hr <- stat_rank(team_totals4$hr_n)
team_totals4$runs <- stat_rank(team_totals4$runs_n)
team_totals4$avg <- stat_rank(team_totals4$avg_n)
team_totals4$rbis <- stat_rank(team_totals4$rbis_n)
team_totals4$ops <- stat_rank(team_totals4$ops_n)
team_totals4$sb <- stat_rank(team_totals4$sb_n)
team_totals4$wins <- stat_rank(team_totals4$wins_n)
team_totals4$so <- stat_rank(team_totals4$so_n)
team_totals4$era <- stat_rank(team_totals4$era_n)
team_totals4$whip <- stat_rank(team_totals4$whip_n)
team_totals4$hra <- stat_rank(team_totals4$hra_n)
team_totals4$saves <- stat_rank(team_totals4$saves_n)

projections <- team_totals4 %>%
  select(squad, hr:saves) %>%
  mutate(proj_pts = hr + runs + avg + rbis + ops + sb + wins + so + era + whip + hra + saves) %>%
  arrange(desc(proj_pts))

#########################

#library(readr)
#write_csv(projections, path = "C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/fantasy_baseball_projections.csv")
#write_csv(all_players3, path = "C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/fantasy_baseball_projections_1.csv")
#write_csv(df_ben2, path = "C:/Users/Ben/Desktop/Daily Fantasy/baseball/eifbl/fantasy_baseball_projections_2.csv")
