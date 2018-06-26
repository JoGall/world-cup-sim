library(tidyverse)

# READ DATA -------------------------------------------------------------------

# Elo ranking data (important this is not called 'team' as this variable modified)
teams_bk <- read.csv("./data/teams.csv", stringsAsFactors = F)

# group stage matches
group_matches <- read.csv("./data/group_matches.csv", stringsAsFactors = F)

# real results so far
real_results <- read.csv("./data/real_results.csv", stringsAsFactors = F)


# 1. Simulate tournament from scratch -----------------------------------------

# simulate n tournaments
set.seed(123) #make repeatible
n <- 100

d1 <- lapply(1:n, function(i) {
  teams <- teams_bk
  simulateTournament(teams, group_matches) %>% 
    mutate(iter = i)
}) %>% 
  plyr::rbind.fill()

# build tables
dd1 <- probTables(d1)

# visualise tables
plotTables(dd1)

# find winner frequency
d1 %>% 
  filter(round == 5) %>% 
  select(winner) %>% 
  group_by(winner) %>% 
  summarise(n = n()) %>% 
  mutate(pc = n / sum(n)) %>% 
  arrange(-n)

# look at iterations where a particular team won
d1 %>% 
  filter(round == 5 & winner == "England")


# 2. Simulate remaining fixtures ------------------------------------------------

n <- 5000

# Update Elo ratings with real results
teams <- teams_bk

real_results2 <- lapply(1:29, function(x) {
  realMatch(real_results[x,], teams)
}) %>% 
  plyr::rbind.fill()
teams_latest <- teams


# simulate remaining fixtures
set.seed(123) #make repeatible

d2 <- lapply(1:n, function(i) {
  teams <- teams_latest
  rbind(real_results2[1:29,],
    simulateTournament(teams, group_matches[30:48,])) %>% 
    mutate(iter = i)
}) %>% 
  plyr::rbind.fill()

dd2 <- probTables(d2)

plotTables(dd2)


# plot latest predicted group tables

d <- read.csv("sim_results.csv")

dd <- probTables(d)

plotTables(dd)

# get outcomes of simulations from different scenarios, e.g. England win vs. Belgium
idx <- filter(d2, team1 == "England" & team2 == "Belgium" & round == 0 & winner == "Belgium")$iter

filter(d2, iter %in% idx & team1 == "Belgium" & team2 == "Panama" & round == 0 & winner == "Panama")$iter

filter(d2, iter %in% idx & round == 5) %>% 
  group_by(winner) %>%
  summarise(n = n()) %>% 
  mutate(pc = n / sum(n)) %>% 
  arrange(-n)
  
