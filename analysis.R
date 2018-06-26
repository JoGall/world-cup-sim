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

# # find winner frequency
# d1 %>% 
#   filter(round == 5) %>% 
#   select(winner) %>% 
#   group_by(winner) %>% 
#   summarise(n = n()) %>% 
#   mutate(pc = n / sum(n)) %>% 
#   arrange(-n)
# 
# # look at iterations where a particular team won
# d1 %>% 
#   filter(round == 5 & winner == "England")


# 2. Simulate remaining fixtures ------------------------------------------------

n <- 1000

# Update Elo ratings with real results
teams <- teams_bk

real_results2 <- lapply(1:36, function(x) {
  realMatch(real_results[x,], teams)
}) %>% 
  plyr::rbind.fill()
teams_latest <- teams


# simulate remaining fixtures
set.seed(123) #make repeatible

d <- lapply(1:n, function(i) {
  teams <- teams_latest
  simulateTournament(teams, group_matches[37:48,], real_results2[1:36,]) %>% 
    mutate(iter = i)
}) %>% 
  plyr::rbind.fill()

d2 <- probTables(d)

plotTables(d2)

# write.csv(d, "sim_results.csv", row.names = F)


# Final group game predictions ------------------------------------------------

d <- read.csv("./data/sim_results.csv")

dd <- probTables(filter(d, Group == "C"))

# get individual predictions for final two matches
d1 <- d %>% 
  filter(round == 0 & Group == "C" & (team1 == "Australia" & team2 == "Peru")) %>% 
  group_by(winner) %>% 
  summarise(n = n()) %>% 
  mutate(pc = round(n / sum(n) * 100),
         res = c(1, 3, 2))

dd2 <- d %>% 
  filter(round == 0 & Group == "C" & (team1 == "Denmark" & team2 == "France")) %>% 
  group_by(winner) %>% 
  summarise(n = n()) %>% 
  mutate(pc = round(n / sum(n) * 100),
         res = c(1, 3, 2))

# plot table
g0 <- plotTables(mutate(dd, Group = fct_recode(Group, "Group C" = "C")))

# plot match 1 odds
g1 <- ggplot(dd1, aes(res, 1, fill = pc)) +
  geom_tile() +
  geom_text(aes(label = pc), size = 5) +
  scale_fill_gradient(low = "white", high = "#82b446", limits = c(15, 60)) +
  # geom_rect(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 4.5, fill = NA, colour = "black") +
  scale_x_continuous(breaks = c(1:3), labels = c("1", "X", "2"), expand = c(0,0), position = "top") +
  scale_y_continuous(breaks = 1, labels = "Australia", expand = c(0,0),
                     sec.axis = sec_axis(~., breaks = 1, label = "Peru")) +
  guides(fill = F) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 14),
        plot.margin = unit(c(1, 0, 0.5, 0), "cm"))

# plot match 2 odds
g2 <- ggplot(dd2, aes(res, 1, fill = pc)) +
  geom_tile() +
  geom_text(aes(label = pc), size = 5) +
  scale_fill_gradient(low = "white", high = "#82b446", limits = c(15, 60)) +
  # geom_rect(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 4.5, fill = NA, colour = "black") +
  scale_x_continuous(breaks = c(1:3), labels = c("1", "X", "2"), expand = c(0,0), position = "top") +
  scale_y_continuous(breaks = 1, labels = "Denmark", expand = c(0,0),
                     sec.axis = sec_axis(~., breaks = 1, label = "France")) +
  guides(fill = F) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 14),
        plot.margin = unit(c(1, 0, 0.5, 0), "cm"))

# combine plots
plot_grid(g0, g1, g2, ncol = 1, rel_heights = c(0.5, 0.2, 0.2), align = 'v')
