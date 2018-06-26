i <- 1
dat <- filter(d2, Group == "F", iter == i)

dat <- read.csv("tiebreak_test.csv") %>% 
  mutate(Group = "F")

dat2 <- rbind(dat %>% 
                select(group = Group, team = team1, opp = team2, gf = ft.A, ga = ft.B),
              dat %>% 
                select(group = Group, team = team2, opp = team1, gf = ft.B, ga = ft.A)) %>% 
  mutate(pts = if_else(gf > ga, 3, if_else(gf < ga, 0, 1))) %>% 
  group_by(team) %>% 
  summarise(Group = group[1], pts = as.integer(sum(pts)), gf = sum(gf), ga = sum(ga), gd = gf - ga) %>% 
  arrange(-pts, -gd, -gf)

# if duplicated rows exist
if(length(which(dup_idx))
   
dup_idx <- duplicated(dat2[,-1], fromLast = T) %>% 
  which

dat3 <- lapply(dup_idx, function(i) {
  # get teams
  dat3 <- dat2[i:(i+1),]
  # get their head to head fixture
  dat4 <- filter(dat, (team1 == dat3$team[1] & team2 == dat3$team[2]) | (team1 == dat3$team[2] & team2 == dat3$team[1]))
  
  dat5a <- dat4 %>% 
    mutate(team = team1, h2h = ft.A - ft.B, cointoss = rbinom(1, 1, 0.5)) %>% 
    select(team, h2h, cointoss)
  
  dat5b <- dat4 %>% 
    mutate(team = team2, h2h = ft.B - ft.A) %>%
    mutate_(cointoss = 1 - dat5a$cointoss) %>% 
    select(team, h2h, cointoss)
  
  rbind(dat5a, dat5b)
  
}) %>% 
  plyr::rbind.fill()

left_join(dat2, dat3, by = "team") %>% 
  arrange(-pts, -gd, -gf, -h2h, -cointoss) %>% 
  mutate()
