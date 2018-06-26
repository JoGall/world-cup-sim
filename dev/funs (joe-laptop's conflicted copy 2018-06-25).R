#==============================================================================
# FUNCTIONS
#==============================================================================

# Match simulation engine -----------------------------------------------------

simMatch <- function(match, teams, knockout = F, hotElo = T) {
  
  # starting Elo
  elo.A <- teams[teams$Country == match$team1,]$Elo
  elo.B <- teams[teams$Country == match$team2,]$Elo
  elo.diff <- (elo.A - elo.B) / 100
  
  # home advantage
  ha.A <- ifelse(match$team1 == "Russia", 1, 0)
  ha.B <- ifelse(match$team2 == "Russia", 1, 0)
  
  # parameters for poisson distribution
  mu.A <- exp(0.1557 + (0.169 * (elo.diff + ha.A)))
  mu.B <- exp(0.1557 + (0.169 * (-elo.diff + ha.B)))
  
  ft.A <- rpois(1, mu.A)
  ft.B <- rpois(1, mu.B)
  
  # simulate ET if knockout round and score level after FT
  et.A <- et.B <- pen.A <- pen.B <- NA
  
  if(knockout == T & ft.A == ft.B) {
    et_res <- simET(mu.A, mu.B)
    # return result
    et.A <- et_res[1]
    et.B <- et_res[2] 
    
    # simulate penalties if level AET
    if(ft.A + et.A == ft.B + et.B) {
      # get penalty skill of teams
      team1.p <- teams[teams$Country == match$team1,]$PenaltySkill
      team2.p <- teams[teams$Country == match$team2,]$PenaltySkill
      # simulate
      pen_res <- simPen(team1.p, team2.p)
      #return result
      pen.A <- pen_res[1]
      pen.B <- pen_res[2]
    }
  }
  
  # index winner, loser for match generation functions
  winner <- ifelse(ft.A > ft.B, match$team1, 
                   ifelse(ft.B > ft.A, match$team2,
                          ifelse(et.A > et.B, match$team1, 
                                 ifelse(et.B > et.A, match$team2,
                                        ifelse(pen.A > pen.B, match$team1, 
                                               ifelse(pen.B > pen.A, match$team2))))))
  loser <- ifelse(is.na(winner), NA, c(match$team1, match$team2)[c(match$team1, match$team2) != winner])
  
  # result dataframe
  result <- data.frame(match, ft.A, ft.B, et.A, et.B, pen.A, pen.B, winner, loser) %>% 
    mutate(winner = as.character(winner), loser = as.character(loser))
  
  # compute updated Elo rankings
  elo_update <- updateElo(result, teams)
  
  teams <<- elo_update[[2]] # updated Elo ratings using potentially deadly global assigment operator!
  
  # return result only
  elo_update[[1]]
  
}

# simulate extra time
simET <- function(mu1, mu2) {
  
  # 2.5 just a magic number atm to get semi-realistic number of extra time goals!
  # (ET is 1/3 as many minutes as regular time but produces more goals)
  gET.A <- floor(rpois(1, mu1) / 2.5) 
  gET.B <- floor(rpois(1, mu2) / 2.5)
  
  c(gET.A, gET.B)
  
}

# simulate penalties
simPen <- function(team1.p, team2.p) {
  
  gPen.A = 0
  gPen.B = 0
  
  for(i in 1:50) {
    gPen.A = gPen.A + rbinom(1, 1, prob = team1.p)
    gPen.B = gPen.B + rbinom(1, 1, prob = team2.p)
    if(i < 5) {
      if(gPen.A > gPen.B + (5-i) | gPen.B > gPen.A + (5-i)) {
        return(c(gPen.A, gPen.B))
        break
      }
    }
    else if(i >= 5) {
      if(gPen.A != gPen.B) {
        return(c(gPen.A, gPen.B))
        break
      }
    }
  }
}

# Update functions ------------------------------------------------------------

updateElo <- function(result, teams) {
  
  # parameters
  Score.A <- result$ft.A + ifelse(is.na(result$et.A), 0, result$et.A) + ifelse(is.na(result$pen.A), 0, ifelse(result$pen.A > result$pen.B, 1, 0))
  Score.B <- result$ft.B + ifelse(is.na(result$et.B), 0, result$et.B) + ifelse(is.na(result$pen.B), 0, ifelse(result$pen.B > result$pen.A, 1, 0))
  
  Elo.A <- teams[teams$Country == result$team1,]$Elo
  Elo.B <- teams[teams$Country == result$team2,]$Elo
  Result.A <- ifelse(Score.A > Score.B, 1, ifelse(Score.A < Score.B, 0, 0.5))
  Result.B <- ifelse(Score.B > Score.A, 1, ifelse(Score.B < Score.A, 0, 0.5))
  Exp.Result.A <- (1 / (10 ^ (-(Elo.A-Elo.B)/400) + 1))
  Exp.Result.B <- (1 / (10 ^ (-(Elo.B-Elo.A)/400) + 1))
  G <- ifelse(abs(Score.A - Score.B) == 2, 1.5, ifelse(abs(Score.A - Score.B) >= 3, (11 + abs(Score.A - Score.B)) / 8, 1))
  K <- 60
  
  # compute Elo delta (rounded to integer)
  Delta.A <- as.numeric(K * G * (Result.A - Exp.Result.A)) %>% round
  Delta.B <- as.numeric(K * G * (Result.B - Exp.Result.B)) %>% round
  
  # update input Elo ratings table
  teams[teams$Country == result$team1,]$Elo <- teams[teams$Country == result$team1,]$Elo + Delta.A
  teams[teams$Country == result$team2,]$Elo <- teams[teams$Country == result$team2,]$Elo + Delta.B
  
  result <- data.frame(result, elo.delta.A = Delta.A, elo.delta.B = Delta.B)
  
  return(list(result, teams))
  
}


# Match generation functions --------------------------------------------------

knockoutMatches <- function(group_tables) {
  
  data.frame(team1 = c(group_tables[group_tables$group == "A","team"][1],
                       group_tables[group_tables$group == "C","team"][1],
                       group_tables[group_tables$group == "B","team"][1],
                       group_tables[group_tables$group == "D","team"][1],
                       group_tables[group_tables$group == "E","team"][1],
                       group_tables[group_tables$group == "G","team"][1],
                       group_tables[group_tables$group == "F","team"][1],
                       group_tables[group_tables$group == "H","team"][1]),
             team2 = c(group_tables[group_tables$group == "B","team"][2],
                       group_tables[group_tables$group == "D","team"][2],
                       group_tables[group_tables$group == "A","team"][2],
                       group_tables[group_tables$group == "C","team"][2],
                       group_tables[group_tables$group == "F","team"][2],
                       group_tables[group_tables$group == "H","team"][2],
                       group_tables[group_tables$group == "E","team"][2],
                       group_tables[group_tables$group == "G","team"][2])) %>% 
    mutate(team1 = as.character(team1), 
           team2 = as.character(team2),
           round = 1)
  
}

quarterMatches <- function(knockout_results) {
  
  data.frame(team1 = c(knockout_results[1,"winner"],
                       knockout_results[3,"winner"],
                       knockout_results[5,"winner"],
                       knockout_results[7,"winner"]),
             team2 = c(knockout_results[2,"winner"],
                       knockout_results[4,"winner"],
                       knockout_results[6,"winner"],
                       knockout_results[8,"winner"])) %>% 
    mutate(team1 = as.character(team1), 
           team2 = as.character(team2),
           round = 2)
  
}

semiMatches <- function(quarter_results) {
  
  data.frame(team1 = c(quarter_results[1,"winner"],
                       quarter_results[2,"winner"]),
             team2 = c(quarter_results[3,"winner"],
                       quarter_results[4,"winner"])) %>% 
    mutate(team1 = as.character(team1), 
           team2 = as.character(team2),
           round = 3)
  
}

thirdMatch <- function(semi_results) {
  
  data.frame(team1 = semi_results$loser[1],
             team2 = semi_results$loser[2]) %>% 
    mutate(team1 = as.character(team1),
           team2 = as.character(team2),
           round = 4)
  
}

finalMatch <- function(semi_results) {
  
  data.frame(team1 = semi_results$winner[1],
             team2 = semi_results$winner[2]) %>% 
    mutate(team1 = as.character(team1), 
           team2 = as.character(team2),
           round = 5)
  
}

# Main simulation function ----------------------------------------------------

simulateTournament <- function(teams, group_matches) {
  
  group_results <- lapply(1:nrow(group_matches), function(x) {
    simMatch(group_matches[x,], teams)
  }) %>% 
    plyr::rbind.fill()
  
  group_tables <- group_results %>% 
    group_by(Group) %>% 
    makeTable() %>% 
    as.data.frame()
  
  knockout_matches <- knockoutMatches(group_tables)
  
  knockout_results <- lapply(1:8, function(x) {
    simMatch(knockout_matches[x,], teams, knockout = T)
  }) %>% 
    plyr::rbind.fill()
  
  quarter_matches <- quarterMatches(knockout_results)
  
  quarter_results <- lapply(1:4, function(x) {
    simMatch(quarter_matches[x,], teams, knockout = T)
  }) %>% 
    plyr::rbind.fill()
  
  semi_matches <- semiMatches(quarter_results)
  
  semi_results <- lapply(1:2, function(x) {
    simMatch(semi_matches[x,], teams, knockout = T)
  }) %>% 
    plyr::rbind.fill()
  
  third_match <- thirdMatch(semi_results)
  
  third_result <- simMatch(third_match, teams, knockout = T)
  
  final_match <- finalMatch(semi_results)
  
  final_result <- simMatch(final_match, teams, knockout = T)
  
  # compile complete results
  full_results <- rbind(group_results,
                        rbind(knockout_results,
                              quarter_results,
                              semi_results,
                              third_result,
                              final_result) %>% 
                          mutate(Group = NA))
  
  return(full_results)
  
}

# Real results annealing ------------------------------------------------------

realMatch <- function(result, teams) {
  
  # index winner, loser
  winner <- ifelse(result$ft.A > result$ft.B, result$team1, 
                   ifelse(result$ft.B > result$ft.A, result$team2,
                          ifelse(result$et.A > result$et.B, result$team1, 
                                 ifelse(result$et.B > result$et.A, result$team2,
                                        ifelse(result$pen.A > result$pen.B, result$team1, 
                                               ifelse(result$pen.B > result$pen.A, result$team2))))))
  loser <- ifelse(is.na(winner), NA, c(result$team1, result$team2)[c(result$team1, result$team2) != winner])
  
  # result dataframe
  result <- data.frame(result, winner, loser) %>% 
    mutate(winner = as.character(winner), loser = as.character(loser))
  
  # compute updated Elo rankings
  elo_update <- updateElo(result, teams)
  
  teams <<- elo_update[[2]] # assign updated Elo ratings
  
  elo_update[[1]]
  
}

# Group stage tables ----------------------------------------------------------

makeTable <- function(dat) {
  # TODO: add tiebreaking: GD, GF, H2H, random
  
  dat2 <- rbind(dat %>% 
                  select(group = Group, team = team1, opp = team2, gf = ft.A, ga = ft.B),
                dat %>% 
                  select(group = Group, team = team2, opp = team1, gf = ft.B, ga = ft.A)) %>% 
    mutate(pts = if_else(gf > ga, 3, if_else(gf < ga, 0, 1))) %>% 
    group_by(team) %>% 
    summarise(group = group[1], pts = as.integer(sum(pts)), gf = sum(gf), ga = sum(ga), gd = gf - ga) %>% 
    arrange(group, -pts, -gd, -gf)
  
  # check for ties
  dup_idx <- duplicated(dat2[,-1], fromLast = T) %>% 
    which
  
  if(length(dup_idx) > 0) {
    dat2 <- tiebreaking(dat, dat2, dup_idx)
  }
  
  dat2 %>% 
    mutate(pos = 1:n()) %>% 
    select(group, pos, team, pts, gf, ga, gd)
  
}

tiebreaking <- function(dat, table, dup_idx) {
  
  dat2 <- lapply(dup_idx, function(i) {
    # get teams
    table <- table[i:(i+1),]
    # get their head to head fixture
    dat2 <- filter(dat, (team1 == table$team[1] & team2 == table$team[2]) | (team1 == table$team[2] & team2 == table$team[1]))
    
    dat3a <- dat2 %>% 
      mutate(team = team1, h2h = ft.A - ft.B, cointoss = rbinom(1, 1, 0.5)) %>% 
      select(team, h2h, cointoss)
    
    dat3b <- dat2 %>% 
      mutate(team = team2, h2h = ft.B - ft.A) %>%
      mutate_(cointoss = 1 - dat3a$cointoss) %>% 
      select(team, h2h, cointoss)
    
    rbind(dat3a, dat3b)
    
  }) %>% 
    plyr::rbind.fill()
  
  left_join(table, dat2, by = "team") %>% 
    arrange(-pts, -gd, -gf, -h2h, -cointoss)
  
}

# Diagnostic functions -----------------------------------------------------

probTables <- function(dat) {
  
  dat2 <- dat %>% 
    group_by(iter) %>% 
    slice(1:48) %>% 
    group_by(iter, Group) %>% 
    do(makeTable(.)) %>% 
    group_by(Group, team) %>% 
    mutate(n = n()) %>% 
    group_by(Group, team, pos) %>% 
    summarise(pc = n() / n[1])
  
  scaffold <- dat2 %>% 
    group_by(Group, team) %>% 
    slice(1) %>% 
    expand(pos = 1:4) %>% 
    mutate(pc = 0)
  
  dat3 <- merge(dat2, scaffold, all = T) %>% 
    group_by(Group, team, pos) %>% 
    arrange(-pc) %>% 
    slice(1)
  
  # create scores to generate a rank order of predicted positions
  preds <- dat3 %>% 
    group_by(Group, team) %>%
    arrange(Group, team, pos) %>% 
    mutate(score = pc[1]*4 + pc[2]*3 + pc[3]*2 + pc[4]) %>%
    summarise(score = score[1]) %>% 
    arrange(Group, -score) %>% 
    mutate(pred = 1:4)
  
  dat4 <- left_join(dat3, preds, by = c("Group", "team"))
  
  dat4 %>%  
    arrange(Group, pred, pos) %>% 
    mutate(pc = round(pc, 2))
  
}


plotTables <- function(dat) {
  
  ggplot(filter(dat), aes(as.factor(pos), reorder(team, desc(pred)), fill = pc)) +
    geom_tile() +
    geom_text(aes(label = pc*100)) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_rect(xmin = 0.5, xmax = 4.5, ymin = 0.5, ymax = 4.5, fill = NA, colour = "black") +
    scale_x_discrete(labels = c("1st", "2nd", "3rd", "4th"), expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    guides(fill = F) +
    facet_wrap(~Group, scales = "free", ncol = 4) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 11))
  
}

#==============================================================================