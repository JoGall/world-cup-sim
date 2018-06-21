#==============================================================================
# METHOD
#==============================================================================

# simulate poisson distribution with mean mu, given by:
log(mu) = beta0 + beta1*x1 + beta2*x2

# where:
x1 = elo - elo.opp / 100 #difference between ELO scores
x2 = HA #home advantage binary, [1,0]
beta* = #coefficients modelled by linear regression using historical scores and ELO ratings at that time, [0.1557, 0.169, 0.182]
  beta0 = 0.1557
beta1 = 0.169
beta2 = 0.182

HA <- ifelse(team == "host", 1, ifelse(team.opp == "host", -1))
x1 = elo - elo.opp + HA

# Elo rating algorithm
Rn = Ro + K ? (W - We)

# where:
Rn, Ro # new, old Elo rating
K # competition weight constant (WC = 60), adjusted by goal difference, [GD 1 = 1, GD 2 = 1.5, GD 3+ = 1.625 + ((n - 2) / 8)]
W # result, [1, 0.5, 0] = [win, draw, loss]
We # expected result, where:
We = 1 / (10^(-dr/400) + 1) #where:
dr # difference in ratings plus 100 points for team playing at home

#==============================================================================
# REFERENCES
#==============================================================================
# ELO rating and World Cup scores
# https://www.eloratings.net

# Methods
# https://eightyfivepoints.blogspot.co.uk/2018/05/what-can-we-expect-from-21st-fifa-world.html
# https://github.com/eightyfivepoints/World-Cup-Simulations
# https://fivethirtyeight.com/features/a-chart-for-predicting-penalty-shootout-odds-in-real-time/
#==============================================================================