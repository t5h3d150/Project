library(openxlsx)
library(dplyr)
library(lpSolveAPI)

seasons = c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")

# number of seasons
N = length(seasons)

# number of gameweeks
n = 38

# init objects
models = object = bestXI = gwXI = c()

# read data sets
data0 = readRDS("~/Statistics/UCT/STA4007W/Project/Project/src/R/data/Project_Data.rds")

# important variables
index = c("name", "position", "team_name", "opp_team_name", "round", "GW",
          "total_points", "was_home", "value", "goals_scored", "assists",
          "goals_conceded", "clean_sheets", "saves", "minutes", "own_goals",
          "penalties_missed", "penalties_saved", "yellow_cards", "red_cards", 
          "bonus", "bps", "influence", "creativity", "threat", "ict_index",
          "selected")

# function for team selection
MILPXI = function(dataSet, gw){
  # extract gw data
  # tie breakers after value
  dataFrame = dataSet %>% filter(GW == gw) %>%
    select(name, position, team_name, total_points, bps, goals_scored, goals_conceded, ict_index, value) %>%
    group_by(position) %>% 
    arrange(match(position, c("GK", "DEF", "MID", "FWD")))
  
  # coefficients and directions
  c = dataFrame$total_points
  rows = nrow(dataFrame)
  
  # get position counts
  counts = dataFrame %>% count(position)
  countGK = (counts$n)[3]
  countDEF = (counts$n)[1]
  countMID = (counts$n)[4]
  countFWD = (counts$n)[2]
  
  # aetup constraints
  squad = rep(1, rows)
  GK = c(rep(1, countGK), rep(0, rows - countGK))
  DEF = c(rep(0, countGK), rep(1, countDEF), rep(0, countMID + countFWD))
  MID = c(rep(0, countGK + countDEF), rep(1, countMID), rep(0, countFWD))
  FWD = c(rep(0, rows - countFWD), rep(1, countFWD))
  
  # squad, GK, DEF lower, DEF upper, MID lower, MID upper, FWD lower, FWD upper
  d = c("=", "=", ">=", "<=", ">=", "<=", ">=", "<=")
  b = c(11, 1, 3, 5, 2, 5, 1, 3)
  
  # # constraints matrix
  A = matrix( c(squad, GK, DEF, DEF, MID, MID, FWD, FWD), nrow = length(b), byrow = T)
  
  
  # MILP setup
  model = make.lp(nrow = length(b), ncol = rows)
  lp.control(model, sense = "max")
  set.type(model, 1:rows, type = c("binary"))
  set.objfn(model, c)
  
  # insert constraints
  for (i in 1:length(b)) {
    add.constraint(model, A[i, ], d[i], b[i])
  }
  
  # The idea is to cut off the current integer solution by adding a constraint. Then re-solve. 
  # Stop when no longer optimal or when the objective starts to deteriorate.
  solve(model)
  
  sols = list()
  cost = get.objective(model)
  
  while (TRUE) {
    sol = round(get.variables(model))
    sols = c(sols, list(sol))
    add.constraint(model, (2 * sol) - 1, "<=", sum(sol) - 1)
    constant = solve(model)
    
    if (constant != 0 | get.objective(model) < cost - 1e-6) {
      break
    }
  }
  
  return( list(model = model, data = dataFrame, solutions = sols) )
}

ties = function(solution, dataFrame){
  squad = dataFrame[which(solution == 1, T), ]
  stats = apply(squad[, 4:8], 2, sum)
  return(stats)
}

# compute for all season data
for (i in 1:N) {
  for (j in 1:n) {
    object[[j]] = MILPXI(data0[[i]], j)
  }
  
  models[[i]] = object
  object = c()
}

names(models) = seasons
for (i in 1:N) {
  results = models[[i]]
  for(j in 1:n){
    d = results[[j]]$data
    s = results[[j]]$solutions
    D = data.frame(t(as.matrix(sapply(s, ties, dataFrame = d))))
    D$squad = 1:nrow(D)
    D = arrange(.data = D, desc(bps), desc(goals_scored), goals_conceded, desc(ict_index))
    gwXI[[j]] = d[which(s[[D[1, "squad"]]] == 1, T), ]
  }
  
  bestXI[[i]] = gwXI
  gwXI = c()
}

# save.image(file = "Kings of The Week.RData")
# savehistory(file = "Kings of The Week.Rhistory")
# save.image(file = "Objects.RData")

load(file = "Kings of The Week.RData")
