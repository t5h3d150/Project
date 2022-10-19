library(openxlsx)
library(dplyr)
library(lpSolveAPI)

# objects
bestXV = models = object = c()

# load objects
load(file = "Objects.RData")
summaryStats = readRDS("~/Statistics/UCT/STA4007W/Project/Project/src/R/data/Summary_Stats.rds")
data0 = readRDS("~/Statistics/UCT/STA4007W/Project/Project/src/R/data/Project_Data.rds")

colnames(summaryStats[[1]]) = c("name", "total_points", "minutes", "goals_scored", "assists", "clean_sheets", 
                                "goals_conceded", "own_goals", "penalties_saved", "penalties_missed", "yellow_cards", 
                                "red_cards", "saves", "bonus", "bps", "influence", "creativity", "threat", "ict_index", 
                                "position", "value", "team_name")

summaryStats[[1]] = summaryStats[[1]][, index[-match(c("GW", "round", "selected", "opp_team_name", "was_home"), index)]]

MILPXV = function(dataSet){
    # tie breakers after value
    dataFrame = dataSet %>%
        select(name, position, team_name, total_points, bps, goals_scored, goals_conceded, ict_index, value) %>%
        group_by(position) %>% arrange(match(position, c("GK", "DEF", "MID", "FWD")))
    
    # teams matrix
    M = sapply(dataFrame$team_name, FUN = function(x, y){return(x == unique(y))}, y = dataFrame$team_name)
    rownames(M) = unique(dataFrame$team_name)
    colnames(M) = NULL
    
    # coefficients and directions
    c = dataFrame$total_points
    a = dataFrame$value
    players = nrow(dataFrame)
    
    # get position counts
    counts = dataFrame %>% count(position)
    countGK = (counts$n)[3]
    countDEF = (counts$n)[1]
    countMID = (counts$n)[4]
    countFWD = (counts$n)[2]
    
    # setup constraints
    squad = rep(1, players)
    GK = c(rep(1, countGK), rep(0, players - countGK))
    DEF = c(rep(0, countGK), rep(1, countDEF), rep(0, countMID + countFWD))
    MID = c(rep(0, countGK + countDEF), rep(1, countMID), rep(0, countFWD))
    FWD = c(rep(0, players - countFWD), rep(1, countFWD))
    
    # budget, squad, GK, DEF, MID, FWD
    d = c("<=", "=", "=", "=", "=", "=")
    b = c(100, 15, 2, 5, 5, 3)
    
    ## constraints matrix
    A = matrix( c(a, squad, GK, DEF, MID, FWD), nrow = length(b), byrow = T)
    
    # MILP setup
    model = make.lp(nrow = length(b), ncol = players)
    lp.control(lprec = model, sense = "max")
    set.type(lprec = model, columns = 1:players, type = c("binary"))
    set.objfn(lprec = model, obj = c)
    
    # insert squad selection constraints
    for (i in 1:length(b)) {
        add.constraint(lprec = model, xt = A[i, ], type = d[i], rhs = b[i])
    }
    
    # insert team max constraints on player selection
    for (i in 1:dim(M)[1]) {
        add.constraint(lprec = model, xt = M[i, ], type = "<=", rhs = 3)
    }
    
    # The idea is to cut off the current integer solution by adding a constraint. Then re-solve. 
    # Stop when no longer optimal or when the objective starts to deteriorate.
    solve(model)
    
    sols = list()
    cost = get.objective(lprec = model)
    
    while (TRUE) {
        sol = round(get.variables(model))
        sols = c(sols, list(sol))
        add.constraint(lprec = model, xt = (2 * sol) - 1, type = "<=", rhs = sum(sol) - 1)
        constant = solve(model)
        
        if (constant != 0 | get.objective(lprec = model) < cost - 1e-6) {
            break
        }
    }
    
    return( list(model = model, data = dataFrame, solutions = sols) )
}

MILPXI = function(dataFrame){
    # coefficients and directions
    c = dataFrame$total_points
    rows = nrow(dataFrame)
    
    # get position counts
    counts = dataFrame %>% count(position)
    countGK = (counts$n)[3]
    countDEF = (counts$n)[1]
    countMID = (counts$n)[4]
    countFWD = (counts$n)[2]
    
    # setup constraints
    squad = rep(1, rows)
    GK = c(rep(1, countGK), rep(0, rows - countGK))
    DEF = c(rep(0, countGK), rep(1, countDEF), rep(0, countMID + countFWD))
    MID = c(rep(0, countGK + countDEF), rep(1, countMID), rep(0, countFWD))
    FWD = c(rep(0, rows - countFWD), rep(1, countFWD))
    constraints = 8
    
    # # constraints matrix
    A = matrix( c(squad, GK, DEF, DEF, MID, MID, FWD, FWD), nrow = constraints, byrow = T)
    
    # squad, GK, DEF lower, DEF upper, MID lower, MID upper, FWD lower, FWD upper
    d = c("=", "=", ">=", "<=", ">=", "<=", ">=", "<=")
    b = c(11, 1, 3, 5, 2, 5, 1, 3)
    
    # MILP setup
    model = make.lp(nrow = constraints, ncol = rows)
    lp.control(model, sense = "max")
    set.type(model, 1:rows, type = c("binary"))
    set.objfn(model, c)
    
    # create a for loop to insert constraints
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

# compute for all season data
for (i in 1:1) {
    models[[i]] = MILPXV(summaryStats[[i]])
    object = c()
}

# names(models) = seasons
for (i in 1:1) {
    d = models[[i]]$data
    s = models[[i]]$solutions
    D = data.frame(t(as.matrix(sapply(s, ties, dataFrame = d))))
    D$squad = 1:nrow(D)
    D = arrange(.data = D, desc(bps), desc(goals_scored), goals_conceded, desc(ict_index))
    
    bestXV[[i]] = d[which(s[[D[1, "squad"]]] == 1, T), ]
}

# run once on cumstats to get optimal squad
View(bestXV[[1]])

# for loop to select same players over and over again
    # sum up the points
# select starting XI for each gw
# double captain points (one with max points in certain gw)

# save.image(file = "Buy and Hold Team.RData")
# savehistory(file = "Buy and Hold Team.Rhistory")
# save.image(file = "Objects.RData")
# load(file = "Buy and Hold Team.RData")