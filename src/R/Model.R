library(openxlsx)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(lpSolveAPI)

# simulation
# parameters: transfer cap, number of gameweeks before incoproating new players, gamweeks to play chips

# load data
load(file = "Objects.RData")

# list to store cumulative players stats
STATS = bestXV = bestXI = records = full = KPI = c()

# get 2016-17 cum stats and store in list
summaryStats = readRDS("~/Statistics/UCT/STA4007W/Project/Project/src/R/data/Summary_Stats.rds")
data0 = readRDS("~/Statistics/UCT/STA4007W/Project/Project/src/R/data/Project_Data.rds")
index0 = c("name", "total_points", "minutes", "goals_scored", "assists", "clean_sheets", 
           "goals_conceded", "own_goals", "penalties_saved", "penalties_missed", "yellow_cards", 
           "red_cards", "saves", "bonus", "bps", "influence", "creativity", "threat", "ict_index", 
           "position", "value", "team_name")

# function to select best XI: change nothing
MILPXI = function(dataFrame, metric){
    # coefficients and directions
    c = dataFrame[, metric]; rows = nrow(dataFrame)
    
    # get position counts
    counts = dataFrame %>% count(position); countGK = (counts$n)[3]
    countDEF = (counts$n)[1]; countMID = (counts$n)[4]; countFWD = (counts$n)[2]
    
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

# function to select best XV: change nothing
MILPXV = function(dataSet, metric){
    # tie breakers after value
    dataFrame = dataSet %>%
        select(name, position, team_name, total_points, bps, goals_scored, goals_conceded, ict_index, value, minutes,
               PPMil, PPMin, Efficiency, everything()) %>% group_by(position) %>%
        arrange(match(position, c("GK", "DEF", "MID", "FWD")))
    dataFrame = as.data.frame(dataFrame)
    
    # teams matrix
    M = sapply(dataFrame$team_name, FUN = function(x, y){return(x == unique(y))}, y = dataFrame$team_name)
    rownames(M) = unique(dataFrame$team_name)
    colnames(M) = NULL
    
    # coefficients and directions
    c = dataFrame[, metric]
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

# check constraints for auto-subs: change nothing
checkConstraints = function(p){
    # p = [DEF, FWD, MID]'
    q = c()
    
    # violate positions
    if(length(p) < 4){ return(FALSE) }
    
    # DEF
    q[1] = ifelse(p[1] > 2 && p[1] < 6, TRUE, FALSE)
    
    # MID
    q[3] = ifelse(p[3] > 2 && p[3] < 6, TRUE, FALSE)
    
    # FWD
    q[2] = ifelse(p[2] > 0 && p[2] < 4, TRUE, FALSE)
    
    return( sum(q) == length(q) )
}

# metrics to use
metrics = c("PPMil", "PPMin", "total_points", "ict_index", "Efficiency")

for (m in 1:1) {
    # for loop to get for all seasons
    for(i in 3:3){
        # get stats from last season and rename columns
        cumulStats = summaryStats[[i]]; colnames(cumulStats) = index0
        
        # remove columns
        cumulStats = cumulStats[, index[-match(c("GW", "round", "selected", "opp_team_name", "was_home"), index)]]
        
        # get stats to update
        quantStats = colnames(cumulStats)[-c(2, 3, 5)]
        
        # filter to match next season gw_1 squad
        dataSet = data0[[i + 1]]
        
        for(j in 1:n){
            # gameweek j data
            gw = dataSet %>% filter(GW == j) %>% select(name, position, team_name, total_points, bps, goals_scored,
                                                        goals_conceded, ict_index, value, minutes, everything()) %>%
                mutate(value = value) %>% group_by(position) %>%  arrange(name)
            
            # select XV players based on metric x of cum stats (adjust for upcoming gameweek)
            if(j == 1){
                # players in prev season found in current gw1 (stays)
                ind2 = which(is.na(match(cumulStats$name, gw$name)) == F)
                
                # take updated gameweek data frame
                eligible = cumulStats[ind2, ]
                
                # players in current gw1 found in previous season
                ind1 = which(is.na(match(gw$name, eligible$name)) == F)
                
                # updated gameweek data
                final = gw[ind1, ]
                final[, "PPMil"] = eligible$total_points / (final$value)
                final[, "PPMin"] = ifelse(eligible$minutes > 0, eligible$total_points / (eligible$minutes), 0)
                final[, "Efficiency"] = ifelse(eligible$minutes > 0, eligible$total_points /
                                              sqrt((eligible$minutes / 90) * final$value), 0)
                
                selection = MILPXV(final, metric = metrics[m])
                d = selection$data
                s = selection$solutions
                D = data.frame(t(as.matrix(sapply(s, ties, dataFrame = d))))
                D$squad = 1:nrow(D)
                D = as.data.frame(D %>% arrange(desc(bps), desc(goals_scored), goals_conceded, desc(ict_index)))
                
                # store solution and corresponding indices
                X = s[[D[1, "squad"]]]; Y = which(X == 1, T)
                bestXV[[j]] = d[Y, ]
                STATS[[j]] = gw
                
            } else{ # get squad from previous gw
                # update data and update stats
                currentPlayers = rbind(startingXI, subs)$name
                
                # account for double gameweeks
                gw = gw %>% group_by(name, position, team_name) %>%
                    summarise(total_points = sum(total_points), minutes = sum(minutes), value = mean(value),
                              goals_scored = sum(goals_scored),
                              assists = sum(assists), clean_sheets = sum(clean_sheets),
                              goals_conceded = sum(goals_conceded), own_goals = sum(own_goals),
                              penalties_saved = sum(penalties_saved),
                              penalties_missed = sum(penalties_missed), yellow_cards = sum(yellow_cards),
                              red_cards = sum(red_cards), saves = sum(saves), bonus = sum(bonus), bps = sum(bps),
                              influence = sum(influence), creativity = sum(creativity), threat = sum(threat),
                              ict_index = sum(ict_index))
                
                # players in prev gw_i+1 found in current gw_i (stays)
                ind1 = which(is.na(match(gw$name, currentPlayers)) == F)
                ind2 = which(is.na(match(cumulStats$name, currentPlayers)) == F)
                
                final = as.data.frame(gw[ind1, ])
                
                # make transfers for BGW or if players in current team leave during the season (squad overhaul)
                ###### this is where transfers are needed
                if(nrow(final) < 15){
                    # players in prev season found in current gw1 (stays)
                    ind1 = which(is.na(match(cumulStats$name, gw$name)) == F)
                    
                    # take updated gw data frame
                    eligible = cumulStats[ind1, ]
                    
                    # players in current gw1 found in prev season
                    ind2 = which(is.na(match(gw$name, eligible$name)) == F)
                    
                    # updated gw
                    final = gw[ind2, ]
                    final[, "PPMil"] = eligible$total_points / (final$value)
                    final[, "PPMin"] = ifelse(eligible$minutes > 0, eligible$total_points / (eligible$minutes), 0)
                    final[, "Efficiency"] = ifelse(eligible$minutes > 0, eligible$total_points /
                                                  sqrt((eligible$minutes / 90) * final$value), 0)
                    
                    selection = MILPXV(final, metric = metrics[m])
                    
                    d = selection$data
                    s = selection$solutions
                    D = data.frame(t(as.matrix(sapply(s, ties, dataFrame = d))))
                    D$squad = 1:nrow(D)
                    D = arrange(.data = D, desc(bps), desc(goals_scored), goals_conceded, desc(ict_index))
                    
                    # store solution and corresponding indices
                    X = s[[D[1, "squad"]]]; Y = which(X == 1, T)
                    bestXV[[j]] = d[Y, ]
                } else{
                    final[, "PPMil"] = cumulStats[ind2, ]$total_points / (final$value)
                    final[, "PPMin"] = ifelse(cumulStats[ind2, ]$minutes > 0,
                                              cumulStats[ind2, ]$total_points / (cumulStats[ind2, ]$minutes), 0)
                    final[, "Efficiency"] = ifelse(cumulStats[ind2, ]$minutes > 0, cumulStats[ind2, ]$total_points /
                                                  sqrt((cumulStats[ind2, ]$minutes / 90) * final$value), 0)
                    bestXV[[j]] = as.data.frame(final %>% arrange(match(position, c("GK", "DEF", "MID", "FWD"))))
                }
                
                # update cum stats and store in list (dont replace)
                STATS[[j]] = rbind(cumulStats[, quantStats], gw[, quantStats]) %>% group_by(name) %>%
                    summarise(total_points = sum(total_points), minutes = sum(minutes), goals_scored = sum(goals_scored),
                              assists = sum(assists), clean_sheets = sum(clean_sheets),
                              goals_conceded = sum(goals_conceded), own_goals = sum(own_goals),
                              penalties_saved = sum(penalties_saved),
                              penalties_missed = sum(penalties_missed), yellow_cards = sum(yellow_cards),
                              red_cards = sum(red_cards), saves = sum(saves), bonus = sum(bonus), bps = sum(bps),
                              influence = sum(influence), creativity = sum(creativity), threat = sum(threat),
                              ict_index = sum(ict_index))
                colnames(cumulStats) = index0[-c(20:22)]
            }
            
            # select starting XI and captains
            bestXI = MILPXI(bestXV[[j]], metric = metrics[m])
            startingXI = as.data.frame(bestXI$data[which(bestXI$solutions[[1]] == 1, T), ])
            subs = as.data.frame(bestXI$data[which(bestXI$solutions[[1]] == 0, T), ])
            outfield = subs[-1, ]
            subs = as.data.frame(rbind(subs[1, ], outfield[order(outfield[, which(colnames(outfield) == metrics[m])],
                                                                 decreasing = T), ]))
            
            # double captain points (max points) + VC
            caps = order(startingXI[, metrics[m]], decreasing = T)[1:2]
            startingXI$captain = subs$captain = 0
            startingXI$captain[caps] = c(2, 1)
            
            # captain points
            startingXI$total_points[caps[1]] = 2 * startingXI$total_points[caps[1]]
            
            # auto-subs
            subout = which(startingXI$minutes == 0, T)
            
            # GK subout
            if(subout[1] == 1 && length(subout) > 0){
                temp = startingXI[1, ]
                startingXI[1, ] = subs[1, ]
                subs[1, ] = temp
                subout = subout[-1]
            }
            
            # outfield subs
            if(length(subout) > 0){
                for (k in 1:length(subout)) {
                    h = subout[k]
                    
                    for (l in 2:4) { # sub #h out for #l
                        if(subs[l, "minutes"] > 0){
                            temp = startingXI[h, ]
                            startingXI[h, ] = subs[l, ]
                            subs[l, ] = temp
                            
                            # check constraints are satisified
                            positions = table(startingXI$position)[-3]
                            
                            if( checkConstraints(positions) != TRUE){
                                temp = startingXI[h, ]
                                startingXI[h, ] = subs[l, ]
                                subs[l, ] = temp
                            }
                        }
                    }
                }
                
                startingXI = as.data.frame(startingXI %>% arrange(match(position, c("GK", "DEF", "MID", "FWD"))))
            }
            
            # check if captain is in line up and account for VC
            caps0 = which(startingXI$captain > 0, T)
            
            # account for vice captain points
            if(length(caps0) == 1){
                if(startingXI$captain[caps0] == 1){# vice captain if captain did not play
                    startingXI$total_points[caps0] = 2 * startingXI$total_points[caps0]
                }
            }
            
            # model records
            records[[j]] = list(startingXI = startingXI, subs = subs)
            
            # cumulatieve player stats
            cumulStats = STATS[[j]]
        }
        
        full[[i]] = records
    }
    
    KPI[[m]] = full
}

save.image(file = "Model.RData")

manager = c()

path = "~/Statistics/UCT/STA4007W/Project/Project/src/R/manager/"

for (j in 1:1) {
    fn = paste(path, "/past", j, ".txt", sep = "")
    df = read.csv(fn, header = T)
    manager = rbind(manager, df)
}
