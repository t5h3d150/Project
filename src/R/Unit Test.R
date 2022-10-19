library(openxlsx)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
options(error = recover)
library(lpSolveAPI)

# simulation
# parameters: transfer cap, number of gameweeks before incoproating new players, gamweeks to play chips

# load data
set.seed(2022)
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
    
    print(sols)
    
    return( list(model = model, data = dataFrame, solutions = sols) )
}

# check constraints for auto-subs: change nothing
checkConstraints = function(p){
    # p = [DEF, FWD, MID]'
    q = c()
    
    # violate positions
    if(length(p) < 3){ return(FALSE) }
    
    # DEF
    q[1] = ifelse(p[1] > 2 && p[1] < 6, TRUE, FALSE)
    
    # MID
    q[3] = ifelse(p[3] > 2 && p[3] < 6, TRUE, FALSE)
    
    # FWD
    q[2] = ifelse(p[2] > 0 && p[2] < 4, TRUE, FALSE)
    
    return( sum(q) == length(q) )
}

# assign chips to be unique
chips = function(){
    total = 1:38
    v = sample(x = total, size = 2)
    gamechips = v
    v = sample(x = total[-c(1, gamechips)], size = 1)
    gamechips = c(gamechips, v)
    v = sample(x = total[-c(1, 20:38, gamechips)], size = 1)
    gamechips = c(gamechips, v)
    v = sample(x = total[-c(1:19, gamechips)], size = 1)
    gamechips = c(gamechips, v)
    
    return(gamechips)
}

# metrics to use
metrics = c("PPMil", "PPMin", "total_points", "bps", "ict_index", "Efficiency")

# transfer cap
transferCap = 1

# transfer type: when it reaches 1 or 2
type = 1

# gameweeks to play chips: sample by replacement
# BB, TC, FH, WC < 19, WC > 18
gamechips = chips()

# freehit
freehit = F

# points multiplier
multiplier = 2

# large constant M
M = 1e6

t = system.time(expr = {
    for (m in 1:1) {
        for(i in 3:3){
            # get stats from last season and rename columns
            cumulStats = summaryStats[[i]]; colnames(cumulStats) = index0
            
            # remove columns
            cumulStats = cumulStats[, index[-match(c("GW", "round", "selected", "opp_team_name", "was_home"), index)]]
            
            # get stats to update
            quantStats = colnames(cumulStats)[-c(2, 3, 5)]
            
            # filter to match next season gw_1 squad
            dataSet = data0[[i + 1]]
            
            # init free transfer
            freeTransfers = 0
            
            for(j in 1:n){
                # init penalty
                penalty = 0
                
                # gameweek j data
                gw = dataSet %>% filter(GW == j) %>% select(name, position, team_name, total_points, bps, goals_scored,
                                                            goals_conceded, ict_index, value, minutes, everything()) %>%
                    mutate(value = value) %>% group_by(position) %>%  arrange(name)
                gw = as.data.frame(gw)
                
                # init gamweek statistics
                statistics = matrix(NA, nrow = 1, ncol = 8)
                colnames(statistics) = c("Points", "Transfers", "Penalty", "TC", "FH", "BB", "WC1", "WC2")
                rownames(statistics) = "Value"
                
                # select XV players based on metric x of cum stats (adjust for upcoming gameweek)
                if(j == 1){
                    # players in prev season found in current gw1
                    ind2 = which(is.na(match(cumulStats$name, gw$name)) == F)
                    
                    # take updated gameweek data frame
                    eligible = cumulStats[ind2, ]
                    
                    # players in current gw1 found in previous season
                    ind1 = which(is.na(match(gw$name, eligible$name)) == F)
                    
                    # updated gameweek data
                    final = gw[ind1, ]
                    final[, "PPMil"] = eligible$total_points / (final$value)
                    final[, "PPMin"] = ifelse(eligible$minutes > 0, eligible$total_points / (eligible$minutes / 90), 0)
                    final[, "Efficiency"] = ifelse(eligible$minutes > 0, eligible$total_points /
                                                       sqrt((eligible$minutes / 90) * final$value), 0)
                    
                    # select squad on updated data
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
                    # update free transfer
                    freeTransfers = freeTransfers + 1
                    
                } else{ # get squad from previous gw
                    # initialize free transfers and cap them at 2
                    freeTransfers = ifelse(freeTransfers + 1 < 3, freeTransfers + 1, 2)
                    
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
                    gw = as.data.frame(gw)
                    
                    # players in current gw found in previous squad (stays)
                    ind1 = which(is.na(match(gw$name, currentPlayers)) == F)
                    ind2 = which(is.na(match(cumulStats$name, currentPlayers)) == F)
                    
                    # draft squad
                    draft = as.data.frame(gw[ind1, ])
                    
                    # freehit + wild card chip
                    if( !is.na(match(j, gamechips[-c(1:2)])) ){
                        # update if freehit
                        freehit = ifelse( j == gamechips[3], T, F)
                        statistics["WC1"] = ifelse(j < 20, 1, 0)
                        statistics["WC2"] = ifelse(j > 19, 1, 0)
                        
                        # players in prev season found in current gw1 (stays)
                        ind1 = which(is.na(match(cumulStats$name, gw$name)) == F)
                        
                        # take updated gw data frame
                        eligible = cumulStats[ind1, ]
                        
                        # players in current gw1 found in prev season
                        ind2 = which(is.na(match(gw$name, eligible$name)) == F)
                        
                        # updated gw
                        final = gw[ind2, ]
                        final[, "PPMil"] = eligible$total_points / (final$value)
                        final[, "PPMin"] = ifelse(eligible$minutes > 0, eligible$total_points / (eligible$minutes / 90), 0)
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
                    }
                    
                    # make transfers for BGW or if players in current team leave during the season (missing players)
                    # emergency transfers
                    else if(nrow(draft) < 15){
                        # penalty incurred
                        penalty = (15 - nrow(final) - freeTransfers) * -4
                        
                        # transfer logic
                        # players in prev season found in current gw1 (stays)
                        ind1 = which(is.na(match(cumulStats$name, gw$name)) == F)
                        
                        # take updated gw data frame
                        eligible = cumulStats[ind1, ]
                        
                        # players in current gameweek found in previous gameweek
                        ind2 = which(is.na(match(gw$name, eligible$name)) == F)
                        
                        # updated gameweek data
                        final = gw[ind2, ]
                        final[, "PPMil"] = eligible$total_points / (final$value)
                        final[, "PPMin"] = ifelse(eligible$minutes > 0, eligible$total_points / (eligible$minutes / 90), 0)
                        final[, "Efficiency"] = ifelse(eligible$minutes > 0, eligible$total_points /
                                                           sqrt((eligible$minutes / 90) * final$value), 0)
                        
                        # players in previous gameweek squad found in current gameweek
                        ind3 = which(is.na(match(final$name, currentPlayers)) == F)
                        
                        # have data frame to keep players in current squad
                        final0 = final
                        final0[ind3, metrics] = final[ind3, metrics] + M
                        
                        # select squad
                        selection = MILPXV(final0, metric = metrics[m])
                        
                        final = final %>% select(name, position, team_name, total_points, bps, goals_scored,
                                                 goals_conceded, ict_index, value, minutes, PPMil, PPMin, Efficiency,
                                                 everything()) %>% group_by(position) %>%
                            arrange(match(position, c("GK", "DEF", "MID", "FWD")))
                        final = as.data.frame(final)
                        
                        selection$data = as.data.frame(final)
                        d = selection$data
                        s = selection$solutions
                        D = data.frame(t(as.matrix(sapply(s, ties, dataFrame = d))))
                        D$squad = 1:nrow(D)
                        D = arrange(.data = D, desc(bps), desc(goals_scored), goals_conceded, desc(ict_index))
                        
                        # store solution and corresponding indices
                        X = s[[D[1, "squad"]]]; Y = which(X == 1, T)
                        bestXV[[j]] = d[Y, ]
                        
                        # update free transfers
                        freeTransfers = 0
                        
                    } else{ # utilize free transfer
                        # logic for free transfers: lowest metric value, include transfer cap
                        
                        if(type == 1){ # if 1 free transfer
                            # penalty incurred
                            penalty = (transferCap - freeTransfers) * -4
                            
                            # update
                            freeTransfers = 0
                            statistics["Transfers"] = transferCap - freeTransfers
                            
                        } else{ # 2 free transfers
                            if(freeTransfers == 2){ # make changes
                                # penalty incurred
                                penalty = (transferCap - freeTransfers) * -4
                                
                                # update
                                freeTransfers = 0
                                statistics["Transfers"] = transferCap - freeTransfers
                            } else{# no changes
                                freeTransfers = freeTransfers + 1
                                statistics["Transfers"] = 0
                            }
                        }
                        
                        # transfer logic
                        # players in previous gameweek found in current gameweek (stays)
                        ind1 = which(is.na(match(cumulStats$name, gw$name)) == F)
                        
                        # take updated gameweek data frame
                        eligible = cumulStats[ind1, ]
                        
                        # players in current gameweek found in prev gameweek
                        ind2 = which(is.na(match(gw$name, eligible$name)) == F)
                        
                        # updated gameweek
                        final = gw[ind2, ]
                        final[, "PPMil"] = eligible$total_points / (final$value)
                        #cat(j, "Here", "\n", sep = " ")
                        final[, "PPMin"] = ifelse(eligible$minutes > 0, eligible$total_points / (eligible$minutes / 90), 0)
                        #cat(j, "Now Here", "\n", sep = " ")
                        final[, "Efficiency"] = ifelse(eligible$minutes > 0, eligible$total_points /
                                                           sqrt((eligible$minutes / 90) * final$value), 0)
                        
                        # players in previous squad found in current gameweek
                        ind3 = which(is.na(match(final$name, draft$name)) == F)
                        
                        # remove players and update squad
                        draft = final[ind3, ]
                        draft = draft[order(draft[, metrics[m]], decreasing = F), ][-c(1:transferCap), ]
                        
                        # players in previous squad found in current gameweek
                        ind3 = which(is.na(match(final$name, draft$name)) == F)
                        
                        # have data frame to keep players in current squad
                        final0 = final
                        final0[ind3, metrics] = final[ind3, metrics] + M
                        
                        # select squad
                        selection = MILPXV(final0, metric = metrics[m])
                        
                        final = final %>% select(name, position, team_name, total_points, bps, goals_scored,
                                                 goals_conceded, ict_index, value, minutes,PPMil, PPMin, Efficiency,
                                                 everything()) %>% group_by(position) %>%
                            arrange(match(position, c("GK", "DEF", "MID", "FWD")))
                        final = as.data.frame(final)
                        
                        selection$data = as.data.frame(final)
                        d = selection$data
                        s = selection$solutions
                        D = data.frame(t(as.matrix(sapply(s, ties, dataFrame = d))))
                        D$squad = 1:nrow(D)
                        D = arrange(.data = D, desc(bps), desc(goals_scored), goals_conceded, desc(ict_index))
                        
                        # store solution and corresponding indices
                        X = s[[D[1, "squad"]]]; Y = which(X == 1, T)
                        bestXV[[j]] = d[Y, ]
                    }
                    
                    # update cum stats and store in list
                    STATS[[j]] = rbind(cumulStats[, quantStats], gw[, quantStats]) %>% group_by(name) %>%
                        summarise(total_points = sum(total_points), minutes = sum(minutes), goals_scored = sum(goals_scored),
                                  assists = sum(assists), clean_sheets = sum(clean_sheets),
                                  goals_conceded = sum(goals_conceded), own_goals = sum(own_goals),
                                  penalties_saved = sum(penalties_saved),
                                  penalties_missed = sum(penalties_missed), yellow_cards = sum(yellow_cards),
                                  red_cards = sum(red_cards), saves = sum(saves), bonus = sum(bonus), bps = sum(bps),
                                  influence = sum(influence), creativity = sum(creativity), threat = sum(threat),
                                  ict_index = sum(ict_index))
                    STATS[[j]] = as.data.frame(STATS[[j]])
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
                multiplier = ifelse(j == gamechips[2], 3, 2)
                startingXI$total_points[caps[1]] = multiplier * startingXI$total_points[caps[1]]
                
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
                        
                        for (l in 2:4) { # sub h out for l
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
                    if(startingXI$captain[caps0] == 1){ # vice captain if captain did not play
                        startingXI$total_points[caps0] = multiplier * startingXI$total_points[caps0]
                    }
                }
                
                # triple captain
                statistics["TC"] = ifelse(multiplier == 3, 1, 0)
                
                # update statistics
                statistics["Penalty"] = penalty
                
                # points
                statistics["Points"] = sum(startingXI$total_points)
                if(j == gamechips[1]){
                    statistics["BB"] = 1
                    statistics["Points"] = statistics["Points"] + sum(subs$total_points)
                }
                
                
                # cumulative player stats
                cumulStats = STATS[[j]]
                
                # reset multiplier
                multiplier = 2
                
                # revert back to previous gameweek squad if freehit chip used
                if(freehit == T){
                    startingXI = records[[j-1]]$startingXI
                    subs = records[[j-1]]$subs
                    statistics["FH"] = 1
                    freehit = F
                }
                
                # store model team records
                records[[j]] = list(startingXI = startingXI, subs = subs, stats = statistics)
            }
            
            # store per season
            full[[i]] = records
        }
        
        # store per KPI
        KPI[[m]] = full
    }
})

cat(t[3] / 60, "minutes elapsed.\n", sep = " ")

save.image(file = "Model.RData")
