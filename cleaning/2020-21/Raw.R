library(openxlsx)
library(dplyr)
options(encoding = "UTF-8")

# possible change of fixture number in fixture list

positions = c("GK", "DEF", "MID", "FWD")
data1 = read.xlsx("2020-21 merged.xlsx", sheet = 1)
data2 = read.csv("master_team_list.csv")
data2 = data2[data2$season == "2020-21", ]
data3 = data.frame(opponent_team = data2$team, opp_team_name = data2$team_name)

data4 = merge(x = data1, y = data3, by = "opponent_team")
data4$season = rep("2020-21", nrow(data4))

data4 = data4 %>% select(season, name, position, GW, team, opp_team_name, team_h_score, team_a_score, everything()) %>% arrange(GW, team, opp_team_name, position)

# rename element type to position
write.xlsx(data4, "draft 2020-21.xlsx")
