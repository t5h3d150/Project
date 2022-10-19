library(openxlsx)

data0 = c()

for (i in 1:6) {
    data0[[i]] = read.xlsx("Project_Data_v5.xlsx", sheet = i)
}

saveRDS(data0, file = "Project_Data.rds")
