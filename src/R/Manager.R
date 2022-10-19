manager = c()

path = "~/Statistics/UCT/STA4007W/Project/Project/src/R/manager/"

for (j in 1:1) {
    fn = paste(path, "/past", j, ".txt", sep = "")
    df = read.csv(fn, header = T)
    manager = rbind(manager, df)
}