library(openxlsx)
dataSet = c()
v = w = c()

for (i in 1:6) {
    dataSet[[i]] = read.xlsx(xlsxFile = "Project Data.xlsx", sheet = i)
    v = c(v, dataSet[[i]]$name)
    w = c(w, dataSet[[i]]$season)
}

t = unique(data.frame(v, w))
t = t[order(t$v, decreasing = F), ]
write.xlsx(t, "Names.xlsx")
