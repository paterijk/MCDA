library(MCDA)

# load performance table csv file
# provided with the MCDA package

f <- system.file("datasets","performanceTable.csv",package="MCDA")

pT <- read.csv(file = f, header=TRUE, row.names=1)

# to filter out cars which do not
# respect Thierry's initial rules

fPT <- pT[(pT$g4>=2 & pT$g5>=2 & pT$g2 < 30), ]

# to drop car a14 from the table

fPT <- fPT[!(rownames(fPT) %in% "a14"), ]

print(fPT)

criteriaMinMax <- c("min","min","min","max","max")

names(criteriaMinMax) <- colnames(pT)

plotRadarPerformanceTable(fPT, criteriaMinMax, overlay=TRUE, bw=TRUE)

