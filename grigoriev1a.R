library(readxl)

Grigoriev <- read.csv("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.csv")

View(Grigoriev)

colnames(Grigoriev) <- c("Region","PISA_15","Literacy_97")
Grigoriev$PISA_15 <- as.numeric(Grigoriev$PISA_15)
Grigoriev$Literacy_97 <- as.numeric(Grigoriev$Literacy_97)
mean(Grigoriev$PISA_15)
save(Grigoriev,file="Grigoriev.rdata")
