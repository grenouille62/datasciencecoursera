# Read input data frame
x<-read.csv("hw1_data-1.txt", TRUE,",")
#Q 11
names(x)
#Q12
x[1:2,]
#Q13
nrow(x)
#Q14
x[(as.integer(nrow(x)) -1):as.integer(nrow(x)),]
#Q15
x[47, "Ozone"]
#Q16
nrow(x[is.na(x[,"Ozone"]),])
#Q17
z <- x[!is.na(x[,"Ozone"]),]
mean(z [,"Ozone"])
#Q18
ozone31 <- x[!is.na(x[,"Ozone"]) & x[,"Ozone"] > 31,]
ozone31Temp90 <- ozone31[ozone31[,"Temp"]>90, ]
mean(ozone31Temp90[,"Solar.R"])
#Q19
month6 <- x[x[,"Month"]==6,]
mean(month6[,"Temp"])
#Q20
month5 <- x[x[,"Month"]==5,]
max(month5 [!is.na(month5[,"Ozone"]), "Ozone"])

