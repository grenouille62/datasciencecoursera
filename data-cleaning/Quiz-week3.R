#Q1
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", method = "curl", destfile = "getdata_data_ss06hid.csv")
housing <- read.csv(file = "getdata_data_ss06hid.csv", header = TRUE)
agricultureLogical <- !is.na(housing[,"AGS"]) & housing[,"AGS"] == 6 & !is.na(housing[,"ACR"]) & housing[,"ACR"] == 3 & housing[,"ST"] == '16' > which(agricultureLogical)

#Q2
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", method = "curl", destfile = "jeff.jpg")
install.packages("jpeg")
library(jpeg)
jeffJPG <- readJPEG(source = "jeff.jpg", native = TRUE)
quantile(jeffJPG, seq(0,1, by = 0.1)) [c(4,9)]

#Q3
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", method = "curl", destfile = "FGDP.csv")
fgdp <- read.csv(file = "FGDP.csv", header = TRUE)

download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", method = "curl", destfile = "FEDSTAT.csv")
fedstat <- read.csv(file = "FEDSTAT.csv", header = TRUE)

install.packages("DBI")
library(DBI)
install.packages("dplyr")
library(dplyr)
library(stringr)
mergeCountryCode <- merge(x = fgdp, y = fedstat, by.x = "X", by.y = "CountryCode", all=FALSE)
mergeCountryCode <- mutate(mergeCountryCode, GDP = as.numeric(str_pad(Gross.domestic.product.2012, width = 3, pad = "0")))
mergeCountryCode <- filter(mergeCountryCode, GDP > 0)
mergeCountryCode <- arrange(mergeCountryCode, desc(GDP))
print (mergeCountryCode[13, "Short.Name"])

#Q4
print(c(mean(filter(mergeCountryCode, Income.Group == "High income: OECD") [,"GDP"]),
  mean(filter(mergeCountryCode, Income.Group == "High income: nonOECD") [,"GDP"])))

#Q5
print(nrow(filter(mergeCountryCode, GDP <= 38 & Income.Group == "Lower middle income")))