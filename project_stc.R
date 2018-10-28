getwd()
setwd("STC")
dataset <- read.csv("rainfall_dataset.csv", stringsAsFactors = FALSE)
View(dataset)
unique(dataset$SUBDIVISION)

#EDA
summary(dataset)
#Remove rows containing na
# sum(is.na(dataset))
# nrow(dataset)
clean.dataset <- na.omit(dataset)
# sum(is.na(clean.dataset))
library(dplyr)
andaman.data <- subset(clean.dataset, clean.dataset$SUBDIVISION == "ANDAMAN & NICOBAR ISLANDS")
View(andaman.data)
nrow(andaman.data)
jai.maharashtra <- subset(clean.dataset, clean.dataset$SUBDIVISION == "MADHYA MAHARASHTRA")
View(jai.maharashtra)

monthly.rain <- c(0,0,0,0,0,0,0,0,0,0,0,0)
yearly.rain <- c(1:10)
color <- rainbow(10)
for(row.year in 1:100) {
  for (jai.row in 1:12) {
    monthly.rain[jai.row] <- jai.maharashtra[row.year * 10,c(2+jai.row)]
  }
  lines(monthly.rain, col = color[row.year], xaxt = "n")
}
axis(side = 1,at = 1:12, labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))

###########################################################################

jai.maharashtra <- subset(clean.dataset, clean.dataset$SUBDIVISION == "MADHYA MAHARASHTRA")
maha1915 <- subset(jai.maharashtra, jai.maharashtra$YEAR == "1915")
maha2015 <- subset(jai.maharashtra, jai.maharashtra$YEAR == "2015")
maha1915 <- maha1915[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
maha2015 <- maha2015[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
View(maha1915)
View(t(maha2015))
tmaha1915 <- t(maha1915)
tmaha1915 <- as.data.frame(tmaha1915)
months <- rownames(tmaha1915)

plot(x = rownames(tmaha1915), y = tmaha1915$`2637`,xlim = c(0,250))
barplot(tmaha1915$`2637`,names.arg = rownames(tmaha1915),ylim = c(0,250),col = rainbow(15),main = "1915 Rainfall of Maharashtra",
        xlab = "Months", ylab = "Rainfall")

tmaha2015 <- t(maha2015)
tmaha2015 <- as.data.frame(tmaha2015)
View(tmaha2015)
barplot(tmaha2015$`2737`,names.arg = rownames(tmaha1915),ylim = c(0,250),col = rainbow(15),main = "2015 Rainfall of Maharashtra",
        xlab = "Months", ylab = "Rainfall")

###########################################################################

bengal.data <- subset(dataset, dataset$SUBDIVISION == "GANGETIC WEST BENGAL")
bengal1915 <- subset(bengal.data, bengal.data$YEAR == "1915")
bengal2015 <- subset(bengal.data, bengal.data$YEAR == "2015")
bengal1915 <- bengal1915[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
bengal2015 <- bengal2015[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
tbengal2015 <- as.data.frame(t(bengal2015))
tbengal1915 <- as.data.frame(t(bengal1915))
barplot(tbengal1915$`2637` ,names.arg = months,ylim = c(0,750),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")
barplot(tbengal2015$`667` ,names.arg = months,ylim = c(0,700),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")

###########################################################################

tamilnadu.data <- subset(dataset, dataset$SUBDIVISION == "TAMIL NADU")
tamilnadu1915 <- subset(tamilnadu.data, tamilnadu.data$YEAR == "1915")
tamilnadu2015 <- subset(tamilnadu.data, tamilnadu.data$YEAR == "2015")
tamilnadu1915 <- tamilnadu1915[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
tamilnadu2015 <- tamilnadu2015[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
ttamilnadu2015 <- as.data.frame(t(tamilnadu2015))
ttamilnadu1915 <- as.data.frame(t(tamilnadu1915))
barplot(ttamilnadu1915$`3442` ,names.arg = months,ylim = c(0,400),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")
barplot(ttamilnadu2015$`3542` ,names.arg = months,ylim = c(0,400),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")

###########################################################################

punjab.data <- subset(dataset, dataset$SUBDIVISION == "PUNJAB")
punjab1915 <- subset(punjab.data, punjab.data$YEAR == "1915")
punjab2015 <- subset(punjab.data, punjab.data$YEAR == "2015")
punjab1915 <- punjab1915[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
punjab2015 <- punjab2015[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
tpunjab2015 <- as.data.frame(t(punjab2015))
tpunjab1915 <- as.data.frame(t(punjab1915))
View(tpunjab1915)
barplot(tpunjab1915$`1487`,names.arg = months,ylim = c(0,150),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")
barplot(tpunjab2015$`1587`,names.arg = months,ylim = c(0,150),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")

###########################################################################

andaman.data <- subset(dataset, dataset$SUBDIVISION == "ANDAMAN & NICOBAR ISLANDS")
andaman1915 <- subset(andaman.data, andaman.data$YEAR == "1915")
andaman2015 <- subset(andaman.data, andaman.data$YEAR == "2015")
andaman1915 <- andaman1915[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
andaman2015 <- andaman2015[,c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")]
tandaman2015 <- as.data.frame(t(andaman2015))
tandaman1915 <- as.data.frame(t(andaman1915))
barplot(tandaman1915$`14` ,names.arg = months,ylim = c(0,600),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")
barplot(tandaman2015$`110` ,names.arg = months,ylim = c(0,600),col = rainbow(15),
        xlab = "Months", ylab = "Rainfall")

###########################################################################

