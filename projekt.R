##########################################
######## Wytrykowska, Niedzielska ########
##########################################


### Wczytanie danych

mortgage <- read.csv("~/Desktop/studia/magisterka/3sem/Modelowanie ryzyka kredytowego/mortgage.csv")

View(mortgage)

dir() # Lists files in the working directory

getwd()

packages_vector <- c(
  "lmtest", "zoo", "lattice", "pROC", "forcats", "RColorBrewer", "devtools", "smbinning", 
  "sqldf", "ggplot2", "scales", "Formula", "partykit", "plyr", "dplyr", "caTools", "tidyr", "gridExtra", "pcaPP", "ggrepel","readr", "woeBinning", "caTools", "magrittr")

package.check <- lapply(packages_vector, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

# Setting graphs areas
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

par(resetPar()) 
par(xpd = T, mfrow=c(1,1))

mortgage <- as_tibble(mortgage)

summary(mortgage) # Basic descripite stats.

#install.packages("moments")
library(moments)
skewness(mortgage$balance_time) #tutaj wstawić zmienną objaśnianą
kurtosis(mortgage$balance_time)
hist(mortgage$balance_time, breaks="Scott")

#Statystyki opisowe wszytskich zmiennych 
library(DescTools)

Desc(mortgage)

# Wartości NA

table(is.na(mortgage))
colSums(is.na(mortgage))

# mamy 270 wartosci NA w kolumnie LTV_time 
## co z nimi zrobic ???








