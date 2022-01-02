##########################################
######## Wytrykowska, Niedzielska ########
##########################################

getwd()


### Wczytanie danych

hmeq <- read.csv("~/Desktop/studia/magisterka/3sem/Modelowanie ryzyka kredytowego/Credit_risk_pro/CR_pro/hmeq.csv")

View(hmeq)

table(is.na(hmeq))


dir() # Lists files in the working directory


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

dane <- as_tibble(hmeq)

summary(hmeq) # Basic descripite stats.

# SPotting duplicates
table(duplicated(df_f))
# brak duplikatow w danych 


df_f <- as.data.frame(unclass(hmeq), stringsAsFactors = TRUE)
df_f[df_f$REASON == "", "REASON"] <- NA
df_f[df_f$JOB == "", "JOB"] <- NA


str(df_f)
#division variables into FACTORS and NUMERIC variables
summary(df_f)

nums <- sapply(df_f, is.numeric) 
#apply() applying function {is.numeric} to all elements of an object {base}
df.n<-df_f[,nums]

fact <- sapply(df_f, is.factor)
df.f<-df_f[,fact]

#HANDLING NAs


#NA's :
# MORTDUE: 518 ok
# VALUE: 112 ok
# YOJ: 515 ok
# DBEROG: 708 wartosci to liczby naturalne 
# DELINQ: 580 wartosci to liczby naturalne  
# CLAGE: 308 ok
# NINQ: 510 - wartosci to liczby naturalne  
# CLNO: 222 - wartosci to liczby naturalne 
# DEBTINC: 1267
# REASON: 252
# JOB: 279


# pierwszy wybor- mediana 
df.n$MORTDUE[is.na(df.n$MORTDUE)] <- median(df.n$MORTDUE, na.rm = TRUE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2063   46276   65019   73761   91488  399550     518 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2063   48139   65019   73001   88200  399550

df.n$VALUE[is.na(df.n$VALUE)] <- median(df.n$VALUE, na.rm = TRUE)
df.n$YOJ[is.na(df.n$YOJ)] <- median(df.n$YOJ, na.rm = TRUE)
df.n$CLAGE[is.na(df.n$CLAGE)] <- median(df.n$CLAGE, na.rm = TRUE)
df.n$DEBTINC[is.na(df.n$DEBTINC)] <- median(df.n$DEBTINC, na.rm = TRUE)

#handling na -  wartosci to liczby naturalne 
df.n$DEROG[is.na(df.n$DEROG)] <- median(df.n$DEROG, na.rm = TRUE)
df.n$DELINQ[is.na(df.n$DELINQ)] <- median(df.n$DELINQ, na.rm = TRUE)
df.n$NINQ[is.na(df.n$NINQ)] <- median(df.n$NINQ, na.rm = TRUE)
df.n$CLNO[is.na(df.n$CLNO)] <- median(df.n$CLNO, na.rm = TRUE)

### Handling na in factor variables 
head(df.f)

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}



df.f[["JOB_encoded"]] <- encode_ordinal(df.f[["JOB"]])
df.f[["city_encoded"]] <- encode_ordinal(df.f[["REASON"]])
head(df.f)


table(df.f[["JOB"]],encode_ordinal(df.f[["JOB"]]), useNA = "ifany")





#install.packages("moments")

#Statystyki opisowe wszytskich zmiennych 
library(DescTools)

Desc(mortgage)




# dobrze to wyglada, ale wartoby się jeszcze spytac:
# ??? jakie są kryteria wyboru: rodzaju imputacji, dyskretyzacji, lub zamodelowania zmiennnych NA ???







