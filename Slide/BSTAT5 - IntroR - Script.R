
setwd("C:/Users/ascari/Desktop/GitHub/B.STAT.5/Dati Agrimonia")
load("LussanaPNRR - Agrimonia Giornaliero.RData")

dati <- 
  (Agrimonia[!is.na(Agrimonia$AQ_pm10) & !is.na(Agrimonia$AQ_pm25),])




# Manipolazione dati

str(dati)
summary(dati)

table(dati$Year)
table(dati$NameStation)

nrow(dati)
ncol(dati)

dim(dati)
# Aggiungo una variabile:
dati$AQ_pm10_log <- log(dati$AQ_pm10)

dim(dati)


# Concentriamoci su solo due centraline:

centralina1 = "Milano Via Senato"
centralina2 = "Moggio Loc Penscei"
#centralina3 = "Lecco Via Sora"


centr1 = subset(dati, NameStation == centralina1,
                select = c(NameStation, Time, Year, AQ_pm10, AQ_pm25, AQ_pm10_log))
centr2 = subset(dati, NameStation == centralina2,
                select = c(NameStation, Time, Year, AQ_pm10, AQ_pm25, AQ_pm10_log))
#centr3 = subset(dati, NameStation == centralina3,
#                select = c(NameStation, Time, Year, AQ_pm10, AQ_pm25))


mean(centr1$AQ_pm10)
mean(centr2$AQ_pm10)

median(centr1$AQ_pm10)
median(centr2$AQ_pm10)

# Quartili
quantile(centr1$AQ_pm10, probs = c(.25, .5, .75))
quantile(centr2$AQ_pm10, probs = c(.25, .5, .75))

# Decili
seq(0.1, 0.9, by = 0.1)
quantile(centr1$AQ_pm10, probs = seq(0.1, 0.9, by = 0.1))
quantile(centr2$AQ_pm10, probs = seq(0.1, 0.9, by = 0.1))



range(centr1$AQ_pm10)
range(centr2$AQ_pm10)

max(centr1$AQ_pm10) - min(centr1$AQ_pm10)
max(centr2$AQ_pm10) - min(centr2$AQ_pm10)

IQR(centr1$AQ_pm10)
IQR(centr2$AQ_pm10)


sd(centr1$AQ_pm10)
sd(centr2$AQ_pm10)

var(centr1$AQ_pm10)
var(centr2$AQ_pm10)




hist(centr1$AQ_pm10, main = "Milano - Concentrazione PM10")
hist(centr2$AQ_pm10, main = "Moggio - Concentrazione PM10")

# ... commento sul fatto che i dati sono log-normali.....
# .................

hist(centr1$AQ_pm10_log, main = "Milano - Concentrazione log(PM10)")
curve(dnorm(x), xlim=c(-4,4), main = "Distribuzione Normale (o Gaussiana)")



boxplot(centr1$AQ_pm10, main = centralina1, ylim=c(0,150))
abline(h = 50, col = "red", lwd = 2)

boxplot(centr2$AQ_pm10, main = centralina2)
abline(h = 50, col = "red", lwd = 2)


par(mfrow=c(1,2))
boxplot(centr1$AQ_pm10, main = centralina1, ylim=c(0,150))
boxplot(centr2$AQ_pm10, main = centralina2, ylim=c(0,150))
par(mfrow=c(1,1))



par(mfrow=c(1,2))
plot(centr1$Time, centr1$AQ_pm10, type = "l")
abline(h = 50, col = "red", lwd = 2)
plot(centr2$Time, centr2$AQ_pm10, type = "l")
abline(h = 50, col = "red", lwd = 2)
par(mfrow=c(1,1))




# Quanti giorni sono caratterizzati da un livello medio di PM10 
# superiore alla soglia?

centr1$AQ_pm10 > 50
sum(centr1$AQ_pm10 > 50)

sum(centr2$AQ_pm10 > 50)


plot(centr1$Time, centr1$AQ_pm10, type = "l")
lines(centr2$Time, centr2$AQ_pm10, col = "red")
#lines(centr3$Time, centr3$AQ_pm10, col = "blue")


as.Date(0)
abline(v = as.numeric(as.Date(c("2017-01-01", "2018-01-01",
                      "2019-01-01", "2020-01-01", "2021-01-01",
                      "2022-01-01"))), col="blue", lty="dashed")


boxplot(dati$AQ_pm10 ~ dati$ARPA_zone)
boxplot(dati$AQ_pm10 ~ dati$ARPA_stat_type)


# Filtriamo per anno:
centr1_2021 = subset(centr1, Year == 2021)
centr2_2021 = subset(centr2, Year == 2021)
#centr3_2021 = subset(centr3, Year == 2021)

plot(centr1_2021$Time, centr1_2021$AQ_pm10, type = "l")
lines(centr2_2021$Time, centr2_2021$AQ_pm10, col = "red")
#lines(centr3_2021$Time, centr3_2021$AQ_pm10, col = "blue")



# Relazione tra variabili
plot(centr1$AQ_pm10, centr1$AQ_pm25, pch = 20)


dati_2021 <- subset(dati, Year == 2021)
dati_2021$IDStation = as.factor(dati_2021$IDStation)

aggregate(dati_2021, list(Station = IDStation), mean)

