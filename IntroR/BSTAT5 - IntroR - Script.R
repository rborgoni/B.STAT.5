
setwd("C:/Users/ascari/Desktop/Github/B.STAT.5/Dati Agrimonia")
load("LussanaPNRR - Agrimonia Giornaliero.RData")

dati <- 
  (Agrimonia[!is.na(Agrimonia$AQ_pm10) & !is.na(Agrimonia$AQ_pm25),])



################################
###### Manipolazione dati ######
################################

str(dati)
summary(dati)

# Tabelle di frequenza: ci indicano quante volte appare
# ogni modalità di una specifica variabile:
table(dati$Year)
table(dati$NameStation)

# Quante righe ha la nostra matrice dei dati?
# Ossia, quante unita' statistiche stiamo considerando?
nrow(dati)
# Quante colonne ha la nostra matrice dei dati?
# Ossia, quante variabili stiamo considerando?
ncol(dati)

dim(dati)



# Possiamo creare e aggiungere una variabile al dataset:
dati$AQ_pm10_log <- log(dati$AQ_pm10)

dim(dati)




# Concentriamoci su due sole centraline:

centralina1 = "Milano Via Senato"
centralina2 = "Moggio Loc Penscei"

# Vogliamo creare due dataset: 
# - il primo deve contenere tutte e sole le unita' 
#   statistiche della prima centralina;
centr1 = subset(dati, 
                NameStation == centralina1,
                select = c(NameStation, Time, Year, AQ_pm10, 
                           AQ_pm25, AQ_pm10_log, 
                           WE_temp_2m, WE_wind_speed_10m_mean, 
                           WE_tot_precipitation))

# - il secondo deve contenere tutte e sole le unita' 
#   statistiche della seconda centralina;
centr2 = subset(dati, 
                NameStation == centralina2,
                select = c(NameStation, Time, Year, AQ_pm10, 
                           AQ_pm25, AQ_pm10_log, 
                           WE_temp_2m, WE_wind_speed_10m_mean,
                           WE_tot_precipitation))



###################################
####### Indici di posizione ####### 
###################################

# Media aritmetica:
mean(centr1$AQ_pm10)
mean(centr2$AQ_pm10)

# Mediana:
median(centr1$AQ_pm10)
median(centr2$AQ_pm10)

# Quartili:
quantile(centr1$AQ_pm10, probs = c(.25, .5, .75))
quantile(centr2$AQ_pm10, probs = c(.25, .5, .75))

# Decili:
seq(0.1, 0.9, by = 0.1)
quantile(centr1$AQ_pm10, probs = seq(0.1, 0.9, by = 0.1))
quantile(centr2$AQ_pm10, probs = seq(0.1, 0.9, by = 0.1))



######################################
####### Indici di variabilita' ####### 
######################################

range(centr1$AQ_pm10)
range(centr2$AQ_pm10)

# Range:
max(centr1$AQ_pm10) - min(centr1$AQ_pm10)
max(centr2$AQ_pm10) - min(centr2$AQ_pm10)

# Range inter-quartilico:
IQR(centr1$AQ_pm10)
IQR(centr2$AQ_pm10)

# Varianza:
var(centr1$AQ_pm10)
var(centr2$AQ_pm10)

# Deviazione Standard:
sd(centr1$AQ_pm10)
sd(centr2$AQ_pm10)



#########################################
####### Rappresentazioni grafiche #######
#########################################


# Istogramma:
hist(centr1$AQ_pm10, main = "Milano - Concentrazione PM10")
hist(centr2$AQ_pm10, main = "Moggio - Concentrazione PM10")

# ... commento sul fatto che i dati sono log-normali.....
# .................

hist(centr1$AQ_pm10_log, main = "Milano - Concentrazione log(PM10)")
curve(dnorm(x), xlim=c(-4,4), main = "Distribuzione Normale (o Gaussiana)")


# Boxplot:
boxplot(centr1$AQ_pm10, main = centralina1, ylim=c(0,150))
abline(h = 50, col = "red", lwd = 2)

# Il comando abline() permette di aggiungere, AD UN
# GRAFICO GIA' ESISTENTE, una retta
# orizzontale (h = ), verticale (v = ) o 
# generica y = a + bx (a =, b =) 

boxplot(centr2$AQ_pm10, main = centralina2)
abline(h = 50, col = "red", lwd = 2)



par(mfrow=c(1,2))
boxplot(centr1$AQ_pm10, main = centralina1, ylim=c(0,150))
boxplot(centr2$AQ_pm10, main = centralina2, ylim=c(0,150))
par(mfrow=c(1,1))


# Abbiamo quindi considerato due centraline che hanno
# rilevato andamenti molto differenti per il PM10.

# Come mai?

dati[which(dati$NameStation == centralina1), "ARPA_zone"]
dati[which(dati$NameStation == centralina2), "ARPA_zone"]



# Diagramma a dispersione
# Il diagramma a dispersione e' molto utile per analizzare come
# due variabili variano congiuntamente:

plot(centr1$AQ_pm10, centr1$AQ_pm25, pch = 20,
     ylab = "PM2.5", xlab = "PM10")
abline(a = 0, b = 1, col="darkgray", lty = "dashed")

cor(centr1$AQ_pm10, centr1$AQ_pm25)


retta = lm(centr1$AQ_pm25~ centr1$AQ_pm10)
retta

abline(retta, col = "red", lwd = 2)






plot(centr1$WE_wind_speed_10m_mean, centr1$AQ_pm25, pch = 20)
plot(centr1$WE_wind_speed_10m_mean, centr1$AQ_pm10, pch = 20)

plot(centr1$WE_tot_precipitation, centr1$AQ_pm25, pch = 20)
plot(centr1$WE_tot_precipitation, centr1$AQ_pm10, pch = 20)







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


# Serie storica del PM10 nelle due centraline:
plot(centr1$Time, centr1$AQ_pm10, type = "l",
     main = "Andamento PM10 nel tempo", ylab = "PM10", xlab = "Tempo")
lines(centr2$Time, centr2$AQ_pm10, col = "red")

# Aggiungiamo delle linee verticali in corrispondenza 
# dell'inizio di ogni anno:
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








dati_2021 <- subset(dati, Year == 2021)
dati_2021$IDStation = as.factor(dati_2021$IDStation)



aggregate(AQ_pm10 ~ IDStation, data = dati_2021, mean)




