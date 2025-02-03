ff <- Agrimonia[,c("IDStation", "NameStation", "Altitude",
                   "Latitude", "Longitude")]

ff2 <- ff[!duplicated(Agrimonia$IDStation),]

write.csv2(ff2, file="Stazioni.csv", row.names =F)

Agrimonia <- Agrimonia[,-c(3,4,5)]
write.csv2(Agrimonia2, file="Agrimonia.csv", row.names =F)

save(Agrimonia, file="Agrimonia2.RData")
