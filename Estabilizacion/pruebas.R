
setwd("/Users/eduardomartinez/Documents/ITAM/semestre 1/Estadistica computacional/CompuStat/CompuStat/pruebas kuri")

Data <- read.csv(file="Original_cars.csv", header=TRUE, sep=",")

Numericos <- as.matrix(Data)

Numericos <- Numericos[,-22]

Estabilizados <- apply(Numericos, 1:2, function(x) as.numeric(x)+runif(1, min = 0, max = 0.5))

write.table(Estabilizados,file="prueba.txt",row.names=FALSE) # drops the rownames

summary(Data[Data$class == 'acc',])