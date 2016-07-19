if(!require (xlsx)){
  install.packages('xlsx')  
  require (xlsx)
}

if(!require (FSelector)){
  install.packages('FSelector')  
  require (FSelector)
}



if(!require (randomForest)){
  install.packages('randomForest')
  library(randomForest)
}
library(utils)
library(stringr)

library(caret)
library(Amelia)

loadFile()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cargaAux$lugar <- as.numeric(factor(cargaAux$lugar, levels=unique(cargaAux$lugar)))
cargaAux$tipoprop <- as.numeric(factor(cargaAux$tipoprop, levels=unique(cargaAux$tipoprop)))
cargaAux$aniomes <- as.numeric(paste(as.character(cargaAux$anio), as.character(cargaAux$mes), sep=""))
cargaAux$anio <- cargaAux$anio
cargaAux$mes <- NULL

noms = c()
ords = c()
idvars = c()
A <- cargaAux[c('sup_tot_m2', 'sup_cub_m2', 'tipoprop', 'lat' , 'lon', 'cant_amb', 'geoname_num')]
B <- amelia(A, noms=noms, ords=ords, idvars=idvars, m=2,
            bound=rbind(c(1, 0, Inf), c(2, 0, Inf), c(3, 0, Inf), c(4, -60, Inf), c(5, -60, Inf),c(6, 0, Inf),c(7, 0, Inf) ))
cargaAux$sup_tot_m2 <- B$imputations$imp1$sup_tot_m2
cargaAux$sup_cub_m2 <- B$imputations$imp1$sup_cub_m2
cargaAux$lat <- B$imputations$imp1$lat
cargaAux$lon <- B$imputations$imp1$lon
cargaAux$cant_amb <- B$imputations$imp1$cant_amb
cargaAux$geoname_num <- B$imputations$imp1$geoname_num


cargaAux.train= cargaAux[,c('geoname_num','Resumen','aniomes','cant_amb','lugar','sup_tot_m2', 'sup_cub_m2', 'tipoprop', 'lat' , 'lon','Categoria','Patio','Estrenar','Cochera','Balcon', 'Clase')]

carga<-read.csv("tp2-clasificacion-V2-final.csv", header= TRUE, sep=";")


cargaAux= carga[,c(2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,21,22,23,24,25,26,28,29,30,31,32)]
A <- str_split_fixed(cargaAux$lat_lon, ',', 2)
cargaAux$lat <- A[,1]
cargaAux$lon <- A[,2]
cargaAux$lat <- as.numeric( cargaAux$lat)
cargaAux$lon <- as.numeric( cargaAux$lon)
cargaAux$lat_lon <- NULL
#cargaAux$aniomes <- paste(as.character(cargaAux$mes), as.character(cargaAux$anio), sep="")
#cargaAux$anio <- cargaAux$anio
#cargaAux$mes <- NULL


cargaAux$Clase= as.factor(cargaAux$Clase)


cargaAux$lugar <- as.numeric(factor(cargaAux$lugar, levels=unique(cargaAux$lugar)))
cargaAux$tipoprop <- as.numeric(factor(cargaAux$tipoprop, levels=unique(cargaAux$tipoprop)))
#cargaAux$geoname_num <- as.numeric(factor(cargaAux$geoname_num, levels=unique(cargaAux$geoname_num)))
cargaAux$cant_amb <- as.numeric(factor(cargaAux$cant_amb, levels=unique(cargaAux$cant_amb)))

noms = c()
ords = c()
idvars = c()
A <- cargaAux[c('sup_tot_m2', 'sup_cub_m2', 'tipoprop', 'lat' , 'lon', 'cant_amb', 'geoname_num')]
B <- amelia(A, noms=noms, ords=ords, idvars=idvars, m=2,
            bound=rbind(c(1, 0, Inf), c(2, 0, Inf), c(3, 0, Inf), c(4, -60, Inf), c(5, -60, Inf),c(6, 0, Inf),c(7, 0, Inf) ))
cargaAux$sup_tot_m2 <- B$imputations$imp1$sup_tot_m2
cargaAux$sup_cub_m2 <- B$imputations$imp1$sup_cub_m2
cargaAux$lat <- B$imputations$imp1$lat
cargaAux$lon <- B$imputations$imp1$lon
cargaAux$cant_amb <- B$imputations$imp1$cant_amb
cargaAux$geoname_num <- B$imputations$imp1$geoname_num


cargaAux$mes <- as.character( cargaAux$mes)
cargaAux$mes[ cargaAux$mes == 'ene'] = '01'
cargaAux$mes[ cargaAux$mes == 'feb'] = '02'
cargaAux$mes[ cargaAux$mes == 'mar'] = '03'
cargaAux$mes[ cargaAux$mes == 'abr'] = '04'
cargaAux$mes[ cargaAux$mes == 'may'] = '05'
cargaAux$mes[ cargaAux$mes == 'jun'] = '06'
cargaAux$mes[ cargaAux$mes == 'jul'] = '07'
cargaAux$mes[ cargaAux$mes == 'ago'] = '08'
cargaAux$mes[ cargaAux$mes == 'sep'] = '09'
cargaAux$mes[ cargaAux$mes == 'oct'] = '10'
cargaAux$mes[ cargaAux$mes == 'nov'] = '11'
cargaAux$mes[ cargaAux$mes == 'dic'] = '12'
#cargaAux$mes <- as.numeric(cargaAux$mes)
cargaAux$aniomes <- as.numeric(paste(as.character(cargaAux$anio), as.character(cargaAux$mes), sep=""))
cargaAux$anio <- cargaAux$anio
cargaAux$mes <- NULL
cargaAux$Clase= as.factor(cargaAux$Clase)


cargaAux.test= cargaAux[,c('geoname_num','Resumen','aniomes','cant_amb','lugar','sup_tot_m2', 'sup_cub_m2', 'tipoprop', 'lat' , 'lon','Categoria','Patio','Estrenar','Cochera','Balcon', 'Clase')]

arboles<-1000
while (arboles <= 3000) {  
  
  print (c(  format(Sys.time(), "%d-%m-%Y_%H-%M-%OS") , "Ramdom Forest", arboles ))
  #
  # Algoritmo Ramdom Forest
  #  
  
  #tmp = as.vector(table(cargaAux.train$Clase))
  #num_classes = length(tmp)
  #min_size = tmp[order(tmp,decreasing=FALSE)[1]]
  #sampsizes = rep(min_size,num_classes)
  fit <- randomForest(as.factor(Clase) ~ geoname_num + tipoprop + sup_tot_m2 +  sup_cub_m2 + lat + lon + Cochera + Estrenar + aniomes + Resumen + Patio + Categoria + cant_amb,
                      #  fit <- randomForest(as.factor(Clase) ~ lugar+ tipoprop + sup_tot_m2 + sup_cub_m2 + Pozo + Fideicomizo + Oportunidad + Construccion + Amenities + Cochera + Baulera + Nuevo + Reciclar + Estrenar + lat + lon,
                      data=cargaAux.train, 
                      importance=TRUE, 
                      mtry=5,
                      ntree=2801,
                      Proximity=TRUE)
  #                    sampsize=sampsizes)
  varImpPlot(fit,
             sort = T,
             main="Variable Importance",
             n.var=8)
  Prediction <- predict(fit, cargaAux.test)
  Tabla.output= data.frame(Prediction, predict(fit, cargaAux.test,  type="prob"))
  result <- confusionMatrix(Prediction, cargaAux.test$Clase)
  precision <- as.data.frame(result$overall)[1,]
  write.table(Tabla.output, file = "randomforest-pred-final.csv", sep=";", row.names = FALSE)
  
  informe.RF = rbind(informe.RF, c(arboles, precision))
  arboles <- arboles + 250
}

Tabla.output= data.frame(attr(results, "probabilities"), results)
write.table(Tabla.output, file = "TestSVM.csv", sep=";", row.names = FALSE)

importance(fit)
varImpPlot(fit)
write.csv(informe.RF, file = "firstforest.csv", row.names = FALSE)



loadFile<- function(){
  # read dataset crea por default un data frame
  carga<-read.csv("tp2-work-train-final.csv", header= TRUE, sep=";")
  
  
  cargaAux= carga[,c(2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,21,22,23,24,25,26,28,29,30,31,32)]
  A <- str_split_fixed(cargaAux$lat_lon, ',', 2)
  cargaAux$lat <- A[,1]
  cargaAux$lon <- A[,2]
  cargaAux$lat <- as.numeric( cargaAux$lat)
  cargaAux$lon <- as.numeric( cargaAux$lon)
  cargaAux$lat_lon <- NULL
  
  
  cargaAux$mes <- as.character( cargaAux$mes)
  cargaAux$mes[ cargaAux$mes == 'ene'] = '01'
  cargaAux$mes[ cargaAux$mes == 'feb'] = '02'
  cargaAux$mes[ cargaAux$mes == 'mar'] = '03'
  cargaAux$mes[ cargaAux$mes == 'abr'] = '04'
  cargaAux$mes[ cargaAux$mes == 'may'] = '05'
  cargaAux$mes[ cargaAux$mes == 'jun'] = '06'
  cargaAux$mes[ cargaAux$mes == 'jul'] = '07'
  cargaAux$mes[ cargaAux$mes == 'ago'] = '08'
  cargaAux$mes[ cargaAux$mes == 'sep'] = '09'
  cargaAux$mes[ cargaAux$mes == 'oct'] = '10'
  cargaAux$mes[ cargaAux$mes == 'nov'] = '11'
  cargaAux$mes[ cargaAux$mes == 'dic'] = '12'
  #  cargaAux$mes <- as.numeric(cargaAux$mes)
  
  cargaAux$Clase= as.factor(cargaAux$Clase)
  
  
  cargaAux
}
