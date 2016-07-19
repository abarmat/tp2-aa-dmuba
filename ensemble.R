# Randomizer
set.seed(100)


# Read file
#data.text.bayes <- read.csv('bayes-pred-test.csv', header=FALSE, sep=";")
data.text.svm <- read.csv('svm-pred-test.csv', header=FALSE, sep=";")
data.text.bayes <- read.csv('randomforest-pred-test.csv', header=TRUE, sep=";")
data.boost <- read.csv('xgboost-pred-test.csv', header=FALSE, sep=";")
data.boost1 <- read.csv('xgboost-pred-test-1.csv', header=FALSE, sep=";")
data.boost2 <- read.csv('xgboost-pred-test-2.csv', header=FALSE, sep=";")
data.boost3 <- read.csv('xgboost-pred-test-3.csv', header=FALSE, sep=";")
data.boost4 <- read.csv('xgboost-pred-test-4.csv', header=FALSE, sep=";")
data.boost5 <- read.csv('xgboost-pred-test-5.csv', header=FALSE, sep=";")


#data.text.rf <- read.csv('TestRF123-45.csv', header=TRUE, sep=";")
#data.text.rf3 <- read.csv('TestRF3.csv', header=TRUE, sep=";")
#data.text.rf1 <- read.csv('TestRF1.csv', header=TRUE, sep=";")

ensamble<- data.frame(V1=integer())

votos<- data.frame(Clase1=integer(),
                   Clase2=integer(),
                   Clase3=integer(),
                   Clase4=integer(),
                   Clase5=integer())


numrows<-nrow(data.boost) 
indice<-1
while (indice <=numrows) {
  
  #    if (data.boost1[indice,1]==1 ){
  #          ensamble[indice,1]=1
  #      }else if (data.boost3[indice,1]==3){
  #         ensamble[indice,1]=3
  #       }else if (data.boost4[indice,1]==4){
  #         ensamble[indice,1]=4
  #       }else if (data.boost5[indice,1]==5){
  #         ensamble[indice,1]=5
  # #       }else if (data.boost2[indice,1]==2){
  # #         ensamble[indice,1]=2
  #       }else{
  
  #############################################        
  ClaseBin=ProbBin=Prob1=Prob2=Prob3=Prob4=Prob5=0
  votos[1,1]=0
  votos[1,2]=0
  votos[1,3]=0
  votos[1,4]=0
  votos[1,5]=0
  
  if (data.boost1[indice,1] > 0){       
    Prob1=data.boost1[indice,2]
  }
  if (data.boost2[indice,1] > 0){       
    Prob2=data.boost2[indice,2]
  }
  if (data.boost3[indice,1] > 0){       
    Prob3=data.boost3[indice,2]
  }
  if (data.boost4[indice,1] > 0){       
    Prob4=data.boost4[indice,2]
  }
  if (data.boost5[indice,1] > 0){       
    Prob5=data.boost5[indice,2]
  }
  
  
  maxProb=max(c(Prob1,Prob2,Prob3,Prob4,Prob5))
  if (maxProb > 0){
    if(maxProb==Prob1){
      #       ensamble[indice,1]=data.boost1[indice,1]
      votos[1,1]= 1.2
      ClaseBin=data.boost1[indice,1]
    }
    if(maxProb==Prob2){
      #       ensamble[indice,1]=data.boost2[indice,1]
      votos[1,2]= 1.2
      ClaseBin=data.boost2[indice,1]
    }
    if(maxProb==Prob3){
      #       ensamble[indice,1]=data.boost3[indice,1]
      votos[1,3]= 1.2
      ClaseBin=data.boost3[indice,1]
    }
    if(maxProb==Prob4){
      #       ensamble[indice,1]=data.boost4[indice,1]
      votos[1,4]= 1.2
      ClaseBin=data.boost4[indice,1]
    }
    if(maxProb==Prob5){
      #       ensamble[indice,1]=data.boost5[indice,1]
      votos[1,5]= 1.2
      ClaseBin=data.boost5[indice,1]
    }
    ProbBin= maxProb
    
  }
  #   }else{
  
  ################################################         
  #  votos[1,1]=0
  #  votos[1,2]=0
  #  votos[1,3]=0
  #  votos[1,4]=0
  #  votos[1,5]=0
  
  votos[1,data.boost[indice,1]]<-votos[1,data.boost[indice,1]] + 1
  
  
  votos[1,data.text.svm[indice,1]]<-votos[1,data.text.svm[indice,1]]+ 1     
  
  
  votos[1,data.text.bayes[indice,1]]<-votos[1,data.text.bayes[indice,1]] + 1  
  
  if (max(votos[1,])>1){
    columna=1
    while (votos[1,columna]< max(votos[1,])){
      columna = columna + 1
    }
    ensamble[indice,1]=columna
    
  }else{
    columna=1
    columnas<- data.frame(numero=integer())
    Prob1=Prob2=Prob3=0
    while (columna<=5){
      if (votos[1,columna]== max(votos[1,])){
        columnas=rbind(columnas, columna)
      }
      columna= columna + 1
    }
    
    
    if (data.text.svm[indice,1] %in% t(columnas)){
      Prob2=max(data.text.svm[indice, c(2,3,4,5)])
    }
    
    if (data.text.bayes[indice,1] %in% t(columnas)){
      Prob3=max(data.text.bayes[indice, c(2,3,4,5,6)])
    }
    
    if (data.boost[indice,1] %in% t(columnas)){
      Prob1=max(data.boost[indice, c(2,3,4,5,6)])
    }
    
    maxProb=max(c(Prob1,Prob2,Prob3, ProbBin))
    
    if(maxProb==Prob1){
      ensamble[indice,1]=data.boost[indice,1]
    }
    if(maxProb==Prob2){
      ensamble[indice,1]=data.text.svm[indice,1]
    }
    if(maxProb==Prob3){
      ensamble[indice,1]=data.text.bayes[indice,1]
    }
    if(maxProb==ProbBin){
      ensamble[indice,1]=ClaseBin
    }
    
    
  }
  #    }
  indice = indice + 1
}

data.test <- read.csv('tp2-work.test-4.csv', header=TRUE, sep=";")

sum(ensamble$V1 == data.test$Clase)/length(data.test$Clase)

#
# conf <- as.matrix(table(ensamble$V1, data.test$Clase))
# colnames(conf, c("1", "2", "3", "4", "5"))
# rownames(conf, c("1", "2", "3", "4", "5"))
# g <- ggplot(as.data.frame(conf))
# g + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + labs(fill="Normalized\nFrequency")

