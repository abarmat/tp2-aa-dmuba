library(xgboost)
library(stringr)


preprocess <- function(items) {
  # Select features
  attr_list = c('anio', 'mes', 'tipoprop', 'lugar', 'sup_tot_m2',
                'lat_lon', 'sup_cub_m2', 'piso', 'cant_amb', 'geoname_num',
                'Estrenar', 'Resumen', 'Cochera', 'Patio', 'Categoria', 'Balcon','Clase')
    
  df <- items[attr_list]
  
  # Convert mes to numeric (maintaning order)
  df$mes <- as.character(df$mes)
  df$mes[df$mes == 'ene'] = '01'
  df$mes[df$mes == 'feb'] = '02'
  df$mes[df$mes == 'mar'] = '03'
  df$mes[df$mes == 'abr'] = '04'
  df$mes[df$mes == 'may'] = '05'
  df$mes[df$mes == 'jun'] = '06'
  df$mes[df$mes == 'jul'] = '07'
  df$mes[df$mes == 'ago'] = '08'
  df$mes[df$mes == 'sep'] = '09'
  df$mes[df$mes == 'oct'] = '10'
  df$mes[df$mes == 'nov'] = '11'
  df$mes[df$mes == 'dic'] = '12'
  
  df$aniomes <- as.numeric(paste(as.character(df$anio), as.character(df$mes), sep=""))
  df$anio <- NULL
  df$mes <- NULL
  
  # Create new features (separating lat and lon)
  A <- str_split_fixed(df$lat_lon, ',', 2)
  df$lat <- A[,1]
  df$lon <- A[,2]
  df$lat_lon <- NULL
  
  # Convert to numeric
  df$lat <- as.numeric(df$lat)
  df$lon <- as.numeric(df$lon)
  df$cant_amb <- as.numeric(df$cant_amb)
  df$sup_tot_m2 <- as.numeric(df$sup_tot_m2)
  df$sup_cub_m2 <- as.numeric(df$sup_cub_m2)
  df$piso <- as.numeric(df$piso)
  df$Clase <- as.numeric(df$Clase)
  
  df$lugar <- NULL
  
  return(df)
}

# Randomizer
set.seed(100)

for(class_id in 0:4) {
  # Read file
  FILENAME <- 'tp2-work-train-final.csv'
  items <- read.csv(FILENAME, header=TRUE, sep=";")
  
  # Preprocessing
  df_all <- preprocess(items)
  
  # Split class
  y_train <- df_all['Clase']-1
  X_train <- subset(df_all, select=-c(Clase))
  
  # Convert to binary classification problem
  y_train$Clase[y_train$Clase!=class_id] = 9
  y_train$Clase[y_train$Clase==class_id] = 1
  y_train$Clase[y_train$Clase==9] = 0

  # Cross-validate
  num_classes = length(table(y_train))
  param <- list("objective" = "binary:logistic",
                "eval_metric" = "error",
                "nthread" = 4)
  
  # cv.nround <- 5
  # cv.nfold <- 10
  # bst.cv = xgb.cv(
  #   param=param, data=data.matrix(X_train), label=y_train[,1], 
  #   nfold=cv.nfold, nrounds=cv.nround, missing=NaN
  # )
  
  # Train
  nround = 500
  bst = xgboost(param=param, data=data.matrix(X_train), label=y_train[,1], 
                nrounds=nround, missing=NaN)
  
  # Feature importance
  # model <- xgb.dump(bst, with.stats = T)
  # names <- dimnames(data.matrix(X_train))[[2]]
  # importance_matrix <- xgb.importance(names, model=bst)
  # xgb.plot.importance(importance_matrix)
  # xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)
  
  ### Try on testing
  
  # Read file
  FILENAME <- 'tp2-clasificacion-V2-final.csv'
  items <- read.csv(FILENAME, header=TRUE, sep=";")
  
  # Preprocessing
  df_all <- preprocess(items)
  
  # Split class
  y_test <- df_all['Clase']-1
  X_test <- subset(df_all, select=-c(Clase))
  
  # Convert to binary classification problem
  y_test$Clase[y_test$Clase!=class_id] = 9
  y_test$Clase[y_test$Clase==class_id] = 1
  y_test$Clase[y_test$Clase==9] = 0
  
  # Predict
  results <- predict(bst, data.matrix(X_test), missing=NaN)
  probs <- results
  preds <- as.integer(results > 0.5)
  preds[preds==1] = class_id+1
  all <- cbind(preds, probs)
  
  # Accuracy
  print(sum(as.integer(preds == y_test$Clase))/length(preds))
    
  # Out
  write.table(all, file=sprintf("xgboost-pred-test-%s.csv", class_id+1), 
              row.names=FALSE, col.names=FALSE, sep=";")
}

