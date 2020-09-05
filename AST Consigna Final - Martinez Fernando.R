############# EXAMEN FINAL - ANÁLISIS DE SERIES TEMPORALES #############
# Alumno: Fernando Martínez

# CArgo las librerías
  library(tseries)
  library(forecast)
  library(ggplot2)
  library(gridExtra)
  library(car)
  library(nortest)
  library(AdequacyModel)
  library(lmtest)
  library(quantmod)
  library(dygraphs)
  library(lessR)
  library(PASWR2)
  library(dplyr)
  library(psych)
  library(pastecs)
  library(astsa)
  library(tseries)
  library(zoo)
  library(xts)
  library(fma)
  library(expsmooth)
  library(Quandl)
  library(fpp)
  library(urca)
  library(AER)
  library(fUnitRoots)
  library(CADFtest)
  library(fpp2)
  library(datasets)
  library(tidyverse)
  library(magrittr)
  library(ggfortify)
  library(gamlss.data)
  library(vars)
  library(urca)
  library(lmtest)
  library(forecast)
  library(ggplot2)
  library(reshape2)
  library(ggfortify)
  library(readxl)
  library(psych)
  library(DataExplorer)
  library(timetk)
  library(keras)
  library(dplyr)

# Limpio la memoria
rm( list=ls() )
gc()

# Cargo el Dataset. El mismo corresponde al precio mensual en USD de distintos commodities (oro, plata y petróleo) desde 1980 a 2020
# Link Dataset: https://www.imf.org/en/Research/commodity-prices
base <- read_xlsx(file.choose())
head(base)
tail(base)

# Hago un primer análisis exploratorio de la base
psych::describe(base)
plot_histogram(base) 

# Planteo las series temporales para los 3 commodities (oro, plata y petróleo)
tsg <- ts(base[,2], start= 1980, freq = 12)   # Serie de tiempo del oro
tss <- ts(base[,3], start= 1980, freq = 12)   # Serie de tiempo de la plata
tso <- ts(base[,4], start= 1980, freq = 12)  # Serie de tiempo del petróleo

# Grafico las series de tiempo
autoplot(tsg, ts.colour = 'dark blue')+ ggtitle("Precio Oro") + ylab("")
autoplot(tss, ts.colour = 'dark green')+ ggtitle("Precio Plata") + ylab("")
autoplot(tso, ts.colour = 'dark red')+ ggtitle("Precio Petróleo") + ylab("")

# Grafico conjuntamente todas las series de tiempo
ts.plot(tsg, tss, tso, col = c("dark blue", "dark green", "dark red"), main = "Evolución del Precio de los Commodities")

# Grafico FAS, FAC y FACP de las distintas series
acf(tsg,type = "covariance",plot = T)
ggAcf(tsg) + ggtitle("FAC Oro") # La serie decrece linealmente, lo que indica que no es estacionaria
ggAcf(tsg,type = "partial") + ggtitle("FACP Oro") 

acf(tss,type = "covariance",plot = T)
ggAcf(tss) + ggtitle("FAC Plata") # La serie decrece linealmente, lo que indica que no es estacionaria
ggAcf(tss,type = "partial") + ggtitle("FACP Plata")

acf(tso,type = "covariance",plot = T)
ggAcf(tso) + ggtitle("FAC Petróleo") # La serie decrece linealmente, lo que indica que no es estacionaria
ggAcf(tso,type = "partial") + ggtitle("FACP Petróleo")

# Planteo el test de Ljung-Box. Si rechazo H0 significa que hay coeficientes de autocorrelación distintos a cero
Incorrelation(tsg,"Ljung-Box") # Ver función autocorrelación al final del script
inco_wn = Incorrelation(tsg,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags
# El p-value se ubica en 0 por lo que rechazo H0 y puedo considerar que es una serie no estacionaria 

Incorrelation(tss,"Ljung-Box")
inco_wn = Incorrelation(tss,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags
# El p-value se ubica en 0 por lo que rechazo H0 y puedo considerar que es una serie no estacionaria 

Incorrelation(tso,"Ljung-Box")
inco_wn = Incorrelation(tso,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags
# El p-value se ubica en 0 por lo que rechazo H0 y puedo considerar que es una serie no estacionaria 

# Para probar la presencia de raíces unitarias, planteo el test de Dickey-Fuller
summary(ur.df(tsg, type = "drift", selectlags = c("AIC")))
summary(ur.df(tsg, type = "trend", selectlags = c("AIC")))
summary(ur.df(tsg, type = "none", selectlags = c("AIC")))
# El estadístico tau1, tau2 y tau3 son mayores a los valores críticos del estadístico por lo que no se puede rechazar H0
# Los estadísticos phi2 y phi3 no son significativos. 

summary(ur.df(tss, type = "drift", selectlags = c("AIC")))
summary(ur.df(tss, type = "trend", selectlags = c("AIC")))
summary(ur.df(tss, type = "none", selectlags = c("AIC")))
# Según el intervalo de confianza elegido se puede rechazar o no H0
# Los estadísticos phi2 y phi3 no son significativos. 

summary(ur.df(tso, type = "drift", selectlags = c("AIC")))
summary(ur.df(tso, type = "trend", selectlags = c("AIC")))
summary(ur.df(tso, type = "none", selectlags = c("AIC")))
# El estadístico tau1, tau2 y tau3 son mayores a los valores críticos del estadístico por lo que no se puede rechazar H0
# Los estadísticos phi2 y phi3 no son significativos. 

# Calculo la cantidad de diferencias para poder hacer la series estacionarias
ndiffs(tsg) # Dos diferencias
ndiffs(tss) # Una diferencia
ndiffs(tso) # Una diferencia

# Pruebo hacer la primer diferencia de cada serie
dtsg <- diff(tsg)
dtss <- diff(tss)
dtso <- diff(tso)

# Grafico las series diferenciadas
autoplot(dtsg, ts.colour = 'dark blue')+ ggtitle("Precio Oro", subtitle = "Primera Diferencia") + ylab("")
autoplot(dtss, ts.colour = 'dark green')+ ggtitle("Precio Plata", subtitle = "Primera Diferencia") + ylab("")
autoplot(dtso, ts.colour = 'dark red')+ ggtitle("Precio Petróleo", subtitle = "Primera Diferencia") + ylab("")

# Grafico conjuntamente todas las series de tiempo diferenciadas
ts.plot(dtsg, dtss, dtso, col = c("dark blue", "dark green", "dark red"), main = "Evolución del Precio Diferenciado de los Commodities")

# Grafico FAS, FAC y FACP de las distintas series diferenciadas
acf(dtsg,type = "covariance",plot = T)
ggAcf(dtsg) + ggtitle("FAC 1ra dif Oro") # La serie decrece exponencialmente, lo que indica estacionariedad
ggAcf(dtsg,type = "partial") + ggtitle("FACP 1ra dif Oro") 

acf(dtss,type = "covariance",plot = T)
ggAcf(dtss) + ggtitle("FAC 1ra dif Plata") # La serie decrece exponencialmente, lo que indica estacionariedad
ggAcf(dtss,type = "partial") + ggtitle("FACP1ra dif Plata")

acf(dtso,type = "covariance",plot = T)
ggAcf(dtso) + ggtitle("FAC 1ra dif Petróleo") # La serie decrece exponencialmente, lo que indica estacionariedad
ggAcf(dtso,type = "partial") + ggtitle("FACP 1ra dif Petróleo")

# Planteo el test de Ljung-Box. Si rechazo H0 significa que hay coeficientes de autocorrelación distintos a cero
Incorrelation(dtsg,"Ljung-Box") 
inco_wn = Incorrelation(dtsg,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags
# El p-value se ubica en 0 por lo que rechazo H0 y puedo considerar que es una serie no estacionaria 

Incorrelation(dtss,"Ljung-Box")
inco_wn = Incorrelation(dtss,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags
# El p-value se ubica en 0 por lo que rechazo H0 y puedo considerar que es una serie no estacionaria 

Incorrelation(dtso,"Ljung-Box")
inco_wn = Incorrelation(dtso,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags
# El p-value se ubica en 0 por lo que rechazo H0 y puedo considerar que es una serie no estacionaria 

# Para probar la presencia de raíces unitarias, planteo el test de Dickey-Fuller
summary(ur.df(dtsg, type = "drift", selectlags = c("AIC")))
summary(ur.df(dtsg, type = "trend", selectlags = c("AIC")))
summary(ur.df(dtsg, type = "none", selectlags = c("AIC")))
# El estadístico tau1, tau2 y tau3 son menores a los valores críticos del estadístico por lo que puedo rechazar H0
# El módulo de los estadísticos phi2 y phi3 no son mayores en módulo al valor crítico. No son significativos. 

summary(ur.df(dtss, type = "drift", selectlags = c("AIC")))
summary(ur.df(dtss, type = "trend", selectlags = c("AIC")))
summary(ur.df(dtss, type = "none", selectlags = c("AIC")))
# El estadístico tau1, tau2 y tau3 son menores a los valores críticos del estadístico por lo que puedo rechazar H0
# El módulo de los estadísticos phi2 y phi3 no son mayores en módulo al valor crítico. No son significativos. 

summary(ur.df(dtso, type = "drift", selectlags = c("AIC")))
summary(ur.df(dtso, type = "trend", selectlags = c("AIC")))
summary(ur.df(dtso, type = "none", selectlags = c("AIC")))
# El estadístico tau1, tau2 y tau3 son menores a los valores críticos del estadístico por lo que puedo rechazar H0
# El módulo de los estadísticos phi2 y phi3 no son mayores en módulo al valor crítico. No son significativos. 

# Creo una base con las 3 series diferenciadas
df <- data.frame(dtsg,dtss,dtso)

# Defino la cantidad de lags convenientes para el VAR
VARselect(df, lag.max = 15) # Elijo un modelo de 10 lags

# Planteo el VAR
VAR1 <- VAR(df, p = 10)

# Analizo la causalidad de granger para el caso de tres variables (entre cada variable y pares de variable)
causality(VAR1, cause = "oro") # Hay causalidad en el sentido de granger
causality(VAR1, cause = "plata") # Hay causalidad en el sentido de granger
causality(VAR1, cause = "petroleo") # Hay causalidad en el sentido de granger

causality(VAR1, cause = c("oro", "plata")) # Hay causalidad en el sentido de granger
causality(VAR1, cause = c("oro", "petroleo")) # Hay causalidad en el sentido de granger
causality(VAR1, cause = c("plata", "petroleo")) # Hay causalidad en el sentido de granger

# Analizo la causalidad de granger para cada variable por separado
grangertest(dtsg ~ dtss, order = 10) # Hay causalidad en el sentido de granger
grangertest(dtss ~ dtsg, order = 10) # Hay causalidad en el sentido de granger

grangertest(dtsg ~ dtso, order = 10) # Hay causalidad en el sentido de granger
grangertest(dtso ~ dtsg, order = 10) # Hay causalidad en el sentido de granger

grangertest(dtso ~ dtss, order = 10) # No hay causalidad en el sentido de granger
grangertest(dtss ~ dtso, order = 10) # Hay causalidad en el sentido de granger

# Planteo las condiciones de estabilidad del VAR
summary(VAR1)

# Ploteo los resiudos del VAR
x11()
plot(VAR1)

# Planteo la prueba de autocorrelación para los residuos
seriala <- serial.test(x = VAR1, lags.pt = 10, type="PT.asymptotic")
seriala$serial  
# p-value menor a 0.05. Rechazo H0 (incorrelación de los residuos)

#Para la prueba de normalidad.
normalidad <- normality.test(VAR1)
normalidad$jb.mul
# Rechazo el supuesto de normalidad, asimetría y kurtosis de los resiudos. No estoy cumpliendo con el supuesto

# Verifico la homocedasticidad de los resiudos
arch1 <- arch.test(VAR1, lags.multi=10)
arch1$arch.mul 
# Rechazo la homocedasticidad, son heterocedásticos

# Analizo la función impulso respuesta
# Impulso respuesta del oro
VAR1_irfg <- irf(VAR1, response="oro", n.ahead=10, boot=TRUE)
VAR1_irfg 
# Grafico el impulso respuesta
plot(VAR1_irfg)

# Impulso respuesta de la plata
VAR1_irfs <- irf(VAR1, response="plata", n.ahead=10, boot=TRUE)
VAR1_irfs
# Grafico el impulso respuesta
plot(VAR1_irfs)

# Impulso respuesta del petróleo
VAR1_irfo <- irf(VAR1, response="petroleo", n.ahead=10, boot=TRUE)
VAR1_irfo
# Grafico el impulso respuesta
plot(VAR1_irfo)

# Analizamos la descomposición de las varianzas de las variables, 
fevd <- fevd(VAR1, n.ahead = 50)
x11()
plot(fevd)

# Para el oro
VAR1_fevd_dtsg<-fevd(VAR1, n.ahead=50)$oro
VAR1_fevd_dtsg 

# Para la plata
VAR1_fevd_dtss <- fevd(VAR1, n.ahead=50)$plata
VAR1_fevd_dtss

# Para la petróleo
VAR1_fevd_dtso <- fevd(VAR1, n.ahead=50)$petroleo
VAR1_fevd_dtso

######### Forecast: Queremos predecir el precio del oro de los últimos cuatro años 08-16 al 07-20

# Conjunto de entrenamiento:
# Tomo un 90% de la base original 
traing <- window(dtsg,start = c(1980,2), end = c(2016,7))
trains <- window(dtss,start = c(1980,2), end = c(2016,7))
traino <- window(dtso,start = c(1980,2), end = c(2016,7))

# Conjunto de test (48 observaciones - 4 años aprox)
# Tomo un 10% de la base original 
test <- window(dtsg,start = c(2016,8), end = c(2020,7))

# Ploteamos el conjunto de train dtsg y el conjunto de test
ts.plot(dtsg,test, gpars = list(col = c("black", "red")))

# Planteo el modelo VAR con el conjunto de entrenamiento
dft <- data.frame(traing,trains,traino)
VAR2 <- VAR(dft, p = 10)

# Realizo la predicción con el modelo VAR
fcast_oro <- predict(VAR2, n.ahead = 48, ci = 0.95)
plot(fcast_oro, names = "oro")

# Precisión modelo VAR
accuracy(VAR2$varresult[[1]])
res <- residuals(VAR2)
fits <- fitted(VAR2)

for(i in 1:4)
{
  fc <- structure(list(mean=fcast_oro$fcst[[i]][,"fcst"], x=dft[,i],
                       fitted=c(NA,NA,fits[,i])),class="forecast")
  print(accuracy(fc,test[,i]))
}

########## Planteamos un modelo ingenuo con estacionalidad ########## 

# Planteo un modelo ingenuo con estacionalidad
mie <- snaive(traing,h = 48)
summary(mie)
autoplot(mie)

# Ploteamos el conjunto de test con las prediciones
ts.plot(test,mie$mean , gpars = list(col = c("black", "red")))

# Ploteamos la serie original y las predicciones
ts.plot(mie$x,mie$fitted, gpars = list(col = c("black", "red")))

# Verificamos los residuos
residuos1 <- resid(mie)

# Verifico la normalidad de los residuos
Normality_Test(na.omit(residuos1),type = "JB") 
Normality_Test(na.omit(residuos1),type = "AD")
Normality_Test(na.omit(residuos1),type = "SW") 
# Con lo tres test rechazo el supuesto de normalidad

# Planteo el test de Ljung-Box. Si rechazo H0 significa que hay coeficientes de autocorrelación distintos a cero
Incorrelation(residuos1,"Ljung-Box")
inco_wn = Incorrelation(residuos1,"Ljung-Box")

# Grafico los p-value para disntitos lags
autoplot(ts(inco_wn$P_Value)) +
  ggtitle("Test de Ljung-Box", subtitle = "P-Value") +
  ylab("")
# todos los p-value son menores a 0.05. Rechazo el supuesto de incorrelación (independencia)

# Chequeamos los residuos del modelo
checkresiduals(mie)

# Verifico la precisión
accuracy(mie, test)

########## Analizamos un modelo de redes neuronales NNAR ########## 

# Planteo y entreno la red
nn1 = nnetar(y = traing, p = 10, P = 0, size = 2) # p = 10 períodos
print(nn1)

# Chequeo los residuos
checkresiduals(nn1)

# Verificamos los residuos
residuos2 <- resid(nn1)

# Verifico la normalidad de los residuos
Normality_Test(na.omit(residuos2),type = "JB") 
Normality_Test(na.omit(residuos2),type = "AD")
Normality_Test(na.omit(residuos2),type = "SW") 
# Con lo tres test rechazo el supuesto de normalidad

# Forecasting para 4 años
fc1 = forecast(nn1,h=48)
fc1.PI = forecast(nn1,h=48, PI = T)
autoplot(fc1.PI) + 
  xlab("Year") + 
  ylab("# of Lynx Trapped") + 
  ggtitle("Ann. Lynx Trappings with 10-year Forecast")

# Veo los intervalos
fc1.PI

# Ploteo el conjunto de test junto con la predicción
ts.plot(test,fc1$mean , gpars = list(col = c("black", "red")))

# Ploteo el conjunto de train junto con la predicción
ts.plot(traing,fc1$fitted , gpars = list(col = c("black", "red")))

# Evaluo la precisión
accuracy(fc1,test)

########## Analizamos un modelo de redes neuronales LSTM ########## 

serie <- as.data.frame(traing)
scaled_train <- (serie$oro - mean(serie$oro))/sd(serie$oro) # Escalo mediante la media y el sd
scaled_train <- as.matrix(scaled_train) # Defino una matriz

# Defino el horizaonte de predicción
prediction <- 48
lag <- prediction

# Armo una matriz con 47 columnas de valores rezagados
x_train_data <- t(sapply(
  1:(length(scaled_train) - lag - prediction + 1),
  function(x) scaled_train[x:(x + lag - 1), 1]
))
dim(x_train_data)

# Lo transformo a un formato array
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(nrow(x_train_data),lag,1)
)
dim(x_train_arr)

# Calculo los valores de Y
y_train_data <- t(sapply(
  (1 + lag):(length(scaled_train) - prediction + 1),
  function(x) scaled_train[x:(x + prediction - 1)]
))
dim(y_train_data)

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(nrow(y_train_data),prediction,1)
)
dim(y_train_arr)

# Preparo los datos pAra la predicción
x_test <- as.data.frame(test)
x_test_scaled <- (x_test$oro - mean(serie$oro))/sd(serie$oro) # Escalo mediante la media y el sd
x_test_scaled <- as.matrix(x_test_scaled) # Defino una matriz

# Lo llevo a una estructura de array
x_pred_arr <- array(
  data = x_test_scaled,
  dim = c(1,lag,1)
)

# Creo el modelo
lstm_model <- keras_model_sequential()
lstm_model %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 48, 1), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

# Compilo el modelo
lstm_model %>%
  compile(loss = 'mae', 
          optimizer = 'adam', 
          metrics = 'accuracy')

# Analizo la estructura del modelo
summary(lstm_model)

# Fiteo el modelo
lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 0,
  shuffle = TRUE #,
  # callbacks = callback_early_stopping(patience = 2, monitor = 'acc'),
  # validation_split = 0.3
)

# Predicciones del modelo
lstm_forecast <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# Reescalo las predicciones
lstm_forecast <- lstm_forecast * sd(serie$oro)+ mean(serie$oro)

# Predicción con los datos de train
fitted <- predict(lstm_model, x_train_arr, batch_size = 1) %>%
  .[, , 1]

# Transformo los datos para tener una predicción por cada fecha
if(dim(fitted)[2] > 1){
  fit <- c(fitted[, 1], fitted[dim(fitted)[1], 2:dim(fitted)[2]])
} else {
  fit <- fitted[, 1]
}

# Vuelvo a reescalar los datos
fitted <- fit * sd(serie$oro) + mean(serie$oro)
length(fitted)

# Especifico los primeros valores como NA
fitted <- c(rep(NA, lag), fitted)

# Configuro las predicciones como serie temporal
lstm_forecast <- tk_ts(lstm_forecast,
                       start = c(2016, 8),
                       end = c(2020, 7),
                       frequency = 12)

# Visualizo las predicciones
lstm_forecast

# Defino el input
input_ts <- traing

# Defino el objeto de pronóstico
forecast_list <- list(
  model = NULL,
  method = "LSTM",
  mean = lstm_forecast,
  x = input_ts,
  fitted = fitted,
  residuals = as.numeric(input_ts) - as.numeric(fitted)
)

class(forecast_list) <- "forecast"

# Grafico el objeto
plot(forecast_list)

# Grafico la serie original y la predicha
fitted2 <- ts(fitted, start= c(1980,2), freq = 12) 
ts.plot(traing,fitted2, gpars = list(col = c("black", "red")))

fitted3 <- ts(append(fitted2,lstm_forecast), start= c(1980,2), freq = 12)

# Ploteo el conjunto de test junto con la predicción
ts.plot(test,lstm_forecast, gpars = list(col = c("black", "red")))

# Evaluo la precisión para test
library(Metrics)
TEST <- as.data.frame(test)
PRED <- as.data.frame(as.matrix(lstm_forecast))
class(PRED$V1)
class(TEST$oro)

me <- sum((TEST$oro - PRED$V1))  / 48
print(me)
rmse <- rmse(TEST$oro,PRED$V1)
print(rmse)
mae <- mae(TEST$oro,PRED$V1)
print(mae)
mpe <- sum((TEST$oro - PRED$V1) / TEST$oro) * 100 / 48
print(mpe)
mape <- sum(abs(TEST$oro - PRED$V1 / TEST$oro)) * 100/48
print(mape)
mase <- mase(TEST$oro,PRED$V1)
print(mase)

# Evaluo la precisión para train
TRAIN <- tail(as.data.frame(traing),390) 
PREDT <- tail(as.data.frame(fitted),390) #  Saco los NA

me <- sum((TRAIN$oro - PREDT$fitted))  / 48
print(me)
rmse <- rmse(TRAIN$oro,PREDT$fitted)
print(rmse)
mae <- mae(TRAIN$oro,PREDT$fitted)
print(mae)
mpe <- sum((TRAIN$oro - PREDT$fitted) / TEST$oro) * 100 / 48
print(mpe)
mape <- sum(abs(TRAIN$oro - PRED$V1 / TEST$oro)) * 100/48
print(mape)
mase <- mase(TRAIN$oro,PREDT$fitted)
print(mase)

# Chequeo los residuos
checkresiduals(append(fitted2,lstm_forecast))

#---------------------------------------------- 0 ----------------------------------------------
# FUNCION INCORRELACION

# Cargo la siguiente función de incorrelación que realiza un test de Ljung-Box o Box-Pierce para distintos lags

Incorrelation <- function(ts, type = c("Ljung-Box","Box-Pierce"), fitdf = 0){
  p_ljung_box = NULL
  s_ljung_box = NULL
  for(i in 0:(length(ts)/4)){
    p_ljung_box[i] = Box.test(ts,lag = i,type = type,fitdf = fitdf)$p.value
    s_ljung_box[i] = Box.test(ts,lag = i,type = type,fitdf = fitdf)$statistic
  }
  table = data.frame(j = 1:(length(ts)/4),
                     P_Value = p_ljung_box,
                     Statistic = s_ljung_box)
  return(table)
}

#---------------------------------------------- 0 ----------------------------------------------
# FUNCION TEST DE NORMALIDAD
Normality_Test <- function(ts,type = c("JB", "AD", "SW")){
  require(tseries)
  require(nortest)
  if(type == "JB"){
    p_val = jarque.bera.test(ts)$p.value
    stat  = jarque.bera.test(ts)$statistic
  } else if(type == "AD"){
    p_val = ad.test(ts)$p.value
    stat  = ad.test(ts)$statistic
  } else {
    p_val = shapiro.test(ts)$p.value
    stat  = shapiro.test(ts)$statistic
  }
  
  table = data.frame(P_Value = p_val,
                     Statistic = stat)
  return(table)
}

