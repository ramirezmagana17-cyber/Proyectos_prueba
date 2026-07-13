library(readxl)
DatosBaseProyecto <- read_excel("DatosBaseProyecto.xlsx")

library(tseries)
library(forecast)
library(lmtest)

library(nortest)
library(moments)
library(car)
library(dplyr)
library(tidyverse)
library(ggplot2)

library(readxl)
datospib <- read_excel("C:/Users/reyes/Downloads/DatosBaseProyecto.xlsx")
View(datospib)

#Gráfica PIB
ggplot(datospib, aes(x=Año, y=PIB))+
  geom_line(color="red4")+
  labs(title="Producto Interno Bruto en México, 1970-2022", y="PIB", x="Año")+
  theme_minimal()

#Gráfica IED
ggplot(datospib, aes(x=Año, y=IED))+
  geom_line(color="red4")+
  labs(title="Inversión Extranjera Directa en México, 1970-2022", y="IED", x="Año")+
  theme_minimal()

#Gráfica AIB
ggplot(datospib, aes(x=Año, y=IED))+
  geom_line(color="red4")+
  labs(title="Ahorro Interno Bruto en México, 1970-2022", y="AIB", x="Año")+
  theme_minimal()

#Reescalar datos, dividir entre 1,000. Previamente ya los había dividido entre 1,000 en el excel
datospib$pib <- datospib$PIB / 1000
datospib$aib <- datospib$AIB / 1000
datospib$ied <- datospib$IED / 1000

#Regresión log-log
#Cambios porcentuales
modelo_loglog <- lm(log(pib)~log(aib)+log(ied),datospib)
summary(modelo_loglog)
#Por cada 1% que aumenta la IED, el PIB aumenta 0.083%

#Regresión lin-lin
#Modelo en niveles.  
modelo_linlin<-lm(pib~aib+ied, datospib)
summary(modelo_linlin)
#Cambios absolutos

#Regresión lin-log
modelo_linlog <- lm(pib ~ log(aib)+log(ied),datospib)
summary(modelo_linlog)
#Por cada aumento porcentual del AIB el PIB aumentan en 109634

#Regresión log-lin
modelo_loglin <- lm(log(pib)~aib+ied, datospib)
summary(modelo_loglin)
#Por un aumento en mil millones del aib, el pib aumenta en 0.0004923%

#Gráficas de correlación
#PIB vs AIB
ggplot(datospib,aes(x=aib,y=pib))+
  geom_point(color="blue3",
             size=2)+
  geom_smooth(method="lm",formula=y~poly(x,2),se=FALSE,color="red4")+
  theme_minimal()+
  labs(title="PIB respecto del Ahorro\nInterno Bruto",
       x="AIB",
       y="PIB")+
  theme(plot.title = element_text(hjust = 0.5,size = 14))

#PIB vs IED
ggplot(datospib,aes(x=ied,y=pib))+
  geom_point(color="blue3",
             size=2)+
  geom_smooth(method="lm",formula=y~poly(x,2),se=FALSE,color="red4")+
  theme_minimal()+
  labs(title="PIB respecto de la Inversion\nExtranjera Directa",
       x="IED",
       y="PIB")+
  theme(plot.title = element_text(hjust = 0.5,size = 14))

#MODELO####

#Niveles####
modeloP1 <-lm (datospib$pib~datospib$aib+datospib$ied)
summary(modeloP1)
resiudal = modeloP1$residuals
# P < 0.05, el modelo es válido
#La R^2 es muy alto, lo que indica que cada una de las variables independientes
#son muy significativas para el modelo

#TASAS DE CRECIMIENTO####
#PIB
tcpa_PIB <-lm(log(pib)~Año, datospib)
summary(tcpa_PIB) #Desde 1970 el PIB ha venido creciendo en promedio 2.6% anual
#AIB
tcpa_AIB <-lm(log(aib)~Año, datospib)
summary(tcpa_AIB) #Desde 1970 el Ahorro Interno Bruto ha venido creciendo en promedio 6.4% anual
#IEB
tcpa_IED <-lm(log(ied)~Año, datospib)
summary(tcpa_IED) #Desde 1970 la IED ha venido creciendo en promedio 10.08% anual

library(dplyr)
library(forecast)
#Agregar las Tasas de Crecimiento a la base datos
datospib$tc_PIB = ((datospib$pib/lag(datospib$pib, n=1L, default=NA, order_by = NULL))-1)*100
datospib$tc_AIB = ((datospib$aib/lag(datospib$aib, n=1L, default=NA, order_by = NULL))-1)*100
datospib$tc_IED = ((datospib$ied/lag(datospib$ied, n=1L, default=NA, order_by = NULL))-1)*100
print(datospib$tc_PIB)

#Media artimética tasa de crecimiento del Pib de 1970-2022
TCPIB_2022 <- datospib[1:53,"tc_PIB"]
mean(TCPIB_2022$tc_PIB, na.rm = TRUE) #Fue de 2.95% Anual

#Gráfica de las tasas de crecimiento
#PIB
ggplot(datospib, aes(x=Año, y=tc_PIB))+
  geom_line(color="blue3")+
  labs(title="Comportamiento del PIB, 1970-2022", y="Tasa de crecimiento", x="Año")+
  theme_minimal()
#AIB
ggplot(datospib, aes(x=Año, y=tc_AIB))+
  geom_line(color="blue3")+
  labs(title="Comportamiento del Ahorro Interno Bruto, 1970-2022", y="Tasa de crecimiento", x="Año")+
  theme_minimal()
#IED
ggplot(datospib, aes(x=Año, y=tc_IED))+
  geom_line(color="blue3")+
  labs(title="Comportamiento de la Inversión Extranjera Directa, 1970-2022", y="Tasa de crecimiento", x="Año")+
  theme_minimal()

#SUPUESTOS####

# 1-Supuesto de LINEALIDAD####
#Se produce cuando exsite relación lineal entre las variables independientes y la variable dependiente
plot(modeloP1,1)
cor.test(datospib$pib,datospib$aib)
cor.test(datospib$pib,datospib$ied)

#Meter logaritmos a la base de datos
datospib$log_PIB = log(datospib$pib)
datospib$log_AIB = log(datospib$aib)
datospib$log_IED = log(datospib$ied)

# Con Logaritmos####
modeloP1_log <-lm (datospib$log_PIB~datospib$log_AIB+datospib$log_IED)
summary(modeloP1_log)
summary(modeloP1_log$residuals)
residual = modeloP1_log$residuals

plot(modeloP1_log,1)
cor.test(datospib$log_PIB,datospib$log_AIB)
cor.test(datospib$log_PIB,datospib$log_IED)
#Decidimos utilizar el modelo en logaritmos para toda las pruebas

# 2-Normalidad de los residuos####
#Grafico
plot(modeloP1,2)
plot(modeloP1_log,2)
#Test de normalidad
shapiro.test(modeloP1_log$residuals) #Sí tiene distribución normal, p-value > 0.05
#La prueba de normalidad de Shapiro nos dice que H0: tiene una distribución normal y
#H1: No tiene un distribucion normal

#Jarque Bera####
jarque.bera.test(modeloP1_log$residual)
#p-value = 0.592, hay normalidad

#Anderson-Darling####
library(nortest)
ad.test(residual)
#p-value = 0.3179


#Graficos
#Histograma####

# Histograma y curva de densidad normal
ggplot(data = data.frame(residual), aes(x = residual)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "red4", color = "blue3") +
  stat_function(fun = dnorm, args = list(mean = mean(residual), sd = sd(residual)), 
                color = "black", size = 1) +
  labs(title = "Histograma de residuos\ndel modelo en Logaritmos", 
       x = "Residuo", y = "Densidad") +
  theme_minimal()

#GRÁFICO DE CAJA Y BIGOTES CON GGPLOT2
ggplot(data = data.frame(residual), aes(y = residual)) +
  geom_boxplot(fill = "blue3", color = "red4") +
  labs(title = "Diagrama de caja", y = "Residuos") +
  theme_minimal()

#GRÁFICA DE CUANTILES CON GGPLOT2
ggplot(data = data.frame(residual), aes(sample = residual)) +
  stat_qq(shape = 20) +
  stat_qq_line(color = "blue4", size = 1) +
  labs(title = "Gráfica de cuantiles", x = "Cuantiles teóricos", y = "Cuantiles de los datos") +
  theme_minimal()

#Modelos Aplicados

#Niveles
modeloP1 <-lm (datospib$pib~datospib$aib+datospib$ied)
summary(modeloP1$residuals)
summary(modeloP1)
residualPro = modeloP1$residuals
print(residualPro)

#Logaritmos
modeloP1_log <-lm (datospib$log_PIB~datospib$log_AIB+datospib$log_IED)
summary(modeloP1_log$residuals)
summary(modeloP1_log)
residual = modeloP1_log$residuals
print(residual)

#Modelo de diferencias
#Niveles
datospib$dif_pib <- c(NA, diff(datospib$pib))
datospib$dif_aib <- c(NA, diff(datospib$aib))
datospib$dif_ied <- c(NA, diff(datospib$ied))
modeloDif <- lm(dif_pib~dif_aib+dif_ied, data = datospib)
summary(modeloDif)
#Logaritmos
datospib$dif_log_pib <- c(NA, diff(datospib$log_PIB))
datospib$dif_log_aib <- c(NA, diff(datospib$log_AIB))
datospib$dif_log_ied <- c(NA, diff(datospib$log_IED))
modeloDif_log <- lm(dif_log_pib~dif_log_aib+dif_log_ied, data = datospib)
summary(modeloDif_log)
residual = modeloDif_log$residuals
plot(modeloDif_log$residual, type='l')

#Supuesto de Homocedasticidad####
#Gráfico
plot(modeloP1_log,3)

bptest(modeloP1_log) #p-value = 0.496
gqtest(modeloP1_log) #p-value = 0.9707

modeloP1_log <-lm (datospib$log_PIB~datospib$log_AIB+datospib$log_IED)
summary(modeloP1_log$residuals)
summary(modeloP1_log)
residual = modeloP1_log$residuals
print(residual)
residual2 = residual^2
print(residual2)

# Data frame con las variables necesarias
data <- data.frame(aib = datospib$aib,
                   residuals = residual2)
# Gráfica
ggplot(data, aes(x = aib, y = residuals)) +
  geom_point(shape = 20, size = 2) +                      
  geom_smooth(method = "lm", color = "blue4", size = 1.5) + 
  labs(
    title = "Residuales vs AIB",
    x = "AIB",
    y = "Residuales"
  ) +
  theme_minimal()       

# Data Frame
data <- data.frame(ied = datospib$ied,
                   residuals = residual2)
# Gráfica
ggplot(data, aes(x = ied, y = residuals)) +
  geom_point(shape = 20, size = 2) +                      
  geom_smooth(method = "lm", color = "red", size = 1.5) + 
  labs(
    title = "Residuales vs IED",
    x = "IED",
    y = "Residuales"
  ) +
  theme_minimal()  

#Test Breusch-Pagan
#H0:El modelo es homocedastico (p > 0.05)
#H1:El modelo no es homocedastico (heterocedastico)
library(car)
ncvTest(modeloP1_log)
#Se rechaza la hipotesis nula
#El modelo no es homocedastico, es heterocedastico.
#Se rechaza la H0 porque P=0.0294, o sea, P < 0.05

#Fit Values
library(lmtest)
#En niveles
modelo_bp = lm(datospib$pib~datospib$aib+datospib$ied)
bptest(modelo_bp)
#Se trata de que no sea siginificativo para poder rechazar la hipotesisi nula

#Con logaritmos
modeloP1_log <-lm (datospib$log_PIB~datospib$log_AIB+datospib$log_IED)
modelo_bp_log = lm(datospib$log_PIB~datospib$log_AIB+datospib$log_IED)
bptest(modelo_bp_log)
#Con el modelo en logaritmos hay Homocedasticidad (P > 0.05)
plot(modelo_bp_log,3)

# BP=9.4993. BP=1.4025
# P=0.0086.  P=0.496
#Para poder rechazar la hipotesis nula no debe de ser significativo

# Pruebas de No Multicolinealidad####
install.packages("DescTools")
library(DescTools)

#Modelo en logaritmos
vif(modeloP1_log)

#Otras pruebas de No Multicolinealidad
#Correlaciones
cor(datospib$log_PIB, datospib$log_AIB)
cor(datospib$log_PIB, datospib$log_IED)
cor(datospib$log_AIB, datospib$log_IED)
#Sí hay multicolinealidad en las variables independientes "x",
#pero sigue siendo menor que la correlación que tiene Y (variables dependiente)
#con las variables independientes


#Matriz de correlaciones####
matrizP = datospib[,c("log_PIB","log_AIB","log_IED")]
cor(matrizP)

#Test FIV
datospib$dif_log_pib <- c(NA, diff(datospib$log_PIB))
datospib$dif_log_aib <- c(NA, diff(datospib$log_AIB))
datospib$dif_log_ied <- c(NA, diff(datospib$log_IED))

#No Autocorrelación####
#Logaritmicos
modeloP1_log <-lm (datospib$log_PIB~datospib$log_AIB+datospib$log_IED)
dwtest(modeloP1_log)
#Diferencias Logaritmicas
modeloDif_log <- lm(dif_log_pib~dif_log_aib+dif_log_ied, data = datospib)
dwtest(modeloDif_log)
#El modelo que mejor se ajustó fue el de diferencias logaritmicas, teniendo 
#un valor P de 0.6247 y un DW de 2.0759

modeloDif_log <- lm(dif_log_pib~dif_log_aib+dif_log_ied, data = datospib)
summary(modeloDif_log)
residual = modeloDif_log$residuals
plot(modeloDif_log$residual, type='l')
vif(modeloDif_log)

acf(residual)
acf <- acf(residual)
acf
pacf(residual)
pacf <-pacf(residual)
pacf

#Conclusión: El modelo es funcional y significativo.
