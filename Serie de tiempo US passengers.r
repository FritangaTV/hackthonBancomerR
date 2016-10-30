#Establece el directorio trabajo
setwd("C:/trabajo independiente 2011/curso facultad ciencias")

#Lee los archivos .csv y los almacena

 datos<-read.table("Airpassengers.csv",sep=",",header=FALSE)
 
 #Los datos son anuales y parten de 1949 a  1960, hay que convertirlos a formato de series de tiempo. La siguiente instrucci�n
 #dice que hay que crear una serie con estacionalidad de 12 que empieza en el mes de Enero de 1949
 #(por eso se usa c(1949,1)
 
 serie<-ts(datos, frequency = 12, start = c(1949, 1))
 
 serie

#Cargar la librer�a que contiene an�lisis de series de tiempo

library(stats)

#Gr�fica de la serie de tiempo

plot(serie)

#Suavizamiento exponencial, m�todo de Holt y Holt Winters multiplicativo y aditivo.
#Todos  se basan en la misma funci�n HoltWinters que depende de tres par�metros,
# alfa, beta y gamma, si gamma y beta son FALSE el suavizamiento es exponencial,
# si gamma=FALSE el suavizamiento es Holt o doble exponencial. Si no se especifican los
# par�metros estos se determinan de forma autom�tica de acuerdo al error cuadr�tico medio.

#Para cuando hay tendencia y nivel (suavizamiento doble exponencial), los valores iniciales del algoritmo
#son L1gorro=X1 (valor serie 1a observacion) y b1gorro=X2-X1. Estos se pueden cambiar usando l.start y b.start.
#Los valores ajustados se proporcionan a partir de la 2a observaci�n.


#Suavizamiento exponencial  permitiendo que el programa elija el par�metro alpha
#Si se escribe exp1 (el nombre dado al suavizamiento) me da el valor de los par�metros
#fitted(exp1) me da la serie ajustada y por eso lines es la instrucci�n para crear la gr�fica
#de los valores ajustados.

exp1 <- HoltWinters(serie, gamma = FALSE, beta = FALSE)
fitted(exp1)
plot(serie)
lines(fitted(exp1)[,1], col = "red")

exp1
fitted(exp1)

#Suavizamiento exponencial dando el par�metro alpha

exp2 <- HoltWinters(serie, alpha=0.6, gamma = FALSE, beta = FALSE)
plot(serie)
lines(fitted(exp2)[,1], col = "green")


# Doble exponencial o Holt, si se deja que el programa elija da un valor de beta=0, que ser�a un suavizamiento
#exponencial as� que abajo se muestra cuando nosotros fijamos la alpha=0.8 y beta=0.2

dobleexp1 <- HoltWinters(serie, gamma = FALSE)
plot(serie)
lines(fitted(dobleexp1)[,1], col = "red")

dobleexp2 <- HoltWinters(serie, alpha=0.8, beta=0.2, gamma = FALSE)
plot(serie)
lines(fitted(dobleexp2)[,1], col = "green")

dobleexp2

#Suavizamiento doble exponencial con valores iniciales L1gorro=X1 y b1gorro=0, que era la forma alternativa
#para iniciar el algoritmo

dobleexp3 <- HoltWinters(serie, alpha=0.8, beta=0.2, gamma = FALSE, l.start=serie[1], b.start=0)
plot(serie)
lines(fitted(dobleexp3)[,1], col = "green")

 #Holt Winters aditivo, la estacionalidad (12) ya viene impl�cita en los datos y permitimos que el programa
 # elija los valores de alpha, beta y gamma
 
 HWa <- HoltWinters(serie, seasonal = "additive")
 plot(serie)
lines(fitted(HWa)[,1], col = "red")

HWa

#Holt Winters multiplicativo, la estacionalidad (12) ya viene impl�cita en los datos

 HWm <- HoltWinters(serie, seasonal = "multiplicative")
 plot(serie)
lines(fitted(HWm)[,1], col = "green")

HWm

#Predicciones para cualquier modelo suavizado visto arriba, en particular aqu� se presenta la predicci�n para un a�o
# adicional con el m�todo multiplicativo.

pred <- predict(HWm, 12, prediction.interval = TRUE)
plot(HWm, pred)

#Descomposici�n cl�sica

# Modelar la tendencia mediante una regresi�n. Si se observa la serie parece que la relaci�n es lineal.
# Primero se genera una sequencia de tiempos de 1949 a 1960 cada uno dividido en 12 meses, es decir 144
# tiempos

t<-seq(1949,1960,length=length(serie))

#Hacemos la regresi�n tomando el tiempo como variable explicativa y las serie como variable respuesta.
#Proporcionamos los valores ajustados bajo la regresi�n.

regres<-lm(serie~t)
fitted.values(regres)

#Esta gr�fica proporciona la regresi�n ajustada (en rojo) junto con la serie de datos original.
plot(serie)
abline(regres,col=2)

#Obtenemos los residuales, la gr�fica plot(residuals(regres)) no los muestra como series de tiempo sino
# como scatter plot. Podemos convertir los residuales en una serie de tiempo para ver la gr�fica de los residuos
#como serie de tiempo, estos deber�an ser estacionarios o modelarloc como un ARMA

residuals(regres)
plot(residuals(regres))

resserie<-ts(residuals(regres), frequency = 12, start = c(1949, 1))
plot(resserie)

#Promedios m�viles, modelan tendencia �nicamente.

#Se usa la funci�n filter, filter= es la instrucci�n despu�s del cual damos los pesos del filtro, para promedios
#m�viles es 1/(n�mero de t�rminos que se promedian) o lo que es lo mismo 1/(2q+1), as� que se usa la instrucci�n
#rep para crear un vector de pesos 1/(2q+1) de longitud 2q+1

plot(serie,type="l")

#Promedios m�viles con q=1
PM3term <- filter(serie,filter=rep(1/3,3))
#Promedios m�viles con q=2
PM5term <- filter(serie,filter=rep(1/5,5))
#Gr�ficas de la serie original y suavizamientos
lines(PM3term,col="red")
lines(PM5term,col="green")

#La descomposici�n clasica con estacionalidad vista en clase puede programarse; sin embargo, en R ya existe
#una funci�n llamada stl que hace una descomposici�n que considera estacionalidad.

serie<-ts(datos, frequency = 12, start = c(1949, 1))
stl(serie[,1],s.window="periodic")
plot(stl(serie[,1],s.window="periodic"))

#Obtenci�n de diferencias para eliminar tendencias y estacionalidad. La funci�n diff sirve para obtener
#la serie diferenciada, lag indica el retraso, por ej. para un modelo con estacionalidad s podr�amos usar lag de
# s y differences indica el orden de las diferencias, i.e. el n�mero de veces que se est� diferenciando.

#Obtener la serie con lag de 12 para eliminar estacionalidad

diflag12<-diff(serie, lag = 12, differences = 1)
plot(diflag12)

#Diferenciar la serie obtenida arriba usando ahora un lag 1 para tratar de eliminar la tendencia

diflag1<-diff(diflag12, lag = 1, differences = 1)
plot(diflag1)

#Si se diferencia 2 veces la serie sin estacionalidad  (difflag12) entonces se usa la instrucci�n siguiente.

diflag2<-diff(diflag12, lag = 1, differences = 2)
plot(diflag2)

# De estas transformaciones se prefiere la serie en que se diferenci� primero de acuerdo a la estacionalidad
# (lag 12) y luego la serie obtenida al diferenciar esta �tima usando un lag  de 1 una sola vez, o sea se usa la serie diflag1.
#No olvidar restar la media de los datos que se van a modelar. En la instrucci�n se usa scale=FALSE porque no
#queremos estandarizar los datos dividiendo entre la desviaci�n est�ndar, solo queremos restar la media.

serieamodelar<-scale(diflag1, scale = FALSE)
serieamodelar

#Autocorrelaciones y autocorrelaciones parciales de los datos. Una observaci�n es que si se usa la siguiente instrucci�n
# aparecen lags que est�n en t�rminos de las estacionalidades, o sea lags decimales, as� que generalmente se prefiere
#la segunda instrucci�n la cual queda en t�rminos de lags enteros porque la instrucci�n ts(serieamodelar,freq=1) transforma
#la serie a una serie en la que no consideran los meses dentro de los a�os.

acf(serieamodelar, lag.max=40, main="ACF del proceso")
acf(ts(serieamodelar,freq=1), lag.max=40, main="ACF del proceso")

#Las autocorrelaciones parciales son:

pacf(ts(serieamodelar,freq=1), lag.max=40, main="PACF del proceso")

#De las ACF sale la ACF para el lag 1 y 23 se sugiere al menos MA(1) y de PACF se sugiere tambi�n AR de
#al menos 1 pues los lags 20 y 22 tambi�n salen de las bandas, hacer ajuste para un ARMA(1,1)

#Primero ilustramos s�mo se obtienen los par�metros v�a Yule Walker, lo cual se usa en modelos AR. Supongamos AR(1)
#Para dar un AR  espec�fico usar la instrucci�n aic=FALSE y order.max indica el orden del modelo AR, en este caso 1.
# En caso contrario si se usa aic= FALSE se obtiene el mejor modelo de acuerdo a  el criterio Akaike.

w<-ar.yw(serieamodelar, aic = FALSE, order.max =1)
w
w$aic

#Si dejamos que de manera automatizada se encuentre el mejor modelo AR, elige el AR(1)

w2<-ar.yw(serieamodelar, aic = TRUE)
w2

#Si queremos ajustar el modelo ARMA(1,1) por m�xima ver. para las diferencias se usa order c(1,0,1) porque el orden de AR es 1,
#el primer t�rmino en el par�ntesis, la serie ya no se diferencia m�s (porque ya la diferenciamos antes) por lo cual
#el segundo lugar del par�ntesis es 0 y el �ltimo corresponde a la parte MA del modelo.  Adem�s include.mean=FALSE,
#porque ya hab�amos restado la media a los datos. Los valores iniciales para el ajuste son distintos que en ITSM,
#pero uno puede darlos agregando init

ARMA11<-arima(serieamodelar,order=c(1,0,1), include.mean= FALSE)
ARMA11

#Si para el AR(1) de arriba estimamos por m�x. ver. usando como valores iniciales lo obtenido v�a Yule Walker agregamos
#la instrucci�n init=w$ar que indica que el valor inicial es el estimado v�a Yule Walker que guardamos con nombre w.
#Adem�s se usa c(1,0,0) para indicar que es un AR(1)

AR1bis<-arima(serieamodelar,order=c(1,0,0), include.mean= FALSE,init=w$ar)
AR1bis

#Queda igual que si en ITSM calculamos por Yule Walker y luego m�x. ver. Para el ARMA(1,1) no queda exactamente por
#los valores iniciales que son distintos en cada caso.

#Como en la serie original lo que hicimos es diferenciar una vez de acuerdo a la estacionalidad, o sea con lag 12,
#y luego una vez para eliminar tendencia usando un lag de 1, en realidad pudimos haber introducido el modelo con
#los datos orginales y obtener el mismo modelo ARMA(1,1) visto introduciendo al modelo como un SARIMA((1,1,1),(0,1,0))
#donde el primer par�ntesis indica que es un ARMA diferenciando una vez (el segundo 1 del par�ntesis indica esto). El
#segundo par�ntesis indica que en la parte estacional se diferencio una vez (esta es la diferencia con lag 12)


ARMA11forma2<-arima(serie,order=c(1,1,1), seasonal = list(order = c(0, 1, 0), period = NA),  include.mean= FALSE)
ARMA11forma2

#Podemos hacer predicciones sobre los siguientes 12 meses e incluso graficarlo

ARMA11.pred<-predict(ARMA11forma2,n.ahead=12)
ARMA11.pred
plot(serie)
lines(ARMA11.pred$pred,col="red")
lines(ARMA11.pred$pred+1.965*ARMA11.pred$se,col="red",lty=3)
lines(ARMA11.pred$pred-1.965*ARMA11.pred$se,col="red",lty=3)

#El problema con la otra forma de ver el ARMA(1,1) sobre las diferencias es que las predicciones son sobre la serie
#diferenciada y hay que regresarse a la original.

ARMA11.pred2<-predict(ARMA11,n.ahead=12)
ARMA11.pred2
plot(serieamodelar)
lines(ARMA11.pred2$pred,col="red")
lines(ARMA11.pred2$pred+1.965*ARMA11.pred2$se,col="red",lty=3)
lines(ARMA11.pred2$pred-1.965*ARMA11.pred2$se,col="red",lty=3)

#Verificaci�n supuestos

#Se usa la funci�n tsdiag que proporciona gr�ficas para los residuales. En este caso lo estamos usando sobre el modelo que usa los datos
#originales, pero pudimos haber usado el otro basado en las diferencias (modelo ARMA11). Observamos que las autocorrelaciones de los residuos
#no se salen de las bandas de confianza  lo cual sugiere un ruido blanco. Adem�s todos los p-values del estad�stico de Ljiung Box est�n
#por arriba de un nivel de significancia de 0.05 hasta el lag 10 al menos as� que no rechazo la hip�tesis nula de
#que las autocorrelaciones conjuntamente son cero as� que esto confirma que el residuo es aproximadamente un ruido blanco.

tsdiag(ARMA11forma2)

#Finalmente se presentan  los residuales y su histograma, que sugieren una distribuci�n m�s o menos normal.

 residuales<-ARMA11$residuals
 plot(residuales)
 hist (residuales,main ="Histograma residuales")
 
 #Una prueba formal de la normalidad es la de Lilliefors(Kolmogorov Sminrov), instalar el pquete nortest

 library(nortest)
 lillie.test(residuales)
 
 #El p-value de la prueba de Liliefors es 0.5347 as� que para un nivel de 0.05 no rechazo la hip�tesis de que los residuos
 #se distribuyen normalmente.
 
 #Entonces los datos cumplen los supuestos y el modelo SARIMA(1,,1,1)(0,1,0) parece ser un buen modelo.


 