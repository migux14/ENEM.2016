#Histograma e an�lise das notas de reda��o e matem�tica

dados <- read.table("ENEM2016.txt", header=T)

Mat <- dados$Mat
Red <- dados$Red

summary(Red)
summary(Mat)
var(Red)
var(Mat)

par(mfrow=c(1,2), oma=c(1,1,1,1))
hist(Red,main = "Reda��o", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Mat,main = "Matem�tica", xlab = "Nota", ylab = "Densidade", probability = T)

#Fazendo compara��o com a Normal

mdMat <- mean(Mat)
mdRed <- mean(Red)
sdMat <- sd(Mat)
sdRed <- sd(Red)

par(mfrow=c(1,2), oma=c(1,1,1,1))

hist(Red,main = "Reda��o", xlab = "Nota", ylab = "Densidade", probability = T)
curve(dnorm(x, mean = mdRed, sd= sdRed), lwd =2, col = 2, add = T)

hist(Mat,main = "Matem�tica", xlab = "Nota", ylab = "Densidade", probability = T)
curve(dnorm(x, mean = mdMat, sd = sdMat), lwd =2, col = 2, add= T)

#Curtose e assimetria (Pacote moments e e1071)

library(moments)
library(e1071)

curt.Mat <- moments::kurtosis(Mat)
curt.Red <- moments::kurtosis(Red)

ass.Mat <- skewness(Mat)
ass.Red <- skewness(Red)

#Assimetria e curtose das notas de matem�tica, respectivamente
ass.Mat
curt.Mat

#Assimetria e curtose das notas de reda��o, respectivamente
curt.Red
ass.Red

#Fazendo padroniza��o das observa��es
##subtraindo a m�dia de cada observa��o
Mat.p <- Mat-mdMat
Red.p <- Red-mdRed

par(mfrow=c(1,2))
hist(Mat.p,main = "Matem�tica", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Red.p,main = "Reda��o", xlab = "Nota", ylab = "Densidade", probability = T)

##Dividindo cada observa��o pelo desvio padr�o
Mat.p <- Mat.p/sdMat
Red.p <- Red.p/sdRed

par(mfrow=c(1,2))
hist(Mat.p,main = "Matem�tica", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Red.p,main = "Reda��o", xlab = "Nota", ylab = "Densidade", probability = T)

#Padronizada vs N�o-Padronizada
par(mfrow=c(1,2))
hist(Mat,main = "Matem�tica", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Mat.p,main = "Mat. Padronizada", xlab = "Nota", ylab = "Densidade", probability = T)

#Comparando com a Normal padr�o (M�dia = 0, vari�ncia = 1)
par(mfrow=c(1,2))
hist(Mat,main = "Matem�tica", xlab = "Nota", ylab = "Densidade",xlim = c(300,900), probability = T)
curve(dnorm(x, mean = mdMat, sd = sdMat), col = 2, lwd = 2, add= T)

hist(Mat.p,main = "Mat. Padronizada", xlab = "Nota", ylab = "Densidade",xlim = c(-3,6), probability = T)
curve(dnorm(x, mean = 0, sd= 1), col = 2, lwd = 2, add = T)
