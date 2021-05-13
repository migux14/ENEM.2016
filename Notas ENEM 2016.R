#Histograma e análise das notas de redação e matemática

dados <- read.table("ENEM2016.txt", header=T)

Mat <- dados$Mat
Red <- dados$Red

summary(Red)
summary(Mat)
var(Red)
var(Mat)

par(mfrow=c(1,2), oma=c(1,1,1,1))
hist(Red,main = "Redação", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Mat,main = "Matemática", xlab = "Nota", ylab = "Densidade", probability = T)

#Fazendo comparação com a Normal

mdMat <- mean(Mat)
mdRed <- mean(Red)
sdMat <- sd(Mat)
sdRed <- sd(Red)

par(mfrow=c(1,2), oma=c(1,1,1,1))

hist(Red,main = "Redação", xlab = "Nota", ylab = "Densidade", probability = T)
curve(dnorm(x, mean = mdRed, sd= sdRed), lwd =2, col = 2, add = T)

hist(Mat,main = "Matemática", xlab = "Nota", ylab = "Densidade", probability = T)
curve(dnorm(x, mean = mdMat, sd = sdMat), lwd =2, col = 2, add= T)

#Curtose e assimetria (Pacote moments e e1071)

library(moments)
library(e1071)

curt.Mat <- moments::kurtosis(Mat)
curt.Red <- moments::kurtosis(Red)

ass.Mat <- skewness(Mat)
ass.Red <- skewness(Red)

#Assimetria e curtose das notas de matemática, respectivamente
ass.Mat
curt.Mat

#Assimetria e curtose das notas de redação, respectivamente
curt.Red
ass.Red

#Fazendo padronização das observações
##subtraindo a média de cada observação
Mat.p <- Mat-mdMat
Red.p <- Red-mdRed

par(mfrow=c(1,2))
hist(Mat.p,main = "Matemática", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Red.p,main = "Redação", xlab = "Nota", ylab = "Densidade", probability = T)

##Dividindo cada observação pelo desvio padrão
Mat.p <- Mat.p/sdMat
Red.p <- Red.p/sdRed

par(mfrow=c(1,2))
hist(Mat.p,main = "Matemática", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Red.p,main = "Redação", xlab = "Nota", ylab = "Densidade", probability = T)

#Padronizada vs Não-Padronizada
par(mfrow=c(1,2))
hist(Mat,main = "Matemática", xlab = "Nota", ylab = "Densidade", probability = T)
hist(Mat.p,main = "Mat. Padronizada", xlab = "Nota", ylab = "Densidade", probability = T)

#Comparando com a Normal padrão (Média = 0, variância = 1)
par(mfrow=c(1,2))
hist(Mat,main = "Matemática", xlab = "Nota", ylab = "Densidade",xlim = c(300,900), probability = T)
curve(dnorm(x, mean = mdMat, sd = sdMat), col = 2, lwd = 2, add= T)

hist(Mat.p,main = "Mat. Padronizada", xlab = "Nota", ylab = "Densidade",xlim = c(-3,6), probability = T)
curve(dnorm(x, mean = 0, sd= 1), col = 2, lwd = 2, add = T)
