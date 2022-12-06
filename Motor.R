rm(list=ls()) 
library(readr)
library(modeest)
library(rjson)

data <- read_csv("Motor1_dados1.csv")
data <- as.data.frame(data)
colnames(data)=c("Segundos","Newtons","Volts")

#Removendo a última linha
data <- data[-length(data[,1]),]

#CONSTRUÇÃO DA MATRIZ PARA MODELAGEM ----
#Seleciona todos os elementos únicos da coluna de Tensão e salva em um vetor 
unic <- unique(data[,3])

modelagem = matrix(data=0, ncol=4, nrow=length(unic))
colnames(modelagem)=c("Newtons","Média","Mediana","Moda")

for (i in 1:length(unic)) {
  
  #Seleciona as linhas com tensões iguais ao valor do vetor unic
  filtered_rows <- data$Volts == unic[i]
  
  #Guarda as linhas com tensões iguais no vetor rows 
  rows <- data[filtered_rows,]
  
  #Guarda a média dos valores de força 
  media <- mean(rows[,2])
  
  #Guarda a mediana dos valores de força 
  mediana <- median(rows[,2])
  
  #Guarda a moda dos valores de força 
  moda <- mfv(rows[,2])
  
  #De maneira explicativa
  modelagem[i,1] <- unic[i]
  modelagem[i,2] <- media
  modelagem[i,3] <- mediana
  modelagem[i,4] <- moda[1] 
}

#PLOT DOS DADOS-----
plot(x=data[,3],y=data[,2],ylim=c(-.05,.01),type="n",ylab="Força (N)",
     xlab="Tesão (V)",main="Dados",lwd=2)
par(new=TRUE)
plot(x=data[,3],y=data[,2],type="l",ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="darkblue",lwd=2)

#HISTOGRAMA EXEMPLO-----
filtered_rows <- data$Volts == 8
rows <- data[filtered_rows,]

png("Rplot01.png", res = 300, width = 660, height = 446)
hist(rows[,2],xlab="Força (N)",ylab="Frequência",main="Motor 1 - 8 Volts",col="#ADD8E6")
dev.off()

median(rows[,2])
mean(rows[,2])
mfv(rows[,2])


hi=density(rows[,2])
lines(hi,col="red")

plot(rows[,1],rows[,2],type="l",col="red",lwd=1,ylab="Força (N)",
     xlab="Tempo (S)",main="Motor 1 com tensão igual a 8 Volts")

#GRÁFICOS  ----
#Média
plot(x=data[,3],y=data[,2],ylim=c(-.05,.01),type="n",ylab="Força (N)",
     xlab="Tesão (V)",main="Grafico dos Modelos",lwd=2)
par(new=TRUE)
plot(x=data[,3],y=data[,2],type="l",ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="darkblue",lwd=2)
par(new=TRUE)
plot(x=modelagem[,1],y=modelagem[,2],pch=19,ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="red",lwd=1)

#Mediana
plot(x=data[,3],y=data[,2],ylim=c(-.05,.01),type="n",ylab="Força (N)",
     xlab="Tesão (V)",main="Grafico dos Modelos",lwd=2)
par(new=TRUE)
plot(x=data[,3],y=data[,2],type="l",ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="darkblue",lwd=2)
par(new=TRUE)
plot(x=modelagem[,1],y=modelagem[,3],pch=19,ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="red",lwd=1)

#Moda
plot(x=data[,3],y=data[,2],ylim=c(-.05,.01),type="n",ylab="Força (N)",
     xlab="Tesão (V)",main="Representação dos Dados",lwd=2)
par(new=TRUE)
plot(x=data[,3],y=data[,2],type="l",ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="darkblue",lwd=2)
par(new=TRUE)
plot(x=modelagem[,1],y=modelagem[,4],pch=19,ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="red",lwd=1)
text(8.5,.008,"Todos os dados",adj=c(0,0),col="darkblue")
text(8.5,.005,"Moda dos dados",adj=c(0,0),col="red")

#CORRELAÇÃO ----
t.test(data[,3],data[,2],mu=0)

wilcox.test(data[,3],data[,2],mu=0)

cor(data[,3],data[,2])
#Média
cor(modelagem[,1],modelagem[,2])
#Mediana
cor(modelagem[,1],modelagem[,3])
#Moda
cor.test(modelagem[,1],modelagem[,4],method = "kendall")

shapiro.test(data[,2])
#REGRESSÃO ----
x <- modelagem[,1]
y <- modelagem[,4]

X <- x
XSQ <- x**2
XCUB <- x**3

model=lm(y~1+X+XSQ+XCUB)
anova(model)
summary(model)
func <- model$coefficients
print(func)

#RESÍDUOS----
erro=resid(model)
hist(erro)
#Teste de Normalidade Shapiro Wilk
sp=shapiro.test(erro)
print("Shapiro Test p-valor")
print(sp$p.value)
p_valor=sp$p.value
if(p_valor>0.1){
  print("Os dados provém de uma população Normal")
}else{
  print("Os dados não provém de uma população Normal")
}
#Gráficos
png("Rplot01.png", res = 300, width = 660, height = 446)
qqnorm(erro,pch=19,col="darkblue")
qqline(erro,col="red")
dev.off()

par(mfrow = c(1, 2), mar = c(5,4,2,2))
qqnorm(erro,pch=19,col="darkblue")
qqline(erro,col="red")
plot(erro,pch=19,col="darkblue",main="Resíduos")
abline(h=0,col="red")

#
x=0:10
y=as.numeric(func[1])+as.numeric(func[2])*x+as.numeric(func[3])*x**2+as.numeric(func[4])*x**3

#Moda
plot(x=data[,3],y=data[,2],ylim=c(-.05,.01),type="n",ylab="Força (N)",
     xlab="Tesão (V)",main="Representação dos Dados",lwd=2)
par(new=TRUE)
plot(x=data[,3],y=data[,2],type="l",ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="darkblue",lwd=2)
par(new=TRUE)
plot(x,y,type="l",ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="green",lwd=2)
par(new=TRUE)
plot(x=modelagem[,1],y=modelagem[,4],pch=19,ylim=c(-.05,.01),axes=FALSE,
     ann=FALSE,col="red",lwd=1)
text(8.5,.008,"Todos os dados",adj=c(0,0),col="darkblue")
text(8.5,.005,"Moda dos dados",adj=c(0,0),col="red")
text(8.5,.002,"Modelo",adj=c(0,0),col="green")


plot(x=n,y=n,ylim=c(25,90),type="n",ylab="Temperatura (?C)",
     xlab="Tempo (s)",main="Grafico dos Dados",lwd=2)

# Writing into JSON file. -----

# creating the list
list1 <- vector(mode="list", length=2)
list1[[1]] <- c("Intercept", "x", "x^2", "x^3")
list1[[2]] <- c(as.numeric(func[1]), as.numeric(func[2]),as.numeric(func[3]), as.numeric(func[4]))

# creating the data for JSON file
jsonData <- toJSON(list1)

# writing into JSON file
write(jsonData, "motor1.json") 

# Give the created file name to the function
motor1 <- fromJSON(file = "motor1.json")

# Print the result
print(motor1)


list2 <- vector(mode="list", length=1)
list2[[1]] <- erro
jsonData <- toJSON(list2)
write(jsonData, "resi.json")

list3 <- vector(mode="list", length=1)
list3[[1]] <- modelagem[,4]
jsonData <- toJSON(list3)
write(jsonData, "moda.json")
