# Declarar as bibliotecas ##############################################################################################

library(oce)
library(lubridate)
library(ggplot2)

#Arquivo 21/03/2002-19:30:00 até 16/10/2002-23:55:00 ###################################################################

file='links_do_arquivo_txt_1'
v1 <-read.table(file, header = FALSE, sep = "", dec = ".")
nl1=nrow(v1['V1'])
elev1 <- as.numeric(unlist(v1['V3']))
ti1=strptime(v1[1,'V1']:v1[1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf1=strptime(v1[nl1,'V1']:v1[nl1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
xx1<-as.POSIXct(c(ti1,tf1), tz="UTC")
time1<-seq(xx1[1], xx1[2], by = "5 min")

#Arquivo 17/10/2002-00:02:00 até 10/10/2006-12:22:00 #################################################################

file2='links_do_arquivo_txt_2'
v2 <-read.table(file2, header = FALSE, sep = "", dec = ".")
nl2=nrow(v2['V1'])
elev2 <- as.numeric(unlist(v2['V3']))
ti2=strptime(v2[1,'V1']:v2[1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf2=strptime(v2[nl2,'V1']:v2[nl2,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
xx2<-as.POSIXct(c(ti2,tf2), tz="UTC")
time2<-seq(xx2[1], xx2[2], by = "5 min")

#Arquivo 10/10/2006-12:25:00 até 31/12/2015-23:55:00 #################################################################

file3='links_do_arquivo_txt_3'
v3 <-read.table(file3, header = FALSE, sep = "", dec = ".")
nl3=nrow(v3['V1'])
elev3 <- as.numeric(unlist(v3['V3']))
ti3=strptime(v3[1,'V1']:v3[1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf3=strptime(v3[nl3,'V1']:v3[nl3,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
xx3<-as.POSIXct(c(ti3,tf3), tz="UTC")
time3<-seq(xx3[1], xx3[2], by = "5 min")

tempo=c(time1, time2, time3) 
elev=c(elev1, elev2, elev3)

# Aplicar o filtro três sigma #########################################################################################

sigma=3
answer = c()
teste<- 
  function (elev,sigma) {
    des_padrao<- sd(na.omit(elev))
    media<- mean(na.omit(elev))
    teste_menos=media-sigma*des_padrao
    teste_mais=media+sigma*des_padrao
    answer = c()
    for (val in elev) {
      tes=(is.na(val))
      if(tes==TRUE){
        t=NaN
      } 
      else{
        if(val < teste_mais & val>teste_menos){
          t=val
        } 
        else{
          t=NaN
        }}
      answer <- append(answer, t)
    }
    return(answer)}

elevation<-teste(elev,sigma)

# Confeccionar as Marés Sintéticas (MSs) ##############################################################################

datsl <- as.sealevel(elevation, tempo)
MS=predict(tidem(datsl))

# Plotar o gráfico dos dados observados, das Marés Sintéticas e dos resíduos ##########################################

oce.plot.ts(tempo, elevation, type='l', ylab="Nível d'água [m]", ylim=c(-1, 3.5),xlab="Período [Anos]",col="black",xaxs='i', drawTimeRange=FALSE)
legend("topright", col=c("black",alpha("orange", 0.8),"red"),
       legend=c("Dados observados (RMPG)","Maré sintética (Tidem)" ,"Resíduos"), lwd=2.8)
abline(h=0, col="black",lwd=2)
lines(tempo, MS, col=alpha("orange", 0.8))
lines(tempo, elevation - MS, col="red")

# Calcular a média, o desvio padrão e o coeficiente de correlação de Pearson entre as MSs e os dados observados########

print("Média e desvio dos resíduos:")
print(mean((na.omit(elevation - MS))))
print(sd((na.omit(elevation - MS))))
print("Média e desvio dos MSs:")
print(mean((na.omit(MS))))
print(sd((na.omit(MS))))
print("Média e desvio dados observados:")
print(mean((na.omit(elevation))))
print(sd((na.omit(elevation))))
print("Coeficiente de corrlação de Pearson:")
print(cor(na.omit(MS),na.omit(elevation),  method = "pearson"))

# Verificar se a distribuição das MSs é semelhante à curva normal ###################################################

d1<-na.omit(MS)
m1<-(mean(d1))
std1<-(sd(d1))
hist(d1, density=20, breaks=20, prob=TRUE, 
     xlab="Nível d'água [m]", ylab="Densidade", ylim=c(0, 3), 
     main="Histograma (Marés Sintéticas)")
curve(dnorm(x, mean=m1, sd=std1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

# Verificar se a distribuição das observações maregráficas é semelhante à curva normal ############################

d2<-na.omit(elevation)
m2<-(mean(d2))
std2<-(sd(d2))
hist(d2, density=20, breaks=20, prob=TRUE, 
     xlab="Nível d'água [m]", ylab="Densidade", ylim=c(0, 3), 
     main="Histograma (Observações Marégraficas)")
curve(dnorm(x, mean=m2, sd=std2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")