# Declarar bibliotecas ------------------------------------------------------------------------------------------------------

library(oce)
library(lubridate)
library(ggplot2)


# Arquivo de entrada com colunas dispostas na seguinte ordem: Dia/Mês/Ano  Hora:Minuto:Segundo  Observação_Maregráfica_5_Minutos
# Marégrafo de Imbituba-SC------------------------------------------------------------------------------------------------------
# Três arquivos de entrada foram empregados------------------------------------------------------------------------------------

# Arquivo 1: 21/03/2002-19:30:00 até 16/10/2002-23:55:00 ----------------------------------------------------------------------

file1='Link do diretório do arquivo 1 com dados maregráficos amostrados com intervalos de 5,10,15.txt'
v1 <-read.table(file1, header = FALSE, sep = "", dec = ".")
nl1=nrow(v1['V1'])
elev1 <- as.numeric(unlist(v1['V3']))
ti1=strptime(v1[1,'V1']:v1[1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf1=strptime(v1[nl1,'V1']:v1[nl1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
x1<-as.POSIXct(c(ti1,tf1), tz="UTC")
time1<-seq(x1[1], x1[2], by = "5 min")

# Arquivo 2: 17/10/2002 até 10/10/2006 ---------------------------------------------------------------------------------------

file2='Link do diretório do arquivo 2 com dados maregráficos amostrados com intervalos de 2,7,12.txt'
v2 <-read.table(file2, header = FALSE, sep = "", dec = ".")
nl2=nrow(v2['V1'])
elev2 <- as.numeric(unlist(v2['V3']))
ti2=strptime(v2[1,'V1']:v2[1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf2=strptime(v2[nl2,'V1']:v2[nl2,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
xx2<-as.POSIXct(c(ti2,tf2), tz="UTC")
time2<-seq(xx2[1], xx2[2], by = "5 min")

# Arquivo 3: 10/10/2006 até 31/12/2015 --------------------------------------------------------------------------------------

file3='Link do diretório do arquivo 3 com dados maregráficos amostrados com intervalos de 5,10,15.txt''
v3 <-read.table(file3, header = FALSE, sep = "", dec = ".")
nl3=nrow(v3['V1'])
elev3 <- as.numeric(unlist(v3['V3']))
ti3=strptime(v3[1,'V1']:v3[1,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf3=strptime(v3[nl3,'V1']:v3[nl3,'V2'],format='%d/%m/%Y:%H:%M:%S')+hours(3)
xx3<-as.POSIXct(c(ti3,tf3), tz="UTC")
time3<-seq(xx3[1], xx3[2], by = "5 min")

# Juntar os dados dos 3 arquivos de entrada --------------------------------------------------------------------------------

tempo=c(time1, time2, time3) 
elev=c(elev1, elev2, elev3)
nl=nl1+nl2+nl3

# Aplicar o filtro três sigma para remover outliers -------------------------------------------------------------------------

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

# Confeccionar as Marés Sintéticas (MSs) pela função TIDEM ----------------------------------------------------------------------

datsl <- as.sealevel(elevation, tempo)
tide <- tidem(datsl)
MS=predict(tide,tempo)

# Substituindo o valor de NaN por valores obtidos do modelo de maré sintética ----------------------------------------------------

substituir<- 
  function (MS,elev, nl) {
    for(i in seq(from=1, to=nl, by=1)){
      teste=is.na(elev[i])
      if(teste==TRUE){
        t=MS[i]
      } 
      else{
        t=elev[i]
      }
      answer <- append(answer, t)
    }
    return(answer)}
result<-substituir(MS,elevation,nl)

# Confeccionar o intervalo de tempo entre 17/10/2002-00:00:00 e 10/10/2006-12:25:00 com amostragem de tempo de 0,5,10,15... 
# Isso se deve pelo fato dos dados entre 17/10/2002-00:02:00 e 10/10/2006-12:22:00 estarem amostrados em intervalor de 2,7,12,17...

ti2=strptime('17/10/2002:00:00:00',format='%d/%m/%Y:%H:%M:%S')+hours(3)
tf2=strptime('10/10/2006:12:25:00',format='%d/%m/%Y:%H:%M:%S')+hours(3)
x2<-as.POSIXct(c(ti2,tf2), tz="UTC")
time_MS2<-seq(x2[1], x2[2], by = "5 min")

MS2=predict(tide,time_MS2)

# Salvar os dados produzidos pela maré sintética para o período entre 17/10/2002-00:00:00 e 10/10/2006-12:20:00 com o intervalo de 0,5,10,15----------------------------------------------------------------------------------------------------------------------

df1 <- data.frame(Dia_UTC=format(time_MS2, "%d/%m/%Y"), Hora_UTC=format(time_MS2, "%H:%M:%S"), SeaLevel = MS2)
write.table(df1,'Link do diretório para salvar os dados sem gaps.txt'', row.names = FALSE, col.names=FALSE, quote=FALSE)

# Salvar todos os dados com os gaps preenchidos -------------------------------------------------------------------------------------

df2 <- data.frame(Dia_UTC=format(tempo, "%d/%m/%Y"), Hora_UTC=format(tempo, "%H:%M:%S"), SeaLevel = result)
write.table(df2,'Link do diretório para salvar os dados sem gaps.txt', row.names = FALSE, col.names=FALSE, quote=FALSE)

