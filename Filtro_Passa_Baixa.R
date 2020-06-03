# Declarar bibliotecas -----------------------------------------------------------------------------------------------------

library(oce)
library(lubridate)
library(ggplot2)

# Arquivo de entrada com colunas dispostas na seguinte ordem: Dia/Mês/Ano  Hora:Minuto:Segundo  Observação_Maregráfica_Horárias
# Período de observações empregado: 21/03/2002-19:30:00 até 16/10/2002-23:55:00-----------------------------------------------
# Marégrafo de Imbituba-SC----------------------------------------------------------------------------------------------------

file='Link do diretório do arquivo com dados maregráficos horários.txt'
v <-read.table(file, header = FALSE, sep = "", dec = ".")
nl=nrow(v['V1'])
elev <- as.numeric(unlist(v['V3']))
ti=strptime(v[1,'V1']:v[1,'V2'],format='%d/%m/%Y:%H:%M:%S')
tf=strptime(v[nl,'V1']:v[nl,'V2'],format='%d/%m/%Y:%H:%M:%S')
x<-as.POSIXct(c(ti,tf), tz="UTC")
tempo<-seq(x[1], x[2], by = "5 min")

# Aplicar Filtro de PUGH (1987, p. 416) ---------------------------------------------------------------------------------------

val=c()
temp=c()
nf=nl-59
m <- c(0.0643225,0.0628604,0.0604728,0.0572315,0.0532331,0.0485954,0.0434525,0.0379505,0.0322412,0.0264773,0.0208063,0.0153661,
0.0102800,0.0056529,0.0015685,-0.0019127,-0.0047544,-0.0069445,-0.0084938,-0.0094346,0.0098173,-0.0097074,-0.0091818,-0.0083247,
-0.0072233,-0.0059642,-0.0046296,-0.0032942,-0.0020225,-0.0008672,0.0001321,0.0009493,0.0015716,0.0019984,0.0022398,0.0023148,
0.0022492,0.0020729,0.0018178,0.0015155,0.0011954,0.0008830,0.0005986,0.0003568,0.0001662,0.0000294,-0.0000560,-0.0000970,
-0.0001032,-0.0000862,-0.0000578,-0.0000288,-0.0000077,0.0000000)

for(i in seq(from=61, to=nf, by=12)){
  x1=elev[i]*0.0648148
  tim= as.character(v[i,'V1']:v[i,'V2'], format = c('%d/%m/%Y:%H:%M:%S'))
  print(tim)
  val2 = c()
  for(j in seq(from=1, length(m), by=1)){
    x2=m[j]*(elev[i+j]+elev[i-j])
    val2 <- append(val2, x2)
  }
  valor=x1+sum(val2)
  val <- append(val, valor)
  temp <- append(temp, tim)
}

hora=substr(temp, start = 12, stop = 19)
dia=substr(temp, start = 1, stop = 10)

# Salvar os dados filtrados em um arquivo .txt com as colunas na mesma disposição dos dados de entrada -------------------------------

df1 <- data.frame(Dia_UTC=dia, Hora_UTC=hora, SeaLevel = val)
write.table(df1,'Link do diretório para salvar os dados filtrados.txt', row.names = FALSE, col.names=FALSE, quote=FALSE)
