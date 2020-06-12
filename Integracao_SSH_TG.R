# Declarar as bibliotecas ------------------------------------------------------------------------------------------

library(signal); library(oce); library(lubridate); library(ggplot2); library(epitools); library(oce)

# Arquivo de dados maregráficos (2002-2016)--------------------------------------------------------------------------
# Formato de entrada: Dia/Mês/Ano  Hora:Minuto:Segundo   SSH (m)-----------------------------------------------------
# Sem cabeçalho------------------------------------------------------------------------------------------------------

file_TG="Link do diretório dos dados maregráficos (.txt)"

v_TG <-read.table(file_TG, header = FALSE, sep = "", dec = ".")
nl_TG=nrow(v_TG['V1'])
ti_TG=strptime(v_TG[1,'V1']:v_TG[1,'V2'],format='%d/%m/%Y:%H:%M:%S')
tf_TG=strptime(v_TG[nl_TG,'V1']:v_TG[nl_TG,'V2'],format='%d/%m/%Y:%H:%M:%S')
x_TG<-as.POSIXct(c(ti_TG,tf_TG), tz="UTC")
tempo_TG<-seq(x_TG[1], x_TG[2], by = "5 min")
tempo_TG_1hora<- julian(tempo_TG, origin = strptime('01/01/2000:12:00:00',format='%d/%m/%Y:%H:%M:%S',tz = 'UTC'))
elev_TG <- as.numeric(unlist(v_TG['V5']))

# Buscar a localização do tempo inicial da série maregráfica "tempo"  na lista de SHH "tempo_list" com precisão de "preci"---------------------------------------------------------------------------------------------------------

buscar_loc_mais<- function (tempo,tempo_list,preci) {
  Val=ceiling(tempo) #arredondar para cima
  loc=match(c(Val,Val+1,Val+2,Val+3,Val+4,Val+5,Val+6,Val+7,Val+8,Val+9,Val+10,
              Val+11,Val+12,Val+13,Val+14,Val+15,Val+16,Val+17,Val+18,Val+19,Val+20,
              Val+21,Val+22,Val+23,Val+24,Val+25,Val+26,Val+27,Val+28,Val+29,Val+30,
              Val+31,Val+32,Val+33,Val+34,Val+35,Val+36,Val+37,Val+38,Val+39,Val+40,
              Val+41,Val+42,Val+43,Val+44,Val+45,Val+46,Val+47,Val+48,Val+49,Val+50,
              Val+51,Val+52,Val+53,Val+54,Val+55,Val+56,Val+57,Val+58,Val+59,Val+60,
              Val+61,Val+62,Val+63,Val+64,Val+65,Val+66,Val+67,Val+68,Val+69,Val+70),
            round(tempo_list,preci))
  loc1=loc[!is.na(loc)][1]
  return(loc1)}

# Buscar a localização do tempo final da série maregráfica "tempo"  na lista de SHH "tempo_list" com precisão de "preci"---------------------------------------------------------------------------------------------------------

buscar_loc_menos<- function (tempo,tempo_list,preci) {
  Val=floor(tempo) #arredondar para baixo
  loc=match(c(Val,Val-1,Val-2,Val-3,Val-4,Val-5,Val-6,Val-7,Val-8,Val-9,Val-10,
              Val-11,Val-12,Val-13,Val-14,Val-15,Val-16,Val-17,Val-18,Val-19,Val-20,
              Val-21,Val-22,Val-23,Val-24,Val-25,Val-26,Val-27,Val-28,Val-29,Val-30,
              Val-31,Val-32,Val-33,Val-34,Val-35,Val-36,Val-37,Val-38,Val-39,Val-40,
              Val-41,Val-42,Val-43,Val-44,Val-45,Val-46,Val-47,Val-48,Val-49,Val-50,
              Val-51,Val-52,Val-53,Val-54,Val-55,Val-56,Val-57,Val-58,Val-59,Val-60,
              Val-61,Val-62,Val-63,Val-64,Val-65,Val-66,Val-67,Val-68,Val-69,Val-70),
            round(tempo_list,preci))
  loc1=loc[!is.na(loc)][1]
  return(loc1)}

# Buscar a localização do tempo final da série maregráfica "tempo"  na lista de SHH "tempo_list" com precisão de "preci"---------------------------------------------------------------------------------------------------------


buscar_aproximar<- function (tempo,tempo_list) {
  Val=tempo
  loc=match(round(c(Val,Val-0.001,Val+0.001,Val-0.002,Val+0.002,Val-0.003,Val+0.003,Val-0.004,Val+0.004,Val-0.005,Val+0.005,Val-0.006,
                    Val+0.006,Val-0.007,Val+0.007,Val-0.008,Val+0.008,Val-0.009,Val+0.009,Val-0.010,Val+0.010,Val-0.011,Val+0.011,
                    Val-0.012,Val+0.012,Val-0.013,Val+0.013,Val-0.014,Val+0.014,Val-0.015,Val+0.015,Val-0.016,Val+0.016,Val-0.017,
                    Val+0.017,Val-0.018,Val+0.018,Val-0.019,Val+0.019,Val-0.020,Val+0.020,Val-0.021,Val+0.021,Val-0.022,Val+0.022,
                    Val-0.023,Val+0.023,Val-0.024,Val+0.024,Val-0.025,Val+0.025,Val-0.026,Val+0.026,Val-0.027,Val+0.027,Val-0.028,
                    Val+0.028,Val-0.029,Val+0.029,Val-0.030,Val+0.030,Val-0.031,Val+0.031,Val-0.032,Val+0.032,Val-0.033,Val+0.033,
                    Val-0.034,Val+0.034,Val-0.035,Val+0.035,Val-0.036,Val+0.036,Val-0.037,Val+0.037,Val-0.038,Val+0.038,Val-0.039,
                    Val+0.039,Val-0.040,Val+0.040,Val-0.041,Val+0.041,Val-0.042,Val+0.042,Val-0.043,Val+0.043,Val-0.044,Val+0.044,
                    Val-0.045,Val+0.045,Val-0.046,Val+0.046,Val-0.047,Val+0.047,Val-0.048,Val+0.048,Val-0.049,Val+0.049,Val-0.050,
                    Val+0.050,Val-0.051,Val+0.051,Val-0.052,Val+0.052,Val-0.053,Val+0.053,Val-0.054,Val+0.054,Val-0.055,Val+0.055,
                    Val-0.056,Val+0.056,Val-0.057,Val+0.057,Val-0.058,Val+0.058,Val-0.059,Val+0.059,Val-0.060,Val+0.060,Val-0.061,
                    Val+0.061,Val-0.062,Val+0.062,Val-0.063,Val+0.063,Val-0.064,Val+0.064,Val-0.065,Val+0.065,Val-0.066,Val+0.066,
                    Val-0.067,Val+0.067,Val-0.068,Val+0.068,Val-0.069,Val+0.069,Val-0.070,Val+0.070,Val-0.071,Val+0.071,Val-0.072,
                    Val+0.072,Val-0.073,Val+0.073,Val-0.074,Val+0.074,Val-0.075,Val+0.075,Val-0.076,Val+0.076,Val-0.077,Val+0.077,
                    Val-0.078,Val+0.078,Val-0.079,Val+0.079,Val-0.080,Val+0.080,Val-0.081,Val+0.081,Val-0.082,Val+0.082,Val-0.083,
                    Val+0.083,Val-0.084,Val+0.084,Val-0.085,Val+0.085,Val-0.086,Val+0.086,Val-0.087,Val+0.087,Val-0.088,Val+0.088,
                    Val-0.089,Val+0.089,Val-0.090,Val+0.090,Val-0.091,Val+0.091,Val-0.092,Val+0.092,Val-0.093,Val+0.093,Val-0.094,
                    Val+0.094,Val-0.095,Val+0.095,Val-0.096,Val+0.096,Val-0.097,Val+0.097,Val-0.098,Val+0.098,Val-0.099,Val+0.099),3),round(tempo_list,3))
  loc1=loc[!is.na(loc)][1]
  return(loc1)}

# Função do filtro 3 sigma--------------------------------------------------------------------------------------------
answer = c()
teste_tres_sigma<- 
  function (elev,sigma) {
    des_padrao<- sd(na.omit(elev))
    media<- mean(na.omit(elev))
    teste_menos=media-sigma*des_padrao
    teste_mais=media+sigma*des_padrao
    answer = c()
    for (val in elev) {
      tes=(is.na(val))
      if(tes==TRUE){
        t=NA
      } 
      else{
        if(val < teste_mais & val>teste_menos){
          t=val
        } 
        else{
          t=NA
          
        }}
      answer <- append(answer, t)
    }
    return(answer)}

#Arquivo de dados de altimetria por satelite-------------------------------------------------------------------------------------

correla<-c(); media_lat<-c(); media_long<-c(); v_passe<-c(); v_file<-c(); mmAno<-c();mmAno2007_2015<-c();num_ele<-c();num_sem_nan<-c()
num_sem_nan_2002_2015=c();num_ele_2002_2015=c();mmAno_TGe=c()

dir<-'Link do diretório dos dados de SSH dispostos em células (.txt)'

file1 <- list.files(dir)
for(f in file1){
  dir2=(paste(dir, fsep = .Platform$file.sep, f, sep = "")) 
  file2<- list.files(dir2)
  for(i in file2) {
    dir3=(paste(dir2, fsep = .Platform$file.sep, i, sep = "")) 
    v_AltSat <-read.table(dir3, header = FALSE, sep = "", dec = ".")[-1,] #[-1,] = não ler primeira linha
    nl_AltSat=nrow(v_AltSat['V1'])
   
    tempo_AltSat <-as.numeric(as.character(unlist(v_AltSat['V1'])))
    lat_AltSat <-as.numeric(as.character(unlist(v_AltSat['V2'])))
    long_AltSat <-as.numeric(as.character(unlist(v_AltSat['V3'])))
    elev_AltSat <-as.numeric(as.character(unlist(v_AltSat['V4'])))
    mlat=mean(na.omit(lat_AltSat))
    mlong=mean(na.omit(long_AltSat))
    
    # Intervalo de dados SSH compatíveis com os dados maregráficos ---------------------------------------------------------------
    
    loc_AltSat_i1=buscar_loc_mais(tempo_TG_1hora[1],tempo_AltSat,0)
    loc_AltSat_f1=buscar_loc_menos(tempo_TG_1hora[length(tempo_TG_1hora)],tempo_AltSat,0)
    
    tempo_AltSat_2=tempo_AltSat[loc_AltSat_i1:loc_AltSat_f1]
    lat_AltSat_2=lat_AltSat[loc_AltSat_i1:loc_AltSat_f1]
    long_AltSat_2=long_AltSat[loc_AltSat_i1:loc_AltSat_f1]
    elev_AltSat_2=elev_AltSat[loc_AltSat_i1:loc_AltSat_f1]
    
    # Compatibilizar o tempo inicial e final entre a série de dados maregráficos de Imbituba-SC e os limites de tempo dos 
    # dados SSH de cada célula-----------------------------------------------------------------------------------------------------
    
    loc_TG_f1=buscar_aproximar(tempo_AltSat_2[length(tempo_AltSat_2)],tempo_TG_1hora)
    loc_TG_i1=buscar_aproximar(tempo_AltSat_2[1],tempo_TG_1hora)
    tempo_TG_2=tempo_TG_1hora[loc_TG_i1:loc_TG_f1]
    elev_TG_2=elev_TG[loc_TG_i1:loc_TG_f1]
    
    # Aplicar a função resample para compatibilizar interalo de tempo entre dados maregráficos e os valores de SSHs-------------
    
    valor_unitario= 1
    intevalo_tempo_serie=9.9156*24*12
    
    elev_TG_inter <- resample(elev_TG_2, valor_unitario, intevalo_tempo_serie)
    tempo_TG_inter <- seq(as.numeric(tempo_TG_2[1]),as.numeric(tempo_TG_2[length(tempo_TG_2)]), by = 9.9156)
    elev_TG_intervalo <-elev_TG_inter[2:length(elev_TG_inter)]
    tempo_TG_intervalo <- tempo_TG_inter[2:length(elev_TG_inter)]
    
    # Retirar os últimos valores de cada lado devido a função resample ------------------------------------------------------
    
    tempo_AltSat_intervalo=tempo_AltSat_2[2:length(elev_TG_inter)]
    lat_AltSat_intervalo=lat_AltSat_2[2:length(elev_TG_inter)]
    long_AltSat_intervalo=long_AltSat_2[2:length(elev_TG_inter)]
    elev_AltSat_intervalo=elev_AltSat_2[2:length(elev_TG_inter)]
    
    # Aplicar filtro 3 sigma nos dados de SSH entre 1992-2017-------------------------------------------------------------------

    elev_AltSat_intervalo<-teste_tres_sigma(elev_AltSat_intervalo,3)

    # Cálculo da correlação entre os dados maregráficos e os dados de SSH ---------------------------------------------------------

    correl=cor(elev_TG_intervalo, elev_AltSat_intervalo,  method = c("pearson"), use="complete.obs")
    
    # Regressão dos dados de SSH entre 2002-2016----------------------------------------------------------------------------------

    rl_Alt_2002_2015=lm(elev_AltSat_intervalo~tempo_AltSat_intervalo)
    val=summary(rl_Alt_2002_2015)$coef
    
    # Caso específico------------------------------------------------------------------------------------------------------
    # if (i=="45.txt" & f=="0163"){
    #   mm_2002_2015_u= val[1]+val[2]*as.numeric(tempo_AltSat_intervalo[length(tempo_AltSat_intervalo)-2]+9.9156+9.9156)}
    # else{
    #     mm_2002_2015_u= val[1]+val[2]*as.numeric(tempo_AltSat_intervalo[length(tempo_AltSat_intervalo)])}
    # if (i=="72.txt" & f=="0163"){
    #   mm_2002_2015= val[1]+val[2]*as.numeric(tempo_AltSat_intervalo[2]-9.9156)}
    # else{
    #     mm_2002_2015= val[1]+val[2]*as.numeric(tempo_AltSat_intervalo[1])}
    # Caso específico-------------------------------------------------------------------------------------------------------

    mm_2002_2015= val[1]+val[2]*as.numeric(tempo_AltSat_intervalo[1])
    mm_2002_2015_u= val[1]+val[2]*as.numeric(tempo_AltSat_intervalo[length(tempo_AltSat_intervalo)])
    mm_ano_2007_2015=(mm_2002_2015_u-mm_2002_2015)/(length(tempo_AltSat_intervalo)/(360/9.9156))*1000
    
    # Aplicar o filtro 3 sigma nos dados de SSH entre 1992-2017-----------------------------------------------------------------

    elev_AltSat<-teste_tres_sigma(elev_AltSat,3)
    
    # Regressão dos dados de SSH entre 1992-2017---------------------------------------------------------------------------------

    rl_Alt=lm(elev_AltSat~tempo_AltSat)
    val=summary(rl_Alt)$coef
    m_por_ano_Alt= val[1]+val[2]*as.numeric(tempo_AltSat[1])
    m_por_ano_Alt_u= val[1]+val[2]*as.numeric(tempo_AltSat[length(tempo_AltSat)])
    mm_ano=(m_por_ano_Alt_u-m_por_ano_Alt)/(length(tempo_AltSat)/(360/9.9156))*1000
    
    # Regressão dos dados maregráficos entre 2002-2016----------------------------------------------------------------------------

    rl_TG=lm(elev_TG_intervalo~tempo_TG_intervalo)
    val=summary(rl_TG)$coef
    m_por_ano_TG= val[1]+val[2]*as.numeric(tempo_TG_intervalo[1])
    m_por_ano_TG_u= val[1]+val[2]*as.numeric(tempo_TG_intervalo[length(tempo_TG_intervalo)])
    mm_ano_TG=(m_por_ano_TG_u-m_por_ano_TG)/(length(tempo_TG_intervalo)/(360/9.9156))*1000

    # Preparar os dados para salvar------------------------------------------------------------------------------------------------

    correla<-append(correla, correl)
    media_lat<-append(media_lat, mlat)
    media_long<-append(media_long, mlong)
    v_passe<-append(v_passe, f)
    v_file<-append(v_file, i)
    mmAno<-append(mmAno, mm_ano)
    mmAno2007_2015<-append(mmAno2007_2015, mm_ano_2007_2015)
    num_ele<-append(num_ele, length(tempo_AltSat))
    num_sem_nan<-append(num_sem_nan,length(na.omit(tempo_AltSat)))
    num_ele_2002_2015<-append(num_ele_2002_2015, length(tempo_AltSat_intervalo))
    num_sem_nan_2002_2015<-append(num_sem_nan_2002_2015,length(na.omit(tempo_AltSat_intervalo)))
    mmAno_TGe<-append(mmAno_TGe,mm_ano_TG)
  }
}

# Salvar os dados-------------------------------------------------------------------------------------------------------------------

df <- data.frame(coef_corr=correla, Lat=media_lat, Long=media_long, passe = v_passe, file=v_file,mm_ano1992_2017=mmAno, 
                  mmAno2007_2015=mmAno2007_2015,mmAno_TGe=mmAno_TGe, num_ele=num_ele,num_sem_nan=num_sem_nan,
                  num_ele_2002_2015=num_ele_2002_2015,num_sem_nan_2002_2015=num_sem_nan_2002_2015)
write.table(df,'Link do diretório para salvar os dados (.txt)', row.names = FALSE)

