# Declarar bibliotecas--------------------------------------------------------------------------------------------

import os
import netCDF4
import numpy as np
import pandas as pd
from geographiclib.geodesic import Geodesic

# Definir os parâmetros do elipsoide TOPEX ---------------------------------------------------------------------------------------

a=6378136.300000 
b=6356751.600563
f=(a-b)/a
geod = Geodesic(a, f)

# Inserir os passes ------------------------------------------------------------------------------------------------

for escolha_passe in ['0239', '0228', '0163', '0152', '0076', '0050']:

    # Abrir os arquivos com extensão .nc e ler os dados--------------------------------------------------------------
    
    def dados_EOT(num):
        jday=[]
        jlon=[]
        jlat=[]
        jltide=[]
        jotide=[]
        missions=os.listdir('Link do diretório do Modelo EOT')
        for i in range(0,len(missions),1):
            print('Missão escolhida:',missions[i])
            link='Link do diretório do Modelo EOT/%s'%(missions[i])
            direct=os.listdir(link)
            for a in range(0,len(direct),1):
                cycle=os.listdir('%s/%s'%(link,direct[a]))     
                for b in range(0,len(cycle),1):
                    nc=netCDF4.Dataset('%s/%s/%s'%(link,direct[a],cycle[b]))
                    p_num=nc.pass_number
                    if p_num==num:
                        day=nc.variables['jday.00'][:]
                        lon=nc.variables['glon.00'][:]
                        lat=nc.variables['glat.00'][:]
                        otide=nc.variables['otide.15'][:] #metros
                        ltide=nc.variables['ltide.15'][:] #metros
                        jday=np.append(jday,day)
                        jlon=np.append(jlon,lon)
                        jlat=np.append(jlat,lat)
                        jotide=np.append(jotide,otide)        
                        jltide=np.append(jltide,ltide)  
                    dados=np.concatenate([[jday],[jlat],[jlon],[jotide],[jltide]])
        return dados
    dads=dados_EOT(escolha_passe)
    
   # Função para ler arquivo .txt ----------------------------------------------------------------------------------------
   # Formato de entrada: Dia/Mês/Ano  Hora:Minuto:Segundo   SSH (m)-------------------------------------------------------
   # A primeira linha deve conter o Cabeçalho-----------------------------------------------------------------------------
    
    def ler_txt(file_name):
        with open(file_name, 'r') as data:
            day=[]
            dlat=[]
            dlong=[]
            ssh=[]
            line = data.readline() #não ler a primeira linha
            for line in data:
                p = line.split()   
                day.append(float(p[0]))
                dlat.append(float(p[1]))
                dlong.append(float(p[2]))
                ssh.append(float(p[3]))
        return day, dlat, dlong,ssh
    
    # Inserir "NaN" nos gaps --------------------------------------------------------------------------------------------
    
    def process(day,lat,lon,ssh):
        dayy=np.array(sorted(day))
        intervalo=np.arange(dayy[0],dayy[len(dayy)-1],9.9156)
        locc=[]
        for i in intervalo:
            ti=i-4
            tf=i+4
            day=np.array(day)
            loc=np.where(np.logical_and(day>=ti, day<=tf))[0]
            if len(loc)==0:
                loc1='NaN'
            if len(loc)>=1:
                loc1=loc[0]
            locc.append(loc1)
            loc2=list(locc)
        day_p=[]
        lon_p=[]
        lat_p=[]
        ssh_p=[]
        for j in loc2:
            if j=="NaN":
                day1="NaN"
                lat1="NaN"
                lon1="NaN"
                ssh1="NaN"
            else:
                day1=day[j]
                lat1=lat[j]
                lon1=lon[j]
                ssh1=ssh[j]     
            day_p.append(day1)
            lat_p.append(lat1)
            lon_p.append(lon1)
            ssh_p.append(ssh1)          
        return day_p,lat_p,lon_p,ssh_p
        
    def remove_colchetes(list1):
        return str(list1).replace('[','').replace(']','')
    
    # Ler o arquivo .txt e remover os efeitos de Maré----------------------------------------------------------------------
    # Cada célula consiste de um arquivo .txt que está hospedada dentro de uma pasta nomeada com o número do passe---------
    # Utilizar as células nas quais os efeitos de barômetro inverso já foram removidos-------------------------------------
    
    diretorio='Link do diretório que contem as células/%s'%(escolha_passe)
    celulas=os.listdir(diretorio)
    for i in range(0,(len(celulas)),1):
        file='%s/%s'%(diretorio,celulas[i])
        day, dlat, dlong,ssh = ler_txt(file)
        clon=[]
        clat=[]
        cssh=[]
        cday=[]
        for j in range(0,len(day),1):
            dec=8
            # Idendificar as coordenadas homólogas entre o modelo EOT e as células-------------------------------------------
            loc=np.where(np.logical_and(np.around(dads[1],decimals=dec)==round(dlat[j],dec), np.around(dads[2],decimals=dec)==round(dlong[j],dec)))
            cday.append(day[j])
            clat.append(dlat[j])
            clon.append(dlong[j])
            cssh.append(ssh[j]+dads[3][loc][0]+dads[4][loc][0]) # Remover os efeitos de marés
        day_pp,lat_pp,long_pp,ssh_pp=process(cday,clat,clon,cssh)
    
	# Salvar-------------------------------------------------------------------------------------------------------------

        col_data={'Day': day_pp,'Lat': lat_pp,'Long': long_pp,'SSH':ssh_pp}
        text= pd.DataFrame(data=col_data)
        pd.set_option("display.precision", 14)
        pd.set_option('max_rows', len(text))
        text = text[['Day', 'Lat', 'Long','SSH']]
        text=text.to_string(index=False)
        # O link do diretório para salvar os dados consiste de uma pasta que contém subpastas vazias nomeadas com os números 
        # dos passes empregados------------------------------------------------------------------------------------------------
        textfile='Link do diretório para salvar os dados/%s/0%s.txt'%(escolha_passe,celulas[i])
        np.savetxt(textfile, np.column_stack(text),delimiter='', fmt='%.4s')
        
