# Declarar bibliotecas--------------------------------------------------------------------------------------------

import os
import utm
import heapq  
import netCDF4
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from geographiclib.geodesic import Geodesic
from math import isnan

# Definir os parâmetros do elipsoide TOPEX --------------------------------------------------------------------------

a=6378136.300000 
b=6356751.600563
f=(a-b)/a
geod = Geodesic(a, f)

# Inserir os passes ------------------------------------------------------------------------------------------------

for escolha_passe in ['0239', '0228', '0163', '0152', '0076', '0050']:
    print(escolha_passe)
    
    # Abrir os arquivos com extensão .nc e ler os dados de altimetria por satélite-----------------------------------
    
    def dados(num):
        jday=[]
        jlon=[]
        jlat=[]
        jssh=[]
        missions=os.listdir('Link do diretório dos dados de altimetria por satélite')
        for i in range(0,len(missions),1):
            print('Missão escolhida:',missions[i])
            link='Link do diretório dos dados de altimetria por satélite/%s'%(missions[i])
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
                        ssh=nc.variables['ssh.33'][:] #metros
                        jday=np.append(jday,day)
                        jlon=np.append(jlon,lon)
                        jlat=np.append(jlat,lat)
                        jssh=np.append(jssh,ssh)             
                    dados=np.concatenate([[jday],[jlat],[jlon],[jssh]])
        return dados
    ss=dados(escolha_passe)
    
    def longi(lo):
        if (lo>0):
            longitu=abs(lo)-360
        else:
            longitu=lo
        return longitu
   
    #Calcular a distância entre a latitude/longitude máxima e mínima (geodésica)-----------------------------------------

    def max_min(list_lat,list_long):
        loc_lat_mx=np.where(list_lat==max(list_lat))
        lat_mx=max(list_lat)
        lon_mx=list_long[loc_lat_mx]
        loc_lat_min=np.where(list_lat==min(list_lat))
        lat_min=min(list_lat)
        lon_min=list_long[loc_lat_min]
        return lat_mx,lon_mx[0],lat_min,lon_min[0]
    
    lat1,lon1,lat2,lon2=max_min(ss[1],ss[2])
    di_mx=(geod.Inverse(lat1,lon1,lat2,lon2)['s12'])/1000
    
    ab=[]
    for i in range(0,len(ss[1]),1):
        abso=((ss[1][i])**2+(ss[2][i])**2)**(1/2)
        ab.append(abso)
    
    bi=round(di_mx/7)
    # bins é o intervalo de latitudes e a "density" é a frequencia desse intervalo------------------------------------------
    hist = plt.hist(ss[1], bins=bi, normed=True)
    density, bins, patches = hist
    maiores=heapq.nlargest(bi,density)
    
    def remove_colchetes(list1):
        return str(list1).replace('[','').replace(']','')
    
    # Posição dos maiores valores---------------------------------------------------------------------------------------------
    
    loc_maiores=[]
    for i in maiores:
        loc_mx=np.where(density==i)[0]
        loc_maiores.append(loc_mx.tolist())
    loc=np.unique(remove_colchetes(loc_maiores).split(","))
    
    # Cálculo da latitude e longitude média das maiores densidades---------------------------------------------------------
    
    med_lat=[]
    med_lon=[]
    for i in loc:
        loc1=bins[(int(i)-1)]
        loc2=bins[int(i)]
        dif=loc1-loc2
        g=np.where(np.logical_and(ss[1]>=loc1, ss[1]<=loc2))
        media_lati=np.mean(ss[1][g])
        media_long=np.mean(ss[2][g])
        med_lat.append(media_lati)
        med_lon.append(media_long)
    
    m_lat=heapq.nlargest(len(med_lat),med_lat)
    llat=[]
    llon=[]
    for i in m_lat:
        if not isnan(i):
            loc3=np.where(med_lat==i)[0][0]
            llat.append(i)
            llon.append(med_lon[loc3])
    
    # Excluir os pontos localizados a uma distancia maior que 500 km do maregrafo de Imbituba-SC----------------------------
    
    longg=[]
    latii=[]
    for i in range(0,len(llat),1):
        lat_imb=-(28+13/60+52.30/3600)
        long_imb=360-(48+39/60+2.06/3600)
        dist_pto_MG=(geod.Inverse(llat[i],llon[i],lat_imb,long_imb)['s12'])/1000
        if dist_pto_MG<=500:
            latii.append(llat[i])
            longg.append(llon[i])

    # Verificar se um determinado ponto está dentro ou fora de um retângulo (ou célula)---------------------------------------
    # Referência: https://www.geeksforgeeks.org/check-whether-given-point-lies-inside-rectangle-not/--------------------------

    def area(x1, y1, x2, y2, x3, y3): 
        return abs((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2.0) 
    
    def check(x1, y1, x2, y2, x3,y3, x4, y4, x, y): 
        
        # Calculate area of rectangle ABCD  
        A = (area(x1, y1, x2, y2, x3, y3) + area(x1, y1, x4, y4, x3, y3)) 
    
        # Calculate area of triangle PAB  
        A1 = area(x, y, x1, y1, x2, y2) 
               
        # Calculate area of triangle PBC  
        A2 = area(x, y, x2, y2, x3, y3) 
                  
        # Calculate area of triangle PCD  
        A3 = area(x, y, x3, y3, x4, y4) 
                  
        # Calculate area of triangle PAD  
        A4 = area(x, y, x1, y1, x4, y4); 
        return (round(A,0) == round(A1 + A2 + A3 + A4,0)) 
    
    #  Ordenar as latitudes em ordem decrescente--------------------------------------------------------------------------------
            
    def decre(lat,long):
        latt=sorted(lat)
        lon=[]
        for i in sorted(latt):
            loc=np.where(i==lat)[0][0]
            lon.append(long[loc])        
        return latt,lon
    latii,longg=decre(latii,longg)
    
    # Remover valores duplicados com base na data-------------------------------------------------------------------------------
    
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
            if len(loc)==1:
                loc1=loc[0]
            if len(loc)==2:
                # Utilizar a menor distância até o ponto médio quando ocorrer sobreposição de datas
                d0=(geod.Inverse(lat[loc[0]],lon[loc[0]],np.mean(lat),np.mean(lon))['s12'])
                d1=(geod.Inverse(lat[loc[1]],lon[loc[1]],np.mean(lat),np.mean(lon))['s12'])
                if d0<d1:
                    loc1=loc[0]
                if d1<d0:
                    loc1=loc[1]
            if len(loc)>=3:
                # Utilizar a menor distância até o ponto médio quando ocorrer sobreposição de datas
                d0=(geod.Inverse(lat[loc[0]],lon[loc[0]],np.mean(lat),np.mean(lon))['s12'])
                d1=(geod.Inverse(lat[loc[1]],lon[loc[1]],np.mean(lat),np.mean(lon))['s12'])
                d2=(geod.Inverse(lat[loc[2]],lon[loc[2]],np.mean(lat),np.mean(lon))['s12'])
                if d0<d1 and d0<d2:
                    loc1=loc[0]
                if d1<d0 and d1<d2:
                    loc1=loc[1]
                if d2<d0 and d2<d1:
                    loc1=loc[2]            
                    
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
        return [x for x in day_p if x != 'NaN'],[x for x in lat_p if x != 'NaN'],[x for x in lon_p if x != 'NaN'],[x for x in ssh_p if x != 'NaN']
       
    for i in range(1,len(latii),1):
        d_meio_borda=(1.5**2+3.5**2)**(1/2)
        
        # Calcular os Azimutes------------------------------------------------------------------------------------------------------
      
        A1=geod.Inverse(latii[i-1],longg[i-1],latii[i],longg[i])['azi1']        
        dis=geod.Inverse(latii[i-1],longg[i-1],latii[i],longg[i])['s12']

        if A1<0:
            Az1=360+A1+23.2
            Az2=Az1+66.8+66.8
            Az3=Az2+23.2+23.2
            Az4=Az3+66.8+66.8
        else:
             Az1=23.2+A1
             Az2=Az1+66.8+66.8
             Az3=Az2+23.2+23.2
             Az4=Az3+66.8+66.8
        
        # Calcular as latitudes e longitudes dos vértices das células ---------------------------------------------------------------
        
        la1 = geod.Direct(latii[i-1],longg[i-1], Az1, d_meio_borda * 1000)['lat2']
        lo1 = geod.Direct(latii[i-1],longg[i-1], Az1, d_meio_borda * 1000)['lon2']+360
        
        la2 = geod.Direct(latii[i-1],longg[i-1], Az2, d_meio_borda * 1000)['lat2']
        lo2 = geod.Direct(latii[i-1],longg[i-1], Az2, d_meio_borda * 1000)['lon2']+360
        
        la3 = geod.Direct(latii[i-1],longg[i-1], Az3, d_meio_borda * 1000)['lat2']
        lo3 = geod.Direct(latii[i-1],longg[i-1], Az3, d_meio_borda * 1000)['lon2']+360
        
        la4 = geod.Direct(latii[i-1],longg[i-1], Az4, d_meio_borda * 1000)['lat2']
        lo4 = geod.Direct(latii[i-1],longg[i-1], Az4, d_meio_borda * 1000)['lon2']+360
        
        la_max=max(la1,la2,la3,la4)
        la_min=min(la1,la2,la3,la4)
        
        lo_max=max(lo1,lo2,lo3,lo4)
        lo_min=min(lo1,lo2,lo3,lo4)
        
        # Delimitar uma área com base na latitudes e longitudes máximas e mínimas para não necessitar fazer a análise da relação de áreas para 		todos os pontos de um passe. Isso economizará tempo de processamento -----------------------------------------------------------

        inter_la=np.where(np.logical_and(ss[1]>=la_min, ss[1]<=la_max))
        inter_lo=np.where(np.logical_and(ss[2]>=lo_min, ss[2]<=lo_max))
    
        loc=np.intersect1d(inter_la[0],inter_lo[0])
        lat_p=[]
        long_p=[]
        day_p=[]
        ssh_p=[] 
        
        #Converter a latitude e longitude em coordenadas UTM para realizar a relação de área e ver se um determinado ponto P está ou não 
	# dentro do retângulo ou célula  ----------------------------------------------------------------------------------------------
        lat_nao=[]
        long_nao=[]
        for k in range(0,len(loc),1):
            la_p=ss[1][loc[k]]
            lo_p=ss[2][loc[k]]
            la1y=utm.from_latlon(la1, lo1-360)[0]
            lo1x=utm.from_latlon(la1, lo1-360)[1]
            la2y=utm.from_latlon(la2, lo2-360)[0]
            lo2x=utm.from_latlon(la2, lo2-360)[1]
            la3y=utm.from_latlon(la3, lo3-360)[0]
            lo3x=utm.from_latlon(la3, lo3-360)[1]
            la4y=utm.from_latlon(la4, lo4-360)[0]
            lo4x=utm.from_latlon(la4, lo4-360)[1]
            la_py=utm.from_latlon(la_p, lo_p-360)[0]
            lo_px=utm.from_latlon(la_p, lo_p-360)[1]
            
            lat_nao.append(la_py)
            long_nao.append(lo_px)
               
            if (check(la1y, lo1x, la2y, lo2x,la3y, lo3x,la4y, lo4x,la_py,lo_px)):
                   lat_p.append(la_p)
                   long_p.append(lo_p)
                   local=np.where(np.logical_and(ss[1]==la_p, ss[2]==lo_p))
                   day_p.append((ss[0][local])[0])
                   ssh_p.append((ss[3][local])[0])
                   
                   if len(ssh_p)>=500:
                       day_p,lat_p,long_p,ssh_p=process(day_p,lat_p,long_p,ssh_p)

	# Salvar-----------------------------------------------------------------------------------------------------------

        col_data={'Day': day_p,'Lat': lat_p,'Long': long_p,'SSH':ssh_p}
        text= pd.DataFrame(data=col_data)
        pd.set_option("display.precision", 14)
        pd.set_option('max_rows', len(text))
        text = text[['Day', 'Lat', 'Long','SSH']]
        text=text.to_string(index=False)
	# O link do diretório para salvar os dados consiste de uma pasta que contém subpastas vazias nomeadas com os números 
        # dos passes empregados---------------------------------------------------------------------------------------------
        textfile='Link do diretório para salvar os dados/%s/%s.txt'%(escolha_passe,i)
        np.savetxt(textfile, np.column_stack(text),delimiter='', fmt='%.4s')
