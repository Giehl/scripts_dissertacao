# Declarar bibliotecas--------------------------------------------------------------------------------------------

import os
import bz2
import math
import datetime
import numpy as np
import pandas as pd
from datetime import date
from netCDF4 import Dataset
from calendar import monthrange
from pykrige.ok import OrdinaryKriging
from datetime import datetime as real_datetime

# Abrir os arquivos com extensão .bz---------------------------------------------------------------------------------
# Referência: https://barbados.mpimet.mpg.de/bcoweb/systems/BCO_python_doc/_modules/BCO/tools/tools.html#bz2Dataset

for passe in ['0239', '0228', '0163','0152', '0076', '0050']: 
    def bz2Dataset(bz2file):
        package_directory = os.path.dirname(os.path.abspath(__file__))
        bz2Obj = bz2.BZ2File(bz2file)
        try:
            dummy_nc_file = package_directory + "/dummy_nc_file.nc"
            nc = Dataset(dummy_nc_file,memory=bz2Obj.read())
        except: 
            print("This function only works with netCDF-4 Datasets.")
            print("If the datamodel of your netcdf file is e.g 'classic' instead of" +
                  " 'netCDF-4' it will break.")
            dummy_nc_file = package_directory + "/MRR__CIMH__LWC__60s_100m__20180520.nc"
            nc = Dataset(filename=dummy_nc_file,mode="r", memory=bz2Obj.read())
        return nc
    
    # Encontrar a grade do modelo que envolve a corrdenada de entrada----------------------------------------------------
    
    def encontrar(latitu,longitu,ncbz):  
        np.random.seed(89239413)
        nc = bz2Dataset(ncbz)
        lati=nc.variables['latitude'][:]
        long=nc.variables['longitude'][:]
        dac=nc.variables['dac']
        data=dac.date
        laa=(np.where(lati==latitu))[0][0]
        loo=(np.where(long==longitu))[0][0]
        dac=dac[laa][loo]
        return data,latitu,longitu,dac
    
    # Aproximar as corrdenadas de entrada com base no intervalo correspondente a resolução espacial do modelo DAC (0,25°x0,25°)
    
    def x_round(x):
        proxi1=round(x*4)/4
        delta=proxi1-x
        if delta>0:
            proxi2=proxi1-0.25
        else:
            proxi2=proxi1+0.25
        return proxi1,proxi2
    
    # Confeccionar os grids com a mesma resolução espacial do modelo-------------------------------------------------------

    def fazer_grid(latt, longg):
        la=x_round(latt)
        lat=np.array([max(la),min(la)])
        lo=x_round(longg)
        long=np.array([min(lo),max(lo)])
        X,Y = np.meshgrid(long,lat)
        return X,Y
    
    # Aplicar a Krigagem Ordinária (baseada na média ponderada)------------------------------------------------------------
    
    def modelo(latt,longg,ncbz):
        glo,gla=fazer_grid(latt,longg)
        Long=[]
        Lat=[]
        Dac=[]
        Data=[]
        for i in range(0,len(gla),1):
            for j in range(0,len(gla),1):
                val=encontrar(gla[i][j],glo[i][j],ncbz)
                Data.append(val[0])
                Lat.append(val[1])
                Long.append(val[2])
                Dac.append(val[3])
                
        # Quando os quatro nós apresentam o mesmo valor, a Krigagem não ocorre ----------------------------------------
        
        if Dac[0]==Dac[1] and Dac[0]==Dac[2] and Dac[0]==Dac[3]:
            z_dac=Dac[0]
        else:
            OK_re = OrdinaryKriging(Long, Lat, Dac, variogram_model='linear', verbose=False,
                             enable_plotting=False, coordinates_type='geographic')
            z_dac = OK_re.execute('points',longg,latt)[0][0]
        return latt,longg,z_dac
        
    # Converter a data Juliana em Dia/Mês/Ano Hora:Minuto:Segundo---------------------------------------------------------
    # Referência: http://opendap.ccst.inpe.br/Observations/ARGO/tmp/netCDF4-0.9.8/netcdftime.py -------------------------
        
    def DateFromJulianDate(JD,calendar='standard'):
        """
    Algorithm:
    Meeus, Jean (1998) Astronomical Algorithms (2nd Edition). Willmann-Bell,
    Virginia. p. 63
        """      
        if JD < 0:
            raise ValueError('Julian Day must be positive')
    
        dayofwk = int(math.fmod(int(JD + 1.5),7))
        (F, Z) = math.modf(JD + 0.5)
        Z = int(Z)
        if calendar in ['standard','gregorian']:
            if JD < 2299160.5:
                A = Z
            else:
                alpha = int((Z - 1867216.25)/36524.25)
                A = Z + 1 + alpha - int(alpha/4)
    
        elif calendar == 'proleptic_gregorian':
            alpha = int((Z - 1867216.25)/36524.25)
            A = Z + 1 + alpha - int(alpha/4)
        elif calendar == 'julian':
            A = Z
        else:
            raise ValueError('unknown calendar, must be one of julian,standard,gregorian,proleptic_gregorian, got %s' % calendar)
        B = A + 1524
        C = int((B - 122.1)/365.25)
        D = int(365.25 * C)
        E = int((B - D)/30.6001)
        # Convert to date
        day = B - D - int(30.6001 * E) + F
        nday = B-D-123
        if nday <= 305:
            dayofyr = nday+60
        else:
            dayofyr = nday-305
        if E < 14:
            month = E - 1
        else:
            month = E - 13
        if month > 2:
            year = C - 4716
        else:
            year = C - 4715
        # a leap year?
        leap = 0
        if year % 4 == 0:
            leap = 1
        if calendar == 'proleptic_gregorian' or \
           (calendar in ['standard','gregorian'] and JD >= 2299160.5):
            if year % 100 == 0 and year % 400 != 0: 
                leap = 0
        if leap and month > 2:
           dayofyr = dayofyr + leap
        # Convert fractions of a day to time    
        (dfrac, days) = math.modf(day/1.0)
        (hfrac, hours) = math.modf(dfrac * 24.0)
        (mfrac, minutes) = math.modf(hfrac * 60.0)
        seconds = round(mfrac * 60.0) # seconds are rounded
        if seconds > 59:
            seconds = 0
            minutes = minutes + 1
        if minutes > 59:
            minutes = 0
            hours = hours + 1
        if hours > 23:
            hours = 0
            days = days + 1
        daysinmonth = monthrange(year, month)[1]
        if days > daysinmonth: 
            days = 1
            month = month + 1
            if month > 12:
                month = 1
                year = year + 1
        # return a 'real' datetime instance if calendar is gregorian.
        if calendar == 'proleptic_gregorian' or \
                (calendar in ['standard','gregorian'] and JD >= 2299160.5):
            return real_datetime(year,month,int(days),int(hours),int(minutes),int(seconds))
        else:
        # or else, return a 'datetime-like' instance.
            return datetime(year,month,int(days),int(hours),int(minutes),int(seconds),dayofwk,dayofyr)
    
    # Função para ler arquivo .txt ----------------------------------------------------------------------------------------
    # Formato de entrada: Dia/Mês/Ano  Hora:Minuto:Segundo   SSH (m)------------------------------------------------------
    # A primeira linha deve conter o Cabeçalho----------------------------------------------------------------------------
                
    def Read_TXT_File(file_name):
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
        return day,dlat,dlong,ssh
    
    def x_round2(x, base=6):
        return base * round(x/base)
    
    # Ler o arquivo .txt e remover os efeitos de Barômetro Inverso --------------------------------------------------------
    # Cada célula consiste de um arquivo .txt que está hospedada dentro de uma pasta nomeada com o número do passe---------
    
    diretorio='Link do diretório que contem as células/%s'%(passe)
    celulas=os.listdir(diretorio)
    for i in range(0,(len(celulas)),1):   
        file='%s/%s'%(diretorio,celulas[i])
        day, dlat, dlong,ssh = Read_TXT_File(file)
        clon=[]
        clat=[]
        cssh=[]
        cday=[]
        for j in range(0,len(day),1):
            # Dias Julianos em Data Juliana = 2451545 + day[j] (referência: 01/01/2000 12:00:00) -------------------------
            data=(DateFromJulianDate(2451545+day[j]))
            i_data = date(int(data.year),int(data.month),int(data.day))
            f_data = date(1992,9,1)
            delta = ((i_data - f_data).days)+15584
            # Determinar o horário mais próximo entre a observação de SSH e do modelo DAC (00:00, 6:00, 12:00, 18:00 UTC)--
            hora_base6=x_round2(data.hour+data.minute/60+data.second/3600)
            if hora_base6==0:
                hora_base6='00'
            if hora_base6==24:
                hora_base6='00'
            elif hora_base6==6:
                 hora_base6='06'
            # Modelo DAC (Dynamic Atmospheric Correction) é distribuído pela AVISO --------------------------------------
            ncbz = 'Link do diretório do Modelo_DAC /%s/dac_dif_%s_%s.nc.bz2'%(data.year,delta,hora_base6)
            ddlat,ddlong,ddac=modelo(dlat[j],dlong[j],ncbz)
            cday.append(day[j])
            clat.append(ddlat)
            clon.append(ddlong)
            cssh.append(ssh[j]+ddac)
        
        # Salvar ----------------------------------------------------------------------------------------------------------
        
        col_data={'Day': cday,'Lat': clat,'Long': clon,'SSH':cssh}
        text= pd.DataFrame(data=col_data)
        pd.set_option("display.precision", 14)
        pd.set_option('max_rows', len(text))
        text = text[['Day', 'Lat', 'Long','SSH']]
        text=text.to_string(index=False)
        # O link do diretório para salvar os dados consiste de uma pasta com subpastas vazias nomeadas com os números 
        # dos passes empregados---------------------------------------------------------------------------------------------
        textfile='Link do diretório para salvar os dados/%s/%s'%(passe,celulas[i])
        np.savetxt(textfile, np.column_stack(text),delimiter='', fmt='%.4s')