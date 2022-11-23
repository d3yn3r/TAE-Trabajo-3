
año=2021
mes=1
dia=1
cadena=""
lista=[]
clase=['Choque','Atropello','Otro','Caida_de_Ocupante','Volcamiento','Incendio','Caida']
comunas = ['ALTAVISTA', 'ARANJUEZ', 'BELEN', 'BUENOS AIRES', 'CASTILLA', 'DOCE DE OCTUBRE', 'EL POBLADO', 'GUAYABAL', 'LA AMERICA', 'LA CANDELARIA', 'LAURELES', 'MANRIQUE', 'POPULAR', 'ROBLEDO', 'SAN ANTONIO DE PRADO', 'SAN CRISTOBAL', 'SAN JAVIER', 'SANTA CRUZ', 'SANTA ELENA', 'VILLA HERMOSA']
import csv

def festivos(fecha_actual):
    x=''
    festivos_fechas = ['2022-01-01', '2022-01-10', '2022-03-21', '2022-04-10', '2022-04-11', '2022-04-12' '2022-04-13', '2022-04-14', '2022-04-15', '2022-04-16', '2022-04-17', '2022-05-01', '2022-05-08', '2022-05-30', '2022-06-20', '2022-06-27', '2022-07-04', '2022-07-20', '2022-08-07', '2022-08-15', '2022-10-17', '2022-10-31', '2022-11-07', '2022-11-14', '2022-12-08', '2022-12-24', '2022-12-31', '2021-01-01', '2021-01-11', '2021-03-22', '2021-03-28', '2021-03-29', '2021-03-30', '2021-03-31', '2021-04-01', '2021-04-02', '2021-04-03', '2021-04-04', '2021-05-01', '2021-05-09', '2021-05-17', '2021-06-07', '2021-06-14', '2021-07-05', '2021-07-20', '2021-08-07', '2021-08-16', '2021-10-18', '2021-10-31', '2021-11-01', '2021-11-15', '2021-12-08', '2021-12-24', '2021-12-25', '2021-12-31']
    for r in festivos_fechas:
        if fecha_actual==r:
            x='SI'
            break
        else:
            x='NO'
    return x




import datetime
from datetime import date
import calendar
import locale

# Idioma "es-ES" (código para el español de España)
locale.setlocale(locale.LC_ALL, 'es-ES')
def findDay(date):
    day, month, year = (int(i) for i in date.split(' '))
    born = datetime.date(year, month, day)
    return born.strftime("%A")
lista_def=[]
date = '06 02 2019'
lista_de_listas=[]
semana=1
header=['FECHA','ANO','CLASE','COMUNA','DIA_SEMANA','SEMANA','MES','FESTIVIDAD']
while(True):
    for i in clase:
        for j in comunas:
            if mes < 10:
                if dia < 10:
                    fecha=str(año) + "-0" + str(mes) + "-0" + str(dia)
                    festivos(fecha)

                    cadena = str(año) + "-0" + str(mes) + "-0" + str(dia) + "," + str(año) + "," + i+","+j+","+str(findDay(str(dia)+" "+str(mes)+" "+str(año)))+","+str(semana)+",0" + str(mes)+","+festivos(fecha)
                    lista_def=[fecha,año,i,j,str(findDay(str(dia)+" "+str(mes)+" "+str(año))),semana,"0" + str(mes),festivos(fecha)]
                else:
                    fecha=str(año) + "-0" + str(mes) + "-" + str(dia)
                    cadena = str(año) + "-0" + str(mes) + "-" + str(dia) + "," + str(año) + "," + i+","+j+","+str(findDay(str(dia)+" "+str(mes)+" "+str(año)))+","+str(semana)+",0" + str(mes)+","+festivos(fecha)
                    lista_def = [fecha, año, i, j, str(findDay(str(dia) + " " + str(mes) + " " + str(año))), semana,
                                 "0" + str(mes), festivos(fecha)]
            else:
                if dia < 10:
                    fecha=str(año) + "-" + str(mes) + "-0" + str(dia)
                    cadena = str(año) + "-" + str(mes) + "-0" + str(dia) + "," + str(año) + "," + i+","+j+","+str(findDay(str(dia)+" "+str(mes)+" "+str(año)))+","+str(semana)+"," + str(mes)+","+festivos(fecha)
                    lista_def = [fecha, año, i, j, str(findDay(str(dia) + " " + str(mes) + " " + str(año))), semana,str(mes), festivos(fecha)]
                else:
                    fecha=str(año) + "-" + str(mes) + "-" + str(dia)
                    cadena = str(año) + "-" + str(mes) + "-" + str(dia) + "," + str(año) + "," + i+","+j+","+str(findDay(str(dia)+" "+str(mes)+" "+str(año)))+","+str(semana)+"," + str(mes)+","+festivos(fecha)
                    lista_def = [fecha, año, i, j, str(findDay(str(dia) + " " + str(mes) + " " + str(año))), semana,
                                 str(mes), festivos(fecha)]

            if (mes == 1 or mes == 3 or mes == 5 or mes == 7 or mes == 8 or mes == 10) and i == 'Caida' and j =='VILLA HERMOSA':
                if dia == 31:
                    mes = mes + 1
                    dia = 0
            elif (mes == 4 or mes == 6 or mes == 9 or mes == 11) and i == 'Caida' and j =='VILLA HERMOSA' :
                if dia == 30:
                    mes = mes + 1
                    dia = 0
            elif mes == 2 and i == 'Caida' and j =='VILLA HERMOSA':
                if dia == 28:
                    mes = mes + 1
                    dia = 0
            elif mes == 12:
                if dia == 31 and i == 'Caida' and j =='VILLA HERMOSA':
                    año = año + 1
                    mes = 1
                    dia = 0
                    semana=1
            #print(cadena)
            lista = lista + [cadena]
            lista_de_listas=lista_de_listas+[lista_def]

    dia=dia+1
    dia_semana = str(findDay(str(dia) + " " + str(mes) + " " + str(año)))
    if dia_semana == 'lunes':
        semana = semana + 1
    if(año>2022):
        break



with open('prediccion.csv', 'w', encoding="utf-8") as file:
    writer = csv.writer(file)
    writer.writerow(header)
    writer.writerows(lista_de_listas)