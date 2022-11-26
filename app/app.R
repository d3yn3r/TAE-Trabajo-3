library(dplyr)
library(tidyverse)
library(rgdal)
library(lubridate)
library(leaflet)
library(stringi)
library(sf)
library(rgdal)
library(ggplot2)
library(cluster)
library(factoextra)

load("geo_data.RData")
load("lm6.RData")
load("lm6_s.RData")
load("lm6_m.RData")
load("funcion_acentos.RDATA")

tipo_de_accidente <- c("Todos", "Atropello", "Caida de Ocupante", "Choque", "Incendio", 
                       "Volcamiento", "Otro")
tiempo <- c("Diario", "Semanal","Mensual")
tiempoD <- c("Diario", "Semanal","Mensual")

anio <- c("2021","2022")

anios<- c("2014","2016","2015","2017","2018","2019","2020")

catastro <- read_sf("Catastro_gdb.shp")
grupos_de_barrios <- c("Todas", levels(factor(catastro$NOMBRE_COM)))

ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                navbarPage("Accidentalidad en medellin",
                           tabPanel(icon("home"),
                                    fluidRow(
                                      
                                      column(8,h3(strong("Descripcion")),
                                             p(style="text-align: justify;","Esta aplicacion esta hecha para observar las predicciones de accidentalidad para el 2021 y el 2022 se agrupan las diferentes comunas de medellin dependiendo de su accidentalidad, podra encontrar el analisis descriptivo realizado para obtencion de estos resultados en el siguiente link", a("Reporte", href="https://github.com/d3yn3r/TAE-Trabajo-3")),
                                             p(style="text-align: justify;", "La accidentalidad en medellin es un problema de movilidad grave, considerada como un problema de salud publica es necesario que se implementen estrategias y politicas para mejorar esta situacion, en esta aplicacion se podran observar en que meses hay mas accidentalidad y en que zonas de medellin se concentra la accidentalidad para darle un mayor enfoque a esas estrategias y politicas")
                                             
                                      )
                                    )
                                    
                           ),
                           tabPanel("Datos Historicos",
                                    fluidRow(
                                      column(4,
                                             h3(strong("Datos historicos")),
                                             p(style="text-align: justify;","En esta seccion puedes consultar los datos de los diferentes accidentes de diferentes fechas entre 2014 y 2020, tambien podras elegir si quieres ver el total de accidentes por semana, diario o mensual, como tambien se podra filtrar por el tipo de accidente")),
                                      column(8,
                                             fluidRow(
                                               column(4, selectInput("tiempoD",
                                                                     "Datos por:",
                                                                     choices = tiempoD)),
                                               column(4, selectInput("tipo_accid",
                                                                     "Tipo de accidente:",
                                                                     choices = tipo_de_accidente)),
                                               column(4, selectInput("years",
                                                                     "Anio:",
                                                                     choices = anios))
                                             ),
                                             plotOutput("datoshist")
                                      )),
                                    fluidRow(column(
                                      4,
                                      p(style="text-align: justify;","Aca podra observar la cantidad de accidentes que hubo por tipo de accidentes de acuerda a una fecha en especifico")
                                    ),
                                    column(8,
                                           fluidRow(
                                             column(4,
                                                    dateInput("Fecha",
                                                              "Ingrese una fecha entre 2014 y 2020",
                                                              value = "2014-07-04",
                                                              min = "2014-07-04",
                                                              max = "2020-12-31",
                                                              format = "yyyy-mm-dd"))
                                           ),
                                           plotOutput("datoshistFecha")
                                    ))
                           ),
                           
                           tabPanel("Prediccion",
                                    fluidRow(
                                      column(5,
                                             h3(strong("Prediccion de accientes")),
                                             p(style="text-align: justify;","Aca se pueden observar las predicciones de accidentalidad hechas para el 2021 y el 2022, se puede seleccionar si desea ver la prediccion del 2021 o del 2022, ademas de poder filtrar por tipo de accidente y si desea ver la cantidad de accidentes diaros o por mes")),
                                      column(7,
                                             fluidRow(
                                               column(4, selectInput("tiempo",
                                                                     "Prediccion por:",
                                                                     choices = tiempo)),
                                               column(4, selectInput("tipo_accidente",
                                                                     "Tipo de accidente:",
                                                                     choices = tipo_de_accidente)),
                                               column(4, selectInput("year",
                                                                     "Anio:",
                                                                     choices = anio))
                                             ),
                                             plotOutput("prediccion")
                                      )),
                                    fluidRow(
                                      column(4,
                                             h3(strong("Prediccion por fecha")),
                                             p(style="text-align: justify;","Aca podra observar la prediccion de cantidad de accidentes por tipo de accidentes de acuerda a una fecha en especifico")),
                                      column(8,
                                             fluidRow(
                                               column(4,
                                                      dateInput("FechaPrediccion",
                                                                "Ingrese una fecha entre 2021 y 2022",
                                                                value = "2021-01-01",
                                                                min = "2021-01-01",
                                                                max = "2022-12-31",
                                                                format = "yyyy-mm-dd")),
                                               
                                             ),
                                             plotOutput("prediccionFecha"))
                                    )
                           ),
                           
                           tabPanel("Agrupamiento",
                                    fluidRow(
                                      column(4, 
                                             h3(strong("Grupos de comunas por nivel de accidentalidad")),
                                             p(style="text-align: justify;","En esta seccion se muestran las comunas agrupadas por su nivel de accidentalidad, usted podra seleccionar la comuna de interes para observar sus caracteristicas (tambien puede seleccionar las comunas dando click en el mismo mapa)"),
                                             selectInput("comuna",
                                                         "Seleccione la comuna:",
                                                         choices = grupos_de_barrios)),
                                      
                                      column(8,
                                             leaflet::leafletOutput("agrupamiento")
                                             
                                      )
                                    ))
                )
                
)


server <- (function(input, output) {
  
  bd_depurada <- read.csv("base_depurada3.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
  
  #catastral <- read.csv("Limite_Barrio_Vereda_Catastral.csv", encoding="UTF-8")
  
  catastro <- read_sf("Catastro_gdb.shp")
  
  
  
  output$prediccion <- renderPlot({
    
    bd_depurada$CLASE <- as.factor(as.character(bd_depurada$CLASE))
    datos_val_2019 <- subset(bd_depurada, (ANO == '2019'))
    datos_val_2020 <- subset(bd_depurada, (ANO == '2020'))
    base_train <- subset(bd_depurada, (ANO != '2019' & ANO != '2020'))
    
    
    datos_lmsemana <- base_train %>% group_by(FECHA, FESTIVIDAD, SEMANA, 
                                              CLASE) %>% count(name = "NRO_ACCID")
    
    lmSemanal <- glm(NRO_ACCID ~ FESTIVIDAD+SEMANA+CLASE, family = "poisson", 
                     data = datos_lmsemana)
    
    datos_lmmes <- base_train %>% group_by(FECHA, FESTIVIDAD, MES, 
                                           CLASE) %>% count(name = "NRO_ACCID")
    
    lmMensual <- glm(NRO_ACCID ~ FESTIVIDAD+MES+CLASE, family = "poisson", 
                     data = datos_lmmes)
    
    
    bd_prediccion <- read.csv("prediccion_corregida.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
    
    bd_prediccion$DIA_SEMANA=remove.accents(bd_prediccion$DIA_SEMANA)
    
    
    if(input$year=="2021"){
      
      bd_prediccion_2021 <- subset(bd_prediccion, (ANO == '2021'))
      
      bd_prediccion_2021$FECHA <- as.Date(bd_prediccion_2021$FECHA)
      bd_prediccion_2021$CLASE <- as.factor(bd_prediccion_2021$CLASE)
      bd_prediccion_2021$DIA_SEMANA <- as.factor(bd_prediccion_2021$DIA_SEMANA)
      bd_prediccion_2021$ANO <- as.integer(bd_prediccion_2021$ANO)
      bd_prediccion_2021$FESTIVIDAD <- as.factor(bd_prediccion_2021$FESTIVIDAD)
      bd_prediccion_2021$COMUNA <- as.factor(bd_prediccion_2021$COMUNA)
      bd_prediccion_2021$SEMANA <- as.integer(bd_prediccion_2021$SEMANA)
      
      if(input$tiempo=="Diario"){
        
        prediccion_2021 <- predict(object = lm6, newdata = bd_prediccion_2021,
                                   type = "response")
        
        prediccion_diaria2021 <- bd_prediccion_2021 %>% 
          mutate(NRO_ACCID = round(prediccion_2021,0))
        
        if(input$tipo_accidente=="Todos"){
          
          diario <- prediccion_diaria2021 %>%
            group_by(FECHA, DIA_SEMANA) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }else{
          
          diario <- prediccion_diaria2021 %>%
            group_by(FECHA, DIA_SEMANA,CLASE) %>%
            filter(CLASE == input$tipo_accidente)%>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
        }
        ggplot(diario,aes(x=FECHA,y=TOTAL_ACCID))+
          geom_bar(stat = "identity", fill=4)  + labs(y="TOTAL ACCIDENTES", x="FECHA")
        
      }else if(input$tiempo=="Semanal"){
        
        prediccion_2021 <- predict(object = lm6_semanal, newdata = bd_prediccion_2021,
                                   type = "response")
        
        prediccion_semana2021 <- bd_prediccion_2021 %>% 
          mutate(NRO_ACCID = round(prediccion_2021,0))
        
        if(input$tipo_accidente=="Todos"){
          
          semana <- prediccion_semana2021 %>% 
            group_by(SEMANA) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }else{
          
          semana <- prediccion_semana2021 %>% 
            group_by(SEMANA,CLASE) %>%
            filter(CLASE == input$tipo_accidente) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }
        
        ggplot(semana,aes(x=SEMANA,y=TOTAL_ACCID))+
          geom_bar(stat = "identity", fill=4) + labs(y="TOTAL ACCIDENTES", x="SEMANA")
      }
      else{
        
        prediccion_2021 <-  predict(object = lm6_mensual, newdata = bd_prediccion_2021,
                                    type = "response")
        
        prediccion_mensual2021 <- bd_prediccion_2021 %>% 
          mutate(NRO_ACCID = round(prediccion_2021,0))
        
        if(input$tipo_accidente == "Todos"){
          
          mensual <- prediccion_mensual2021 %>%
            group_by(MES) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        } else{
          
          mensual <- prediccion_mensual2021 %>%
            group_by(MES, CLASE) %>%
            filter(CLASE == input$tipo_accidente)%>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }
        ggplot(mensual,aes(x=MES,y=TOTAL_ACCID))+
          geom_bar(stat = "identity", fill=4)  + labs(y="TOTAL ACCIDENTES", x="MES")
        
        
      }
      
    }else{
      
      bd_prediccion_2022 <- subset(bd_prediccion, (ANO == '2022'))
      
      bd_prediccion_2022$FECHA <- as.Date(bd_prediccion_2022$FECHA)
      bd_prediccion_2022$CLASE <- as.factor(bd_prediccion_2022$CLASE)
      bd_prediccion_2022$DIA_SEMANA <- as.factor(bd_prediccion_2022$DIA_SEMANA)
      bd_prediccion_2022$ANO <- as.integer(bd_prediccion_2022$ANO)
      bd_prediccion_2022$FESTIVIDAD <- as.factor(bd_prediccion_2022$FESTIVIDAD)
      bd_prediccion_2022$COMUNA <- as.factor(bd_prediccion_2022$COMUNA)
      bd_prediccion_2022$SEMANA <- as.integer(bd_prediccion_2022$SEMANA)
      
      if(input$tiempo=="Diario"){
        
        prediccion_2022 <- predict(object = lm6, newdata = bd_prediccion_2022,
                                   type = "response")
        
        prediccion_diaria2022 <- bd_prediccion_2022 %>% 
          mutate(NRO_ACCID = round(prediccion_2022,0))
        
        if(input$tipo_accidente=="Todos"){
          
          diario <- prediccion_diaria2022 %>%
            group_by(FECHA, DIA_SEMANA) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }else{
          
          diario <- prediccion_diaria2022 %>%
            group_by(FECHA, DIA_SEMANA,CLASE) %>%
            filter(CLASE == input$tipo_accidente)%>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
        }
        ggplot(diario,aes(x=FECHA,y=TOTAL_ACCID))+
          geom_bar(stat = "identity", fill=4)  + labs(y="TOTAL ACCIDENTES", x="FECHA")
        
      }else if(input$tiempo=="Semanal"){
        
        prediccion_2021 <- predict(object = lm6_semanal, newdata = bd_prediccion_2022,
                                   type = "response")
        
        prediccion_semana2022 <- bd_prediccion_2022 %>% 
          mutate(NRO_ACCID = round(prediccion_2022,0))
        
        if(input$tipo_accidente=="Todos"){
          
          semana <- prediccion_semana2022 %>% 
            group_by(SEMANA) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }else{
          
          semana <- prediccion_semana2022 %>% 
            group_by(SEMANA,CLASE) %>%
            filter(CLASE == input$tipo_accidente) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }
        
        ggplot(semana,aes(x=SEMANA,y=TOTAL_ACCID))+
          geom_bar(stat = "identity", fill=4) + labs(y="TOTAL ACCIDENTES", x="SEMANA")
      }
      else{
        
        prediccion_2022 <- predict(object = lm6_mensual, newdata = bd_prediccion_2022,
                                   type = "response")
        
        prediccion_mensual2022 <- bd_prediccion_2022 %>% 
          mutate(NRO_ACCID = round(prediccion_2022,0))
        
        if(input$tipo_accidente == "Todos"){
          
          mensual <- prediccion_mensual2022 %>%
            group_by(MES) %>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        } else{
          
          mensual <- prediccion_mensual2022 %>%
            group_by(MES, CLASE) %>%
            filter(CLASE == input$tipo_accidente)%>%
            summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
          
        }
        ggplot(mensual,aes(x=MES,y=TOTAL_ACCID))+
          geom_bar(stat = "identity", fill=4)  + labs(y="TOTAL ACCIDENTES", x="MES")
        
        
      }
    }
    
  })
  
  output$datoshist <- renderPlot({
    base_depurada <- read.csv("base_depurada3.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
    
    ano <- input$years
    base_depurada$CLASE <- as.factor(as.character(base_depurada$CLASE))
    
    datos <- subset(base_depurada, (ANO==ano))
    
    
    if(input$tiempoD=="Diario"){
      
      datos <- datos %>% group_by(FECHA, FESTIVIDAD, DIA_SEMANA, 
                                  CLASE) %>% count(name = "NRO_ACCID")
      
      if(input$tipo_accid=="Todos"){
        
        diario <- datos %>% 
          group_by(FECHA, DIA_SEMANA) %>%
          summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
        
      }else{
        
        diario <- datos %>% 
          group_by(FECHA, DIA_SEMANA,CLASE) %>%
          filter(CLASE==input$tipo_accid) %>%
          summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
      }
      
      ggplot(diario,aes(FECHA,TOTAL_ACCID))+
        geom_bar(stat = "identity", fill=4) + labs(y="TOTAL ACCIDENTES", x="FECHA")
      
    }else if(input$tiempoD=="Semanal"){
      
      datos <- datos %>% group_by(FECHA, FESTIVIDAD, SEMANA, 
                                  CLASE) %>% count(name = "NRO_ACCID")
      
      if(input$tipo_accid=="Todos"){
        semanal <- datos %>%
          group_by(SEMANA) %>%
          summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
        
      }else{
        
        semanal <- datos %>%
          group_by(SEMANA,CLASE) %>%
          filter(CLASE==input$tipo_accid) %>%
          summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
        
      }
      
      ggplot(semanal,aes(x=SEMANA,y=TOTAL_ACCID))+
        geom_bar(stat = "identity", fill=4) + labs(y="TOTAL ACCIDENTES", x="SEMANA")
      
    }else{
      
      datos <- datos %>% group_by(FECHA, FESTIVIDAD, MES, 
                                  CLASE) %>% count(name = "NRO_ACCID")
      
      
      if(input$tipo_accid=="Todos") {
        
        mensual <- datos %>%
          group_by(MES) %>%
          summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
        
      }else{
        
        mensual <- datos %>%
          group_by(MES,CLASE) %>%
          filter(CLASE==input$tipo_accid) %>%
          summarise_at(vars(NRO_ACCID),list(TOTAL_ACCID=sum))
      }
      
      ggplot(mensual,aes(x=MES,y=TOTAL_ACCID))+
        geom_bar(stat = "identity", fill=4)  + labs(y="TOTAL ACCIDENTES", x="MES")
      
    }
    
  })
  
  output$datoshistFecha <- renderPlot({
    
    base_depurada <- read.csv("base_depurada3.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
    base_depurada$CLASE <- as.factor(as.character(base_depurada$CLASE))
    date <- input$Fecha
    
    fecha <- base_depurada %>% group_by(FECHA, CLASE)%>%
      filter(FECHA==date) %>% count(name = "NRO_ACCID")
    
    ggplot(fecha,aes(CLASE, NRO_ACCID))+
      geom_bar(stat = "identity", fill=4) + labs(y="Total de accidentes", x="clase")
    
    
  })
  
  output$prediccionFecha <- renderPlot({
    #Actualizar
    bd_prediccion <- read.csv("prediccion_corregida.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
    bd_prediccion$DIA_SEMANA=remove.accents(bd_prediccion$DIA_SEMANA)
    
    bd_depurada$CLASE <- as.factor(as.character(bd_depurada$CLASE))
    datos_val_2019 <- subset(bd_depurada, (ANO == '2019'))
    datos_val_2020 <- subset(bd_depurada, (ANO == '2020'))
    base_train <- subset(bd_depurada, (ANO != '2019' & ANO != '2020'))
    
    
    bd_prediccionF <- bd_prediccion
    
    bd_prediccionF$FECHA <- as.Date(bd_prediccionF$FECHA)
    bd_prediccionF$CLASE <- as.factor(bd_prediccionF$CLASE)
    bd_prediccionF$DIA_SEMANA <- as.factor(bd_prediccionF$DIA_SEMANA)
    bd_prediccionF$ANO <- as.integer(bd_prediccionF$ANO)
    bd_prediccionF$FESTIVIDAD <- as.factor(bd_prediccionF$FESTIVIDAD)
    
    prediccionF <- predict(object = lm6, newdata = bd_prediccionF,
                           type = "response")
    prediccion_diariaF <- bd_prediccionF %>% 
      mutate(NRO_ACCID = round(prediccionF,0))
    
    date <- input$FechaPrediccion
    
    fecha <- prediccion_diariaF %>% group_by(FECHA, CLASE,NRO_ACCID)%>%
      filter(FECHA==date)
    
    ggplot(fecha,aes(CLASE, NRO_ACCID))+
      geom_bar(stat = "identity", fill=4) + labs(y="Total de accidentes", x="clase")
    
  })
  
  output$agrupamiento <- renderLeaflet({
    
    
      if(input$comuna == "Todas"){mapa_geo <- mapa_geo}
      else{mapa_geo <-mapa_geo %>% filter(NOMBRE_COM == input$comuna)}
    
      colorgrupos <- c("#f3d10f", "#2bf30f", "#f37e0f", "#f3380f")
      mapa_geo$colores <- ifelse(mapa_geo$kmm.cluster == "1", "#f3d10f",
                                 ifelse(mapa_geo$kmm.cluster == "2", "#2bf30f",
                                        ifelse(mapa_geo$kmm.cluster == "3", "#f37e0f",
                                               ifelse(mapa_geo$kmm.cluster == "4", "#f3380f",0))))
    
      leaflet() %>% addPolygons(data = mapa_geo, opacity = 0.4, color = "#545454",weight = 1, fillColor = mapa_geo$colores,
                                fillOpacity = 0.4, label = ~NOMBRE_BAR,
                                highlightOptions = highlightOptions(color = "#blue", weight = 3, bringToFront = T, opacity = 1),
                                popup = paste("Barrio: ", mapa_geo$NOMBRE_BAR, "<br>", "Grupo: ", mapa_geo$kmm.cluster, "<br>", "NÃºmero de Accidentes con heridos: ", mapa_geo$Con_heridos, "<br>", "NÃºmero de Accidentes con muertos: ", mapa_geo$Con_muertos, "<br>", "NÃºmero de Accidentes con solo daÃ±os: ", mapa_geo$Solo_danos)) %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addLegend(position = "bottomright", colors = colorgrupos, labels = c("Grupo 1: Accidentalidad Media Baja", "Grupo 2: Accidentalidad Baja", "Grupo 3: Accidentalidad Media Alta", "Grupo 4: Accidentalidad Alta"))
    
  })
  
  
  
})


# Run the application 
shinyApp(ui = ui, server = server)
