#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(sf)
library(dplyr)
library(tidyverse)
library(leaflet)
library(leaflet.providers)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    bd_depurada <- read.csv("base_depurada.csv", encoding="UTF-8")
    
    #catastral <- read.csv("Limite_Barrio_Vereda_Catastral.csv", encoding="UTF-8")
    
    catastro <- read_sf("Catastro_gdb.shp")
    
    
    
    output$prediccion <- renderPlot({
      
      bd_depurada$CLASE <- as.factor(as.character(bd_depurada$CLASE))
      datos_val_2019 <- subset(bd_depurada, (ANO == '2019'))
      datos_val_2020 <- subset(bd_depurada, (ANO == '2020'))
      base_train <- subset(bd_depurada, (ANO != '2019' & ANO != '2020'))
      
      datos_lmdiario <- base_train %>% group_by(FECHA, FESTIVIDAD, DIA_SEMANA, 
                                             CLASE) %>% count(name = "NRO_ACCID")
      
      lmDiario <- glm(NRO_ACCID ~ FESTIVIDAD+DIA_SEMANA+CLASE, family = "poisson", 
                 data = datos_lmdiario)
      
      datos_lmsemana <- base_final03 %>% group_by(FECHA, FESTIVIDAD, SEMANA, 
                                             CLASE) %>% count(name = "NRO_ACCID")
      
      lmSemanal <- glm(NRO_ACCID ~ FESTIVIDAD+SEMANA+CLASE, family = "poisson", 
                 data = datos_lmsemana)
      
      datos_lmmes <- base_final03 %>% group_by(FECHA, FESTIVIDAD, MES, 
                                             CLASE) %>% count(name = "NRO_ACCID")
      
      lmMensual <- glm(NRO_ACCID ~ FESTIVIDAD+MES+CLASE, family = "poisson", 
                 data = datos_lmmes)
      
      bd_prediccion <- read.csv("prediccion_sin_comuna.csv", sep = ",", encoding = "UTF-8")
      
      Base_prediccion03 <- Base_prediccion[,-4]
      Base_prediccion04 <- Base_prediccion03[,-4]
      
      
      if(input$year=="2021"){
        
        bd_prediccion_2021 <- subset(bd_prediccion, (ANO == '2021'))
        
        bd_prediccion_2021$FECHA <- as.Date(bd_prediccion_2021$FECHA)
        bd_prediccion_2021$CLASE <- as.factor(bd_prediccion_2021$CLASE)
        bd_prediccion_2021$DIA_SEMANA <- as.factor(bd_prediccion_2021$DIA_SEMANA)
        bd_prediccion_2021$ANO <- as.integer(bd_prediccion_2021$ANO)
        bd_prediccion_2021$FESTIVIDAD <- as.factor(bd_prediccion_2021$FESTIVIDAD)
        
        if(input$tiempo=="Diario"){
          
          prediccion_2021 <- predict(object = lmDiario, newdata = bd_prediccion_2021,
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
          
          prediccion_2021 <- predict(object = lmSemanal, newdata = bd_prediccion_2021,
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
          
          prediccion_2021 <- predict(object = lmMensual, newdata = bd_prediccion_2021,
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
        
        if(input$tiempo=="Diario"){
          
          prediccion_2022 <- predict(object = lmDiario, newdata = bd_prediccion_2022,
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
          
          prediccion_2022 <- predict(object = lmSemanal, newdata = bd_prediccion_2022,
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
          
          prediccion_2022 <- predict(object = lmMensual, newdata = bd_prediccion_2022,
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
      base_depurada <- read.csv("base_depurada.csv", encoding="UTF-8")
      
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
      
      base_depurada <- read.csv("base_depurada.csv", encoding="UTF-8")
      base_depurada$CLASE <- as.factor(as.character(base_depurada$CLASE))
      date <- input$Fecha
      
      fecha <- base_depurada %>% group_by(FECHA, CLASE)%>%
        filter(FECHA==date) %>% count(name = "NRO_ACCID")
      
      ggplot(fecha,aes(CLASE, NRO_ACCID))+
        geom_bar(stat = "identity", fill=4) + labs(y="Total de accidentes", x="clase")
      
      
    })
    
    output$prediccionFecha <- renderPlot({
      
      bd_depurada$CLASE <- as.factor(as.character(bd_depurada$CLASE))
      datos_val_2019 <- subset(bd_depurada, (ANO == '2019'))
      datos_val_2020 <- subset(bd_depurada, (ANO == '2020'))
      base_train <- subset(bd_depurada, (ANO != '2019' & ANO != '2020'))
      
      datos_lmdiario <- base_train %>% group_by(FECHA, FESTIVIDAD, DIA_SEMANA, 
                                                CLASE) %>% count(name = "NRO_ACCID")
      
      lmDiario <- glm(NRO_ACCID ~ FESTIVIDAD+DIA_SEMANA+CLASE, family = "poisson", 
                      data = datos_lmdiario)
      
      bd_prediccionF <- bd_prediccion
      
      bd_prediccionF$FECHA <- as.Date(bd_prediccionF$FECHA)
      bd_prediccionF$CLASE <- as.factor(bd_prediccionF$CLASE)
      bd_prediccionF$DIA_SEMANA <- as.factor(bd_prediccionF$DIA_SEMANA)
      bd_prediccionF$ANO <- as.integer(bd_prediccionF$ANO)
      bd_prediccionF$FESTIVIDAD <- as.factor(bd_prediccionF$FESTIVIDAD)
      
      prediccionF <- predict(object = lmDiario, newdata = bd_prediccionF,
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
      catastro <- read_sf("Catastro_gdb.shp")
      barrios_csv<-read.csv("Catastro_gdb.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
      Unido <- inner_join(barrios_csv, base_depurada, by = c("COMUNA" = "NUMCOMUNA"))
      
      nueva_base2 <- read.csv("prueba2.csv", dec=",", header=T,sep=",", encoding = "UTF-8")
      
      ahhh<-nueva_base2
      nombre_de_barrio<-iconv(ahhh$NOMBRE_BARRIO,from="UTF-8",to="ASCII//TRANSLIT")
      codigo_barrio<-iconv(ahhh$CODIGO,from="UTF-8",to="ASCII//TRANSLIT")
      prueba_def<-data.frame(codigo_barrio,nombre_de_barrio)
      prueba_def<-prueba_def%>%
        arrange(nombre_de_barrio)
      
      basemapa <- read_excel("mapa.xlsx")
      base_mapa <- data_frame(basemapa)
      maparrr <- inner_join(base_mapa, prueba_def, by = c("Barrio" = "nombre_de_barrio"))
      maparrr$codigo_barrio <- as.numeric(as.character(maparrr$codigo_barrio))
      catastro$CODIGO <- as.numeric(as.character(catastro$CODIGO))
      
      if(input$comuna == "Todas"){mapa02 <- inner_join(catastro, maparrr, by = c("CODIGO" = "codigo_barrio"))}
      else{mapa02 <- inner_join(catastro, maparrr, by = c("CODIGO" = "codigo_barrio")) %>% filter(NOMBRE_COM == input$comuna)}
      
      colorgrupos <- c("#00FF66", "#CCFF00", "#FF0000", "#0066FF")
      mapa02$colores <- ifelse(mapa02$kmm.cluster == "1", "#00FF66",
                               ifelse(mapa02$kmm.cluster == "2", "#CCFF00",
                                      ifelse(mapa02$kmm.cluster == "3", "#FF0000",
                                             ifelse(mapa02$kmm.cluster == "4", "#0066FF",0))))
      
      leaflet() %>% addPolygons(data = mapa02, opacity = 0.4, color = "#545454",weight = 1, fillColor = mapa02$colores,
                                fillOpacity = 0.4, label = ~NOMBRE_BAR,
                                highlightOptions = highlightOptions(color = "#262626", weight = 3, bringToFront = T, opacity = 1),
                                popup = paste("Barrio: ", mapa02$NOMBRE_BAR, "<br>", "Grupo: ", mapa02$kmm.cluster, "<br>", "Numero de Accidentes con heridos: ", mapa02$Con_heridos, "<br>", "Numero de Accidentes con muertos: ", mapa02$Con_muertos, "<br>", "Numero de Accidentes con solo danos: ", mapa02$Solo_danos)) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addLegend(position = "bottomright", colors = colorgrupos, labels = c("Grupo 1: Accidentalidad Moderada", "Grupo 2: Accidentalidad Baja", "Grupo 3: Accidentalidad Alta", "Grupo 4: Accidentalidad Media-Alta"))
      
    })
    
    

})
