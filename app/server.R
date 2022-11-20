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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    base_final <- read.csv("base_final.csv", encoding="UTF-8")
    names(base_final)<- c("X","FECHA","ANO","CLASE","DIRECCION","HORA","DIA_SEMANA","GRAVEDAD","MES","NRO_RADICADO","NUMCOMUNA","LONGITUD","LATITUD","DISENO","COMUNA","BARRIO","FESTIVIDAD"     ,"SEMANA","TIPO_FESTIVIDAD")
    
    catastral <- read.csv("Limite_Barrio_Vereda_Catastral.csv", encoding="UTF-8")
    
    #catastro <- read_sf("Limite_Barrio_Vereda_Catastral.shp")
    
    barrio_vereda <- read.csv("Barrio_Vereda_2014.csv", encoding="UTF-8")
    
    output$prediccion <- renderPlot({
      
      base_final$CLASE <- as.factor(as.character(base_final$CLASE))
      datos_vl <- subset(base_final, (ANO == '2018'))
      base_final01 <- subset(base_final, (ANO != '2018'))
      base_final02 <- subset(base_final01, (ANO != '2019'))
      base_final03 <- subset(base_final02, (ANO != '2020'))
      
      datos_lm4 <- base_final03 %>% group_by(FECHA, FESTIVIDAD, DIA_SEMANA, 
                                             CLASE) %>% count(name = "NRO_ACCID")
      
      lm4 <- glm(NRO_ACCID ~ FESTIVIDAD+DIA_SEMANA+CLASE, family = "poisson", 
                 data = datos_lm4)
      
      datos_lm6 <- base_final03 %>% group_by(FECHA, FESTIVIDAD, SEMANA, 
                                             CLASE) %>% count(name = "NRO_ACCID")
      
      lm6 <- glm(NRO_ACCID ~ FESTIVIDAD+SEMANA+CLASE, family = "poisson", 
                 data = datos_lm6)
      
      datos_lm7 <- base_final03 %>% group_by(FECHA, FESTIVIDAD, MES, 
                                             CLASE) %>% count(name = "NRO_ACCID")
      
      lm7 <- glm(NRO_ACCID ~ FESTIVIDAD+MES+CLASE, family = "poisson", 
                 data = datos_lm7)
      
      Base_prediccion <- read.csv("prediccion.csv", sep = ",", encoding = "UTF-8")
      names(Base_prediccion)<- c("X","FECHA","ANO","CLASE","DIA_SEMANA","MES","SEMANA","FESTIVIDAD")
      
      Base_prediccion03 <- Base_prediccion[,-4]
      Base_prediccion04 <- Base_prediccion03[,-4]
      
      
      if(input$year=="2020"){
        
        if(input$tiempo=="Diario"){
          
          Base_prediccion_2020 <- subset(Base_prediccion, (ANO != '2021'))
          
          Base_prediccion_2020$FECHA <- as.Date(Base_prediccion_2020$FECHA)
          Base_prediccion_2020$CLASE <- as.factor(Base_prediccion_2020$CLASE)
          Base_prediccion_2020$DIA_SEMANA <- as.factor(Base_prediccion_2020$DIA_SEMANA)
          Base_prediccion_2020$ANO <- as.integer(Base_prediccion_2020$ANO)
          Base_prediccion_2020$FESTIVIDAD <- as.factor(Base_prediccion_2020$FESTIVIDAD)
          
          prediccion_2020 <- predict(object = lm4, newdata = Base_prediccion_2020,
                                     type = "response")
          prediccion_diaria2020 <- Base_prediccion_2020 %>% 
            mutate(NRO_ACCID = round(prediccion_2020,0))
          
          if(input$tipo_accidente=="Todos"){
            
            diario_20_02 <- prediccion_diaria2020 %>%
              group_by(FECHA, DIA_SEMANA, CLASE, FESTIVIDAD) %>%
              summarise(NRO_TOTAL_ACCID=NRO_ACCID)
            
          }else{
            
            diario_20_02 <- prediccion_diaria2020 %>%
              group_by(FECHA, DIA_SEMANA, CLASE, FESTIVIDAD) %>%
              filter(CLASE == input$tipo_accidente)%>%
              summarise(NRO_TOTAL_ACCID=NRO_ACCID)
          }
          plot(x = factor(diario_20_02$FECHA), y = diario_20_02$NRO_TOTAL_ACCID)
          
        }else{
          
          Base_prediccion_2020 <- subset(Base_prediccion, (ANO != '2021'))
          
          Base_prediccion_2020$FECHA <- as.Date(Base_prediccion_2020$FECHA)
          Base_prediccion_2020$CLASE <- as.factor(Base_prediccion_2020$CLASE)
          Base_prediccion_2020$DIA_SEMANA <- as.factor(Base_prediccion_2020$DIA_SEMANA)
          Base_prediccion_2020$ANO <- as.integer(Base_prediccion_2020$ANO)
          Base_prediccion_2020$FESTIVIDAD <- as.factor(Base_prediccion_2020$FESTIVIDAD)
          
          prediccion_2020 <- predict(object = lm7, newdata = Base_prediccion_2020,
                                     type = "response")
          prediccion_mensual2020 <- Base_prediccion_2020 %>% 
            mutate(NRO_ACCID = round(prediccion_2020,0))
          
          prediccion_mensual2020 <-  prediccion_mensual2020[,c(-1,-2,-3,-5,-7)]
          
          mensual_20 <- prediccion_mensual2020 %>% group_by(CLASE, MES, NRO_ACCID, FESTIVIDAD) %>% summarize(total = n())
          mensual_20 <- mutate(mensual_20, NRO_ACCID_TOTAL=NRO_ACCID*total)
          
          mensual_20_02 <- mensual_20 %>%
            group_by(MES, CLASE, FESTIVIDAD) %>%
            summarise(NRO_TOTAL_ACCID=sum(NRO_ACCID_TOTAL))
          
          if(input$tipo_accidente == "Todos"){
            
            mensual_20_02 <- mensual_20 %>%
              group_by(MES, CLASE, FESTIVIDAD) %>%
              summarise(NRO_TOTAL_ACCID=sum(NRO_ACCID_TOTAL))
            
          } else{
            
            mensual_20_02 <- mensual_20 %>%
              group_by(MES, CLASE, FESTIVIDAD) %>%
              filter(CLASE == input$tipo_accidente)%>%
              summarise(NRO_TOTAL_ACCID=sum(NRO_ACCID_TOTAL))
            
          }
          plot(x = factor(mensual_20_02$MES), y = mensual_20_02$NRO_TOTAL_ACCID)
          
          
        }
        
      }else{
        
        if(input$tiempo=="Diario"){
          Base_prediccion_2021 <- subset(Base_prediccion, (ANO != '2020'))
          
          Base_prediccion_2021$FECHA <- as.Date(Base_prediccion_2021$FECHA)
          Base_prediccion_2021$CLASE <- as.factor(Base_prediccion_2021$CLASE)
          Base_prediccion_2021$DIA_SEMANA <- as.factor(Base_prediccion_2021$DIA_SEMANA)
          Base_prediccion_2021$ANO <- as.integer(Base_prediccion_2021$ANO)
          Base_prediccion_2021$FESTIVIDAD <- as.factor(Base_prediccion_2021$FESTIVIDAD)
          
          prediccion_2021 <- predict(object = lm4, newdata = Base_prediccion_2021,
                                     type = "response")
          prediccion_diaria2021 <- Base_prediccion_2021 %>% 
            mutate(NRO_ACCID = round(prediccion_2021,0))
          
          diario_21_02 <- prediccion_diaria2021 %>%
            group_by(FECHA, DIA_SEMANA, CLASE, FESTIVIDAD) %>%
            summarise(NRO_TOTAL_ACCID=NRO_ACCID)
          
          if(input$tipo_accidente=="Todos"){
            
            diario_21_02 <- prediccion_diaria2021 %>%
              group_by(FECHA, DIA_SEMANA, CLASE, FESTIVIDAD) %>%
              summarise(NRO_TOTAL_ACCID=NRO_ACCID)
          }else{
            
            diario_21_02 <- prediccion_diaria2021 %>%
              group_by(FECHA, DIA_SEMANA, CLASE, FESTIVIDAD) %>%
              filter(CLASE == input$tipo_accidente)%>%
              summarise(NRO_TOTAL_ACCID=NRO_ACCID)
          }
          plot(x = factor(diario_21_02$FECHA), y = diario_21_02$NRO_TOTAL_ACCID)
          
        } else{
          
          Base_prediccion_2021 <- subset(Base_prediccion, (ANO != '2020'))
          
          Base_prediccion_2021$FECHA <- as.Date(Base_prediccion_2021$FECHA)
          Base_prediccion_2021$CLASE <- as.factor(Base_prediccion_2021$CLASE)
          Base_prediccion_2021$DIA_SEMANA <- as.factor(Base_prediccion_2021$DIA_SEMANA)
          Base_prediccion_2021$ANO <- as.integer(Base_prediccion_2021$ANO)
          Base_prediccion_2021$FESTIVIDAD <- as.factor(Base_prediccion_2021$FESTIVIDAD)
          
          prediccion_2021 <- predict(object = lm4, newdata = Base_prediccion_2021,
                                     type = "response")
          prediccion_mensual2021 <- Base_prediccion_2021 %>% 
            mutate(NRO_ACCID = round(prediccion_2021,0))
          
          prediccion_mensual2021 <-  prediccion_mensual2021[,c(-1,-2,-3,-5,-7)]
          
          mensual_21 <- prediccion_mensual2021 %>% group_by(CLASE, MES, NRO_ACCID, FESTIVIDAD) %>% summarize(total = n())
          mensual_21 <- mutate(mensual_21, NRO_ACCID_TOTAL=NRO_ACCID*total)
          
          mensual_21_02 <- mensual_21 %>%
            group_by(MES, CLASE, FESTIVIDAD) %>%
            summarise(NRO_TOTAL_ACCID=sum(NRO_ACCID_TOTAL))
          
          if(input$tipo_accidente=="Todos"){
            
            mensual_21_02 <- mensual_21 %>%
              group_by(MES, CLASE, FESTIVIDAD) %>%
              summarise(NRO_TOTAL_ACCID=sum(NRO_ACCID_TOTAL))
            
          }else{
            
            mensual_21_02 <- mensual_21 %>%
              group_by(MES, CLASE, FESTIVIDAD) %>%
              filter(CLASE == input$tipo_accidente)%>%
              summarise(NRO_TOTAL_ACCID=sum(NRO_ACCID_TOTAL))
            
          }
          
          plot(x = factor(mensual_21_02$MES), y = mensual_21_02$NRO_TOTAL_ACCID)
          
        }
      }
      
    })

})
