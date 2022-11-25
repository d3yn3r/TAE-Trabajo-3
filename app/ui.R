#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(sf)
library(dplyr)



tipo_de_accidente <- c("Todos", "Atropello", "Caida de Ocupante", "Choque", "Incendio", 
                    "Volcamiento", "Otro")
tiempo <- c("Diario", "Semanal","Mensual")
tiempoD <- c("Diario", "Semanal","Mensual")

anio <- c("2021","2022")

anios<- c("2014","2016","2015","2017","2018","2019","2020")

catastro <- read_sf("Catastro_gdb.shp")
grupos_de_barrios <- c("Todas", levels(factor(catastro$NOMBRE_COM)))

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  navbarPage("Accidentalidad en medellin",
                    tabPanel(icon("home"),
                             fluidRow(
                               column(8,h3(strong("Descripcion")),
                                      p(style="text-align: justify;","Esta aplicacion esta hecha para observar las predicciones de accidentalidad para el 2021 y el 2022 se agrupan las diferentes comunas de medellin dependiendo de su accidentalidad, podra encontrar el analisis descriptivo realizado para obtencion de estos resultados en el siguiente link"),
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
                                                            "Prediccion por:",
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
                                      leafletOutput("agrupamiento")
                                      )
                             ))
                  )
    
))
