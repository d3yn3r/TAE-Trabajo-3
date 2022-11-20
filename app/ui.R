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
tiempo <- c("Diario", "Mensual")

anio <- c("2020","2021")

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  navbarPage("Accidentalidad en medellin",
                    tabPanel(icon("home"),
                             fluidRow(
                               column(7,h3(strong("Descripcion")),
                                      p(style="text-align: justify;","Esta aplicacion esta hecha para observar las predicciones de accidentalidad para el 2021 y el 2022 se agrupan las diferentes comunas de medellin dependiendo de su accidentalidad, podra encontrar el analisis descriptivo realizado para obtencion de estos resultados en el siguiente link"),
                                      )
                             )
                          
                             ),
                    tabPanel("Prediccion",
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
                                    )
                             ),
                             
                    tabPanel("Agrupamiento")
                  )
    
))
