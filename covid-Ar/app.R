library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(extrafont)
library(DT)

dat<-read_csv("datCovid.csv")
data_casos<-read_csv("data_casos.csv")
data_internados<-read_csv("data_internados.csv")
data_fallecidos<-read_csv("data_fallecidos.csv")
source("ggplot_themes.R")

provincia_list <-c("CABA","Córdoba","Buenos Aires","Río Negro","Chaco","San Luis",
               "Entre Ríos","Tierra del Fuego","Santa Fe","Tucumán","Jujuy","Salta",
               "Santa Cruz","Santiago del Estero","Neuquén","Corrientes","Mendoza",
               "La Pampa","Misiones","La Rioja","San Juan","Chubut","Formosa","Catamarca")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Coronavirus en Argentina", titleWidth = "300px"),
    dashboardSidebar(width = 300,
                     sidebarMenu(
                         menuItem("Casos Diagnosticados", tabName = "casos_diagnosticados"),
                         menuItem("Hospitalizados", tabName = "hospitalizados" ),
                         menuItem("Fallecidos", tabName = "fallecidos")
                     ),
                     sliderInput("fecha",
                                  label = "Días",
                                  timeFormat = "%Y-%m-%d",
                                  value = ymd("2020-03-06"),
                                  min = ymd("2020-03-06"),
                                  max = ymd("2020-07-07"),
                                  width = "100%"),
                     selectInput("provincias",
                                 label = "Provincias",
                                 choices = c("Nacional" = "",
                                             list(Provincias = provincia_list)),
                                 multiple = TRUE
                                 )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "casos_diagnosticados",
                    fluidRow(
                            box(title = "Casos Diagnosticados",
                                leafletOutput("mapaCasos",height = "600px", width = "100%")
                                ),
                            box(title = "Casos por día",
                                plotOutput("casos_dia",height = "250px")
                                ),
                            box(title = "Casos acumulados por provincia",
                                div(
                                    style = "overflow-y:scroll;height:270px", dataTableOutput("tablaCasos")
                                    ),
                                height = "330px",
                                )
                            )
                    ),
            tabItem(tabName = "hospitalizados",
                    fluidRow(
                        box(title = "Hospitalizados",
                            leafletOutput("capaHospitalizados",height = 600, width = "100%")
                            ),
                        box(title = "Internados por día",
                            plotOutput("internados_dia",height = "250px")
                            ),
                        box(title = "Internados acumulados por provincia",
                            div(
                                style = "overflow-y:scroll;height:270px", dataTableOutput("tabla_internados")
                            ),
                            height = "330px",
                        )
                    )
            ),
            tabItem(tabName = "fallecidos",
                    fluidRow(
                        box(title = "Fallecidos",
                            leafletOutput("capaFallecidos",height = 600, width = "100%")
                            ),
                        box(title = "Fallecidos por día",
                            plotOutput("fallecidos_dia",height = "250px")
                            ),
                        box(title = "Fallecidos acumulados por provincia",
                            div(
                                style = "overflow-y:scroll;height:270px", dataTableOutput("tabla_fallecidos")
                            ),
                            height = "330px",
                        )
            )
        )
     )
    )
)

server <- function(input, output) {
    #objetos reactivos
    graph_casos_reac <- reactive({
        data_casos %>% group_by(fecha_diagnostico, residencia_provincia_nombre,lat,lng) %>%
            filter(fecha_diagnostico <= input$fecha,
                   if_else(is.null(input$provincias),
                           residencia_provincia_nombre %in% provincia_list,
                           residencia_provincia_nombre %in% input$provincias))
        })
    graph_internados_reac <- reactive({
        data_internados %>% group_by(fecha_internacion, residencia_provincia_nombre,lat,lng) %>%
            filter(fecha_internacion <= input$fecha,
                   if_else(is.null(input$provincias),
                           residencia_provincia_nombre %in% provincia_list,
                           residencia_provincia_nombre %in% input$provincias))
    })
    graph_fallecidos_reac <- reactive({
        data_fallecidos %>% group_by(fecha_fallecimiento, residencia_provincia_nombre,lat,lng) %>%
            filter(fecha_fallecimiento <= input$fecha,
                   if_else(is.null(input$provincias),
                           residencia_provincia_nombre %in% provincia_list,
                           residencia_provincia_nombre %in% input$provincias))
    })
    #mapas por capa de datos
    output$mapaCasos <- renderLeaflet({
        graph_casos_reac() %>% leaflet() %>% 
            addProviderTiles(providers$Stamen.TonerLite, 
                             options = tileOptions(minZoom = 4, maxZoom = 5)) %>%
            addCircleMarkers(lat = ~lat, lng = ~lng, radius= ~log2(Casos)*3, 
                             fillOpacity = 0.5,
                             label = ~residencia_provincia_nombre,
                             stroke = F, fillColor = "#6a5acd",
                             layerId = ~residencia_provincia_nombre)%>%
            setView(lat = -34,lng = -64 , zoom = 4) %>% 
            setMaxBounds(lng1 = -80, lng2 = -50, lat1 = -55, lat2 = -20)
    })
    output$capaHospitalizados <- renderLeaflet({
        graph_internados_reac() %>% leaflet() %>% 
            addProviderTiles(providers$Stamen.TonerLite, 
                             options = tileOptions(minZoom = 4, maxZoom = 5)) %>%
            addCircleMarkers(lat = ~lat, lng = ~lng, radius= ~log2(Casos)*3, 
                             fillOpacity = 0.5,
                             label = ~residencia_provincia_nombre,
                             stroke = F, fillColor = "#ff6347",
                             layerId = ~residencia_provincia_nombre)%>%
            setView(lat = -34,lng = -64 , zoom = 4) %>% 
            setMaxBounds(lng1 = -80, lng2 = -50, lat1 = -55, lat2 = -20)
    })
    
    output$capaFallecidos <- renderLeaflet({
        graph_fallecidos_reac() %>% leaflet() %>% 
            addProviderTiles(providers$Stamen.TonerLite, 
                             options = tileOptions(minZoom = 4, maxZoom = 5)) %>%
            addCircleMarkers(lat = ~lat, lng = ~lng, radius= ~log2(Casos)*3, 
                             fillOpacity = 0.5,
                             label = ~residencia_provincia_nombre,
                             stroke = F, fillColor = "#696969",
                             layerId = ~residencia_provincia_nombre)%>%
            setView(lat = -34,lng = -64 , zoom = 4) %>% 
            setMaxBounds(lng1 = -80, lng2 = -50, lat1 = -55, lat2 = -20)
    })
    #gráficos por capas
    output$casos_dia <- renderPlot({
        graph_casos_reac() %>%
            ggplot(aes(fecha_diagnostico,Casos))+
            geom_col(fill = "#6a5acd")+
            sb_sober_theme_col
    })
    output$tablaCasos <-renderDataTable({
        graph_casos_reac() %>% ungroup() %>% 
            select(residencia_provincia_nombre, Casos,fecha_diagnostico) %>%
            group_by(residencia_provincia_nombre) %>% summarize(Casos = sum(Casos))
    },selection = list(target = "cell")
    )
    output$internados_dia <- renderPlot({
        graph_internados_reac() %>% 
            ggplot(aes(fecha_internacion,Casos))+
            geom_col(fill = "#ff6347")+
            sb_sober_theme_col
    })
    output$tabla_internados <- renderDataTable({
        graph_internados_reac() %>% ungroup() %>% 
            select(residencia_provincia_nombre, Casos,fecha_internacion) %>%
            group_by(residencia_provincia_nombre) %>% summarize(Casos = sum(Casos))
    })
    output$fallecidos_dia <- renderPlot({
        graph_fallecidos_reac() %>% 
            ggplot(aes(fecha_fallecimiento,Casos))+
            geom_col(fill = "#696969")+
            sb_sober_theme_col
    })
    output$tabla_fallecidos <- renderDataTable({
        graph_fallecidos_reac() %>% ungroup() %>% 
            select(residencia_provincia_nombre, Casos,fecha_fallecimiento) %>%
            group_by(residencia_provincia_nombre) %>% summarize(Casos = sum(Casos))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
