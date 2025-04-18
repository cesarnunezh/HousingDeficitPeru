# 0. Importing and calling libraries ----
library(tidyverse)
library(readxl)
library(foreign)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(mapsPERU)
library(sf)
library(scales)
library(plotly)
library(classInt)
library(bslib)
library(shinydashboard)

# 1. Importing the data ----
## Now we import the dataset that is in an excel format
file_name <- "https://raw.githubusercontent.com/cesarnunezh/HousingDeficitPeru/main/data/bd_censo_indicadores.csv"
file_name2 <- "https://raw.githubusercontent.com/cesarnunezh/HousingDeficitPeru/main/data/bd_censo_indicadoresNUEVAMEDICION.csv"

# Read the Excel file
data <- read.csv(file_name)
dataNew <- read.csv(file_name2) %>% 
  select(-Total)

cols_to_keep <- names(data)[!grepl("rural|urbano", names(data), ignore.case = TRUE)]

# Crear el subconjunto de datos
data <- data[, cols_to_keep]

data <- data %>%
  mutate(ccdd = sprintf("%02d", ccdd),
         ccpp = sprintf("%02d", ccpp),
         ccdi = sprintf("%02d", ccdi),
         ubigeo = sprintf("%06d", ubigeo))


dataNew <- dataNew %>% 
  mutate(ubigeo = sprintf("%06d", ubigeo)) %>% 
  left_join(data %>% select(ubigeo, departamento, provincia, distrito)) %>%
  mutate(ccdd = sprintf("%02d", ccdd),
         ccpp = sprintf("%02d", ccpp),
         ccdi = sprintf("%02d", ccdi))

dataNames <- c("Número de hogares con déficit habitacional (en miles)",	"Número de hogares con déficit cuantitativo (en miles)",	
               "Número de hogares con viviendas inadecuadas (en miles)",	"Número de hogares secundarios (en miles)",	
               "Número de hogares con déficit cualitativo (en miles)",	"Número de hogares sin acceso al paquete de servicios básicos (en miles)",	
               "Número de hogares con viviendas irrecuperables (en miles)",	"Número de hogares con hacinamiento (en miles)",	
               "Porcentaje de hogares con déficit habitacional (%)",	"Porcentaje de hogares con déficit cuantitativo (%)",	
               "Porcentaje de hogares con viviendas inadecuadas (%)",	"Porcentaje de hogares secundarios (%)",	
               "Porcentaje de hogares con déficit cualitativo (%)",	"Porcentaje de hogares sin acceso al paquete de servicios básicos (%)",	
               "Porcentaje de hogares con viviendas irrecuperables (%)",	"Porcentaje de hogares con hacinamiento (%)")

dataNames <- c("ubigeo", "ccdd", "ccpp", "ccdi", "departamento", "provincia", "distrito", dataNames)

dataNamesNew <- c("Número de hogares con déficit habitacional (en miles)", 	"Número de hogares con déficit cualitativo de vivienda (en miles)",
                  "Número de hogares cuyas viviendas requieren de mejoras en sus paredes, techos o piso (en miles)", 	"Número de hogares cuyas viviendas cuentan con servicios básicos deficitarios (en miles)", 	
                  "Número de hogares cuyas viviendas requieren ampliarse (en miles)", 	"Número de hogares con déficit cuantitativo de vivienda (en miles)", 	
                  "Número de hogares cuyas viviendas son irrecuperables (en miles)", 	"Número de hogares secundarios con hacinamiento (en miles)", 	
                  "Número de hogares que cuentan con hacinamiento no ampliable (en miles)", 	"Porcentaje de hogares con déficit habitacional (%)", 	
                  "Porcentaje de hogares con déficit cualitativo de vivienda (%)", 	"Porcentaje de hogares cuyas viviendas requieren de mejoras en sus paredes, techos o piso (%)", 	
                  "Porcentaje de hogares cuyas viviendas cuentan con servicios básicos deficitarios (%)", 	"Porcentaje de hogares cuyas viviendas requieren ampliarse (%)", 	
                  "Porcentaje de hogares con déficit cuantitativo de vivienda (%)", 	"Porcentaje de hogares cuyas viviendas son irrecuperables (%)", 	
                  "Porcentaje de hogares secundarios con hacinamiento (%)", 	"Porcentaje de hogares que cuentan con hacinamiento no ampliable (%)")

dataNamesNew <- c("ccdd", "ccpp", "ccdi", "ubigeo", dataNamesNew, "departamento", "provincia", "distrito")

names(data) <- dataNames
names(dataNew) <- dataNamesNew

## Separate database for departamentos, provincias and distritos
departamentos <- data %>%
  filter(substr(ubigeo, 3, 6) == "0000")

provincias <- data %>%
  filter(substr(ubigeo, 5, 6) == "00" & substr(ubigeo, 3, 4) != "00")

distritos <- data %>%
  filter(substr(ubigeo, 5, 6) != "00" & substr(ubigeo, 3, 4) != "00")

departamentosNew <- dataNew %>%
  filter(substr(ubigeo, 3, 6) == "0000")

provinciasNew <- dataNew %>%
  filter(substr(ubigeo, 5, 6) == "00" & substr(ubigeo, 3, 4) != "00")

distritosNew <- dataNew %>%
  filter(substr(ubigeo, 5, 6) != "00" & substr(ubigeo, 3, 4) != "00")

## List of the names of departamentos, provincias and distritos
listDpto <- departamentos %>%
  select(departamento)  %>%  
  arrange(departamento) %>% 
  drop_na() %>% 
  pull(departamento)

listDpto <- c("TODOS", listDpto)

listProv <- provincias %>%
  select(provincia)  %>%  
  arrange(provincia) %>% 
  drop_na() %>% 
  pull(provincia)

listProv <- c("TODOS", listProv)

listDist <- distritos %>%
  select(distrito)  %>%  
  arrange(distrito) %>% 
  drop_na() %>% 
  pull(distrito)

listDist <- c("TODOS", listDist)

## List of the variables 
Indicadoresiniciales <- names(data)
Indicadoresexcluidos <- c("ubigeo", "ccdd", "ccpp", "ccdi", "departamento", "provincia", "distrito")

listIndicadores <- setdiff(Indicadoresiniciales, Indicadoresexcluidos)
listIndicadores <- listIndicadores[!grepl("rural|urbano", listIndicadores, ignore.case = TRUE)]
print(listIndicadores)

IndicadoresinicialesNew <- names(dataNew)
IndicadoresexcluidosNew <- c("ubigeo", "ccdd", "ccpp", "ccdi", "departamento", "provincia", "distrito", "Total")

listIndicadoresNew <- setdiff(IndicadoresinicialesNew, IndicadoresexcluidosNew)
listIndicadoresNew <- listIndicadoresNew[!grepl("rural|urbano", listIndicadoresNew, ignore.case = TRUE)]

print(listIndicadoresNew)

#1.  Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(
    bg = "white",    # Background color
    fg = "black",    # Foreground (text) color
    primary = "#116361",
    secondary = "#ED7D31",
    success = "#96D250",
    font = "Helvetica"
  ),
  tags$head(
    tags$style(HTML("
      /* Custom Navbar Colors */
      .navbar {
        background-color: #116361 !important; /* Force Navbar background color */
        color: #FFFFFF !important; /* Force Navbar text color */
      }
      .navbar .navbar-nav > li > a {
        background-color: #116361 
        color: #FFFFFF !important; /* Force Navbar link color */
      }
      .navbar .navbar-brand {
        background-color: #116361 
        color: #FFFFFF !important; /* Force Navbar brand color */
      }
      .navbar .navbar-nav > li > a:hover,
      .navbar .navbar-nav > li > a:focus {
        background-color: #116361 !important; /* Navbar link hover color */
      }
    "))
  ),
  navbarPage("Dashboard ASEI",
             tabPanel("Mapa interactivo", fluid = TRUE, icon = icon("globe-americas"),
                      h2(strong("Mapa interactivo")),
                      # Filtros horizontales en la parte superior
                      fluidRow(
                        column(
                          width = 3,
                          selectInput(inputId = "Dpto",
                                      label = "Departamento",
                                      choices = listDpto,
                                      selected = "TODOS",
                                      width = "100%")
                        ),
                        column(
                          width = 3,
                          uiOutput("uiProv")
                        ),
                        column(
                          width = 3,
                          uiOutput("uiDist")
                        ),
                        column(
                          width = 3,
                          selectInput(inputId = "Metodo",
                                      label = "Metodología",
                                      choices = c("Metodología INEI", "Nueva metodología"),
                                      selected = "Metodología INEI",
                                      width = "100%")
                        )
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          uiOutput("uiIndicador")
                        )
                      ),
                      # Contenido principal centrado
                      fluidRow(
                        column(
                          width = 12,  # Ajusta el ancho del mainPanel
                          offset = 2,  # Centra el mainPanel
                          mainPanel(
                            div(
                              class = "simpleDiv",
                              plotlyOutput("map1", height = "100%", width = "100%")
                            ))),
                        hr(),
                        column(
                          width = 12,
                          offset = 2,
                          mainPanel(
                            h4(strong("Datos resumen - Metodología INEI")),
                            div(
                              class = "simpleDiv",
                              layout_columns(
                                value_box(
                                  textOutput("dato1"),
                                  title = "Déficit habitacional",
                                  p("N° de hogares (en miles)"),
                                  showcase = icon("house", size = 100),
                                  showcase_layout = "left center",
                                  theme = "secondary"),
                                value_box(
                                  textOutput("dato2"),
                                  title = "Déficit cuantitativo",
                                  p("N° de hogares (en miles)"),
                                  showcase = icon("house", size = 100),
                                  showcase_layout = "left center",
                                  theme = "secondary"),
                                value_box(
                                  textOutput("dato3"),
                                  title = "Déficit cualitativo",
                                  p("N° de hogares (en miles)"),
                                  showcase = icon("house", size = 100),
                                  showcase_layout = "left center",
                                  theme = "secondary")
                              )
                            ),
                            h4(strong("Datos resumen - Nueva Metodología")),
                            div(
                              class = "simpleDiv",
                              layout_columns(
                                value_box(
                                  textOutput("dato4"),
                                  title = "Déficit habitacional",
                                  p("N° de hogares (en miles)"),
                                  showcase = icon("house", size = 100),
                                  showcase_layout = "left center",
                                  theme = "success"),
                                value_box(
                                  textOutput("dato5"),
                                  title = "Déficit cuantitativo",
                                  p("N° de hogares (en miles)"),
                                  showcase = icon("house", size = 100),
                                  showcase_layout = "left center",
                                  theme = "success"),
                                value_box(
                                  textOutput("dato6"),
                                  title = "Déficit cualitativo",
                                  p("N° de hogares (en miles)"),
                                  showcase = icon("house", size = 100),
                                  showcase_layout = "left center",
                                  theme = "success")
                              )
                            )
                          )
                        )
                        )
                      ),
             tabPanel("Ficha técnica", fluid = TRUE, icon = icon("chart-bar"))
  )
)


## Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$apply, {
    session$setCurrentTheme(
      bs_theme(
        bg = input$bg,
        fg = input$fg,
        primary = input$primary,
        secondary = input$secondary,
        success = input$success
      )
    )
  })
## Present provincias depending on the selected departamento and distritos depending on the selected provincia.
  observeEvent(input$Dpto, {
    if (input$Dpto == "TODOS") {
      prov_choices <- provincias %>%
        select(provincia) %>%
        arrange(provincia) %>%
        drop_na() %>%
        pull(provincia)
    } else {
      ubigeo_dpto <- departamentos %>%
        filter(departamento == input$Dpto) %>%
        pull(ubigeo)
      prov_choices <- provincias %>%
        filter(substr(ubigeo, 1, 2) == substr(ubigeo_dpto, 1, 2)) %>%
        select(provincia) %>%
        arrange(provincia) %>%
        drop_na() %>%
        pull(provincia)
    }
    prov_choices <- c("TODOS", prov_choices)
    
    updateSelectInput(session, "Prov", choices = prov_choices)
  })
  
  observeEvent(input$Prov, {
    if (input$Prov == "TODOS") {
      dist_choices <- distritos %>%
        select(distrito) %>%
        arrange(distrito) %>%
        drop_na() %>%
        pull(distrito)
    } else {
      ubigeo_prov <- provincias %>%
        filter(provincia == input$Prov) %>%
        pull(ubigeo)
      dist_choices <- distritos %>%
        filter(substr(ubigeo, 1, 4) == substr(ubigeo_prov, 1, 4)) %>%
        select(distrito) %>%
        arrange(distrito) %>%
        drop_na() %>%
        pull(distrito)
    }
    dist_choices <- c("TODOS", dist_choices)
    
    updateSelectInput(session, "Dist", choices = dist_choices)
  })
  
  output$uiProv <- renderUI({
    selectInput(inputId = "Prov",
                label = "Elija la provincia",
                choices = c("TODOS"),
                selected = "TODOS",
                width = "100%")
  })
  
  output$uiDist <- renderUI({
    selectInput(inputId = "Dist",
                label = "Elija el distrito",
                choices = c("TODOS"),
                selected = "TODOS",
                width = "100%")
  })
  
  observeEvent(input$Metodo, {
    if (input$Metodo == "Metodología INEI") {
      ind_choices <- listIndicadores
    } else {
      ind_choices <- listIndicadoresNew
    }

    updateSelectInput(session, "Indicador", choices = ind_choices)
  })
  
  output$uiIndicador <- renderUI({
    selectInput(inputId = "Indicador",
              label = "Seleccione el indicador",
              choices = c("TODOS"),
              selected = "Porcentaje de hogares con déficit habitacional (%)",
              width = "500px")
  })
  ## Elaborate the map for the different indicators
    
  output$map1 <- renderPlotly({
    
    custom_palette <- c("#96d250", "#f2b138", "#c8463c")
    
    if (input$Metodo == "Metodología INEI"){
      df <- data
    } else {
      df <- dataNew
    }
    
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      map_peru <- map_DIST  %>%  #Cargamos la base de datos sobre las provincias del Peru
        rename(ubigeo = COD_DISTRITO) %>% #renombramos la variable del DF para el merge por UBIGEO
        st_as_sf()
      
      alterna <- df  %>%
        select(ubigeo, input$Indicador) 
      
      colnames(alterna) <- c("ubigeo", "indicador")
      
      map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
        mutate(indicador = as.numeric(indicador)) 
      
      # Filtrar solo los distritos de la provincia seleccionada
      
      ubigeo_prov <- provincias %>%
        filter(provincia == input$Prov) %>%
        pull(ubigeo)
      
      map_shiny <- map_shiny %>%
        filter(substr(ubigeo, 1, 4) == substr(ubigeo_prov, 1, 4))
      
      breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
      formatted_labels <- sprintf("(%s, %s]", 
                                  comma(breaks$brks[-length(breaks$brks)]), 
                                  comma(breaks$brks[-1]))
      map_shiny$jenks_breaks <- cut(map_shiny$indicador, 
                                    breaks$brks, 
                                    include.lowest = TRUE, 
                                    labels = formatted_labels)
      
      map_shiny <- map_shiny %>%
        mutate(text = paste(distritos$distrito[match(ubigeo, distritos$ubigeo)],"<br>",
                            input$Indicador, ":", format(round(indicador, 1), big.mark = ",", decimal.mark = ".")))
      
      # Identificar el distrito seleccionado
      
      selected_district <- distritos %>%
        filter(distrito == input$Dist, 
               provincia == input$Prov,
               departamento == input$Dpto) %>%
        pull(ubigeo)
      
      # Filter the map_shiny data frame to highlight the selected district
      map_shiny <- map_shiny %>%
        mutate(is_selected = ubigeo == selected_district)
      
      mapa <- map_shiny %>% 
        ggplot() +
        aes(geometry = geometry) +
        geom_sf(aes(fill = jenks_breaks, text=text), linetype = 1,
                lwd = 0.25) +
        geom_sf(data = map_shiny[map_shiny$is_selected, ], aes(text = text), fill = NA, color = "black", size = 3) +  # Añadir bordes de distritos
        theme_minimal()+
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
        scale_fill_manual(values = custom_palette, name = input$Indicador)
      
      ggplotly(mapa, tooltip = "text") %>% 
        style(hoverinfo = "text", traces = 1)
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      map_peru <- map_DEP  %>%  #Cargamos la base de datos sobre los departamentos del Peru
        rename(ubigeo = COD_DEPARTAMENTO ) %>% #renombramos la variable del DF para el merge por UBIGEO
        st_as_sf()
      
      alterna <- df  %>%
        select(ubigeo, input$Indicador) 
      
      colnames(alterna) <- c("ubigeo", "indicador")
      
      map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
        mutate(indicador = as.numeric(indicador)) 
      
      breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
      formatted_labels <- sprintf("(%s, %s]", 
                                  comma(breaks$brks[-length(breaks$brks)]), 
                                  comma(breaks$brks[-1]))
      map_shiny$jenks_breaks <- cut(map_shiny$indicador, 
                                    breaks$brks, 
                                    include.lowest = TRUE, 
                                    labels = formatted_labels)
      map_shiny <- map_shiny %>%
        mutate(text = paste(departamentos$departamento[match(ubigeo, departamentos$ubigeo)],"<br>",
                            input$Indicador, ":", format(round(indicador, 1), big.mark = ",", decimal.mark = ".")))
      
      # Identificar el departamento seleccionado
      
      selected_departamento <- map_shiny %>% 
        filter(ubigeo == departamentos$ubigeo[departamentos$departamento == input$Dpto])
      
      mapa <- map_shiny %>% 
        ggplot() +
        aes(geometry = geometry) +
        geom_sf(aes(fill = jenks_breaks, text=text), linetype = 1,
                lwd = 0.25) +
        geom_sf(data = selected_departamento, aes(text = text), fill = NA, color = "black", size = 3) +  # Resaltar departamento seleccionado
        theme_minimal()+
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
        scale_fill_manual(values = custom_palette, name = input$Indicador)
      
      ggplotly(mapa, tooltip = "text") %>% 
        style(hoverinfo = "text", traces = 1)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){
      
      map_peru <- map_PROV  %>%  #Cargamos la base de datos sobre las provincias del Peru
        rename(ubigeo = COD_PROVINCIA) %>% #renombramos la variable del DF para el merge por UBIGEO
        st_as_sf()
      
      alterna <- df  %>%
        select(ubigeo, input$Indicador) 
      
      colnames(alterna) <- c("ubigeo", "indicador")
      
      map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
        mutate(indicador = as.numeric(indicador)) 
      
      # Filtrar solo las provincias del departamento seleccionado
      
      ubigeo_dpto <- departamentos %>% 
        filter(departamento == input$Dpto) %>% 
        pull(ubigeo)
      
      # Caso de Callao donde solo hay una provincia
      
      if (ubigeo_dpto == "070000") {
        # Incluir Lima (150000) si es Callao
        map_shiny <- map_shiny %>%
          filter(substr(ubigeo, 1, 2) %in% c(substr(ubigeo_dpto, 1, 2), "15"))
      } else {
        map_shiny <- map_shiny %>%
          filter(substr(ubigeo, 1, 2) == substr(ubigeo_dpto, 1, 2))
      }
      
      # Continuación del código para generar los breaks y el mapa
      
      breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
      formatted_labels <- sprintf("(%s, %s]", 
                                  comma(breaks$brks[-length(breaks$brks)]), 
                                  comma(breaks$brks[-1]))
      map_shiny$jenks_breaks <- cut(map_shiny$indicador, 
                                    breaks$brks, 
                                    include.lowest = TRUE, 
                                    labels = formatted_labels)
      
      map_shiny <- map_shiny %>%
        mutate(text = paste(provincias$provincia[match(ubigeo, provincias$ubigeo)],"<br>",
                            input$Indicador, ":", format(round(indicador, 1), big.mark = ",", decimal.mark = ".")))
      
      selected_provincia <- provincias %>%
        filter(provincia == input$Prov,
               departamento == input$Dpto) %>%
        pull(ubigeo)
      
      # Filter the map_shiny data frame to highlight the selected district
      map_shiny <- map_shiny %>%
        mutate(is_selected = ubigeo == selected_provincia)
      
      mapa <- map_shiny %>% 
        ggplot() +
        aes(geometry = geometry) +
        geom_sf(aes(fill = jenks_breaks, text = text), linetype = 1,
                lwd = 0.25) +
        geom_sf(data = map_shiny[map_shiny$is_selected, ], aes(text = text), fill = NA, color = "black", size = 3) + # Resaltar provincia seleccionada
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
        scale_fill_manual(values = custom_palette, name = input$Indicador)
      
      # Convertir el ggplot a un objeto plotly
      ggplotly(mapa, tooltip = "text") %>% 
        style(hoverinfo = "text", traces = 1)
      
    } else {
      print("Mapa del Perú por regiones ")
      
      map_peru <- map_DEP  %>%  #Cargamos la base de datos sobre los departamentos del Peru
        rename(ubigeo = COD_DEPARTAMENTO ) %>% #renombramos la variable del DF para el merge por UBIGEO
        st_as_sf()
      
      alterna <- df  %>%
        select(ubigeo, input$Indicador) 
      
      colnames(alterna) <- c("ubigeo", "indicador")
      
      map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
        mutate(indicador = as.numeric(indicador)) 
      
      breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
      formatted_labels <- sprintf("(%s, %s]", 
                                  comma(breaks$brks[-length(breaks$brks)]), 
                                  comma(breaks$brks[-1]))
      map_shiny$jenks_breaks <- cut(map_shiny$indicador, 
                                    breaks$brks, 
                                    include.lowest = TRUE, 
                                    labels = formatted_labels)
      
      map_shiny <- map_shiny %>%
        mutate(text = paste(departamentos$departamento[match(ubigeo, departamentos$ubigeo)],"<br>",
                            input$Indicador, ":", format(round(indicador, 1), big.mark = ",", decimal.mark = ".")))
      
      mapa <- map_shiny %>% 
        ggplot() +
        aes(geometry = geometry) +
        geom_sf(aes(fill = jenks_breaks, text=text), linetype = 1,
                lwd = 0.25) +
        theme_minimal()+
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
        scale_fill_manual(values = custom_palette, name = input$Indicador)
      
      ggplotly(mapa, tooltip = "text") %>% 
        style(hoverinfo = "text", traces = 1)
      
    }
  }) 
  
  output$dato1 <- renderPrint({
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      df <- distritos %>% 
        filter(departamento == input$Dpto & provincia == input$Prov & distrito == input$Dist)
      
      valor <- sum(df$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit habitacional (%)`, w = df$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      df <- departamentos %>% 
        filter(departamento == input$Dpto)
      
      valor <- sum(df$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit habitacional (%)`, w = df$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){  
      
      df <- provincias %>% 
        filter(departamento == input$Dpto & provincia == input$Prov)
      
      valor <- sum(df$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit habitacional (%)`, w = df$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
      
    } else {
      
      valor <- sum(departamentos$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(departamentos$`Porcentaje de hogares con déficit habitacional (%)`, w = departamentos$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
    }
    
 
    
  })
  
  output$dato2 <- renderPrint({
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      df <- distritos %>% 
        filter(departamento == input$Dpto & provincia == input$Prov & distrito == input$Dist)
      
      valor <- sum(df$`Número de hogares con déficit cuantitativo (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cuantitativo (%)`, w = df$`Número de hogares con déficit cuantitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      df <- departamentos %>% 
        filter(departamento == input$Dpto)
      
      valor <- sum(df$`Número de hogares con déficit cuantitativo (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cuantitativo (%)`, w = df$`Número de hogares con déficit cuantitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){  
      
      df <- provincias %>% 
        filter(departamento == input$Dpto & provincia == input$Prov)
      
      valor <- sum(df$`Número de hogares con déficit cuantitativo (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cuantitativo (%)`, w = df$`Número de hogares con déficit cuantitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
      
    } else {
      
      valor <- sum(departamentos$`Número de hogares con déficit cuantitativo (en miles)`)
      pct <- weighted.mean(departamentos$`Porcentaje de hogares con déficit cuantitativo (%)`, w = departamentos$`Número de hogares con déficit cuantitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
    }
    
  })
  
  output$dato3 <- renderPrint({
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      df <- distritos %>% 
        filter(departamento == input$Dpto & provincia == input$Prov & distrito == input$Dist)
      
      valor <- sum(df$`Número de hogares con déficit cualitativo (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cualitativo (%)`, w = df$`Número de hogares con déficit cualitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = ".")) 
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      #cat(paste0("N° de hogares: ", formatted_value, "\nPorcentaje: ", formatted_pct))
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      df <- departamentos %>% 
        filter(departamento == input$Dpto)
      
      valor <- sum(df$`Número de hogares con déficit cualitativo (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cualitativo (%)`, w = df$`Número de hogares con déficit cualitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){  
      
      df <- provincias %>% 
        filter(departamento == input$Dpto & provincia == input$Prov)
      
      valor <- sum(df$`Número de hogares con déficit cualitativo (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cualitativo (%)`, w = df$`Número de hogares con déficit cualitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
      
    } else {
      
      valor <- sum(departamentos$`Número de hogares con déficit cualitativo (en miles)`)
      pct <- weighted.mean(departamentos$`Porcentaje de hogares con déficit cualitativo (%)`, w = departamentos$`Número de hogares con déficit cualitativo (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    }
    
  })
  
  output$dato4 <- renderPrint({
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      df <- distritosNew %>% 
        filter(departamento == input$Dpto & provincia == input$Prov & distrito == input$Dist)
      
      valor <- sum(df$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit habitacional (%)`, w = df$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      df <- departamentosNew %>% 
        filter(departamento == input$Dpto)
      
      valor <- sum(df$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit habitacional (%)`, w = df$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){  
      
      df <- provinciasNew %>% 
        filter(departamento == input$Dpto & provincia == input$Prov)
      
      valor <- sum(df$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit habitacional (%)`, w = df$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
      
    } else {
      
      valor <- sum(departamentosNew$`Número de hogares con déficit habitacional (en miles)`)
      pct <- weighted.mean(departamentosNew$`Porcentaje de hogares con déficit habitacional (%)`, w = departamentosNew$`Número de hogares con déficit habitacional (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
    }
    
    
    
  })
  
  output$dato5 <- renderPrint({
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      df <- distritosNew %>% 
        filter(departamento == input$Dpto & provincia == input$Prov & distrito == input$Dist)
      
      valor <- sum(df$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cuantitativo de vivienda (%)`, w = df$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      df <- departamentosNew %>% 
        filter(departamento == input$Dpto)
      
      valor <- sum(df$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cuantitativo de vivienda (%)`, w = df$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){  
      
      df <- provinciasNew %>% 
        filter(departamento == input$Dpto & provincia == input$Prov)
      
      valor <- sum(df$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cuantitativo de vivienda (%)`, w = df$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
      
    } else {
      
      valor <- sum(departamentosNew$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      pct <- weighted.mean(departamentosNew$`Porcentaje de hogares con déficit cuantitativo de vivienda (%)`, w = departamentosNew$`Número de hogares con déficit cuantitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
    }
    
  })
  
  output$dato6 <- renderPrint({
    
    if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
      
      df <- distritosNew %>% 
        filter(departamento == input$Dpto & provincia == input$Prov & distrito == input$Dist)
      
      valor <- sum(df$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cualitativo de vivienda (%)`, w = df$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = ".")) 
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      #cat(paste0("N° de hogares: ", formatted_value, "\nPorcentaje: ", formatted_pct))
      
    } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
      
      df <- departamentosNew %>% 
        filter(departamento == input$Dpto)
      
      valor <- sum(df$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cualitativo de vivienda (%)`, w = df$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){  
      
      df <- provinciasNew %>% 
        filter(departamento == input$Dpto & provincia == input$Prov)
      
      valor <- sum(df$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      pct <- weighted.mean(df$`Porcentaje de hogares con déficit cualitativo de vivienda (%)`, w = df$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
      
    } else {
      
      valor <- sum(departamentosNew$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      pct <- weighted.mean(departamentosNew$`Porcentaje de hogares con déficit cualitativo de vivienda (%)`, w = departamentosNew$`Número de hogares con déficit cualitativo de vivienda (en miles)`)
      
      formatted_value <- paste(format(round(valor, 1), big.mark = ",", decimal.mark = "."))
      formatted_pct <- paste0(sprintf("%.2f", pct), "%")
      
      cat(formatted_value)
      
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)