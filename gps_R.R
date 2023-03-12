library(shiny) 
library(leaflet)
library("leaflet.extras")
library(bslib)
library(osrm)
library(sf)
library(shinycssloaders)

options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 2)
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti", base_font = font_google("Montserrat")),
  title = "Falcon-6 Rocket Design - GPS",
  navbarPage(
    title = "Falcon-6 - Recuperação do foguete",
    id = "navbarID",
    tabPanel(
      title = "Início",
      icon = icon("house", lib = "font-awesome"),
      sidebarLayout(
        sidebarPanel(
          h3(strong("Insira as coordenadas geográficas iniciais e finais do foguete")),
          br(),
          numericInput("lat1", "Latitude do ponto de partida:", value = -19.67, step = 0.0001, width = 600),
          numericInput("lon1", "Longitude do ponto de partida:", value = -43.21241653929604, step = 0.0001, width = 600),
          br(), hr(),  br(),
          numericInput("lat2", "Latitude final do foguete:", value = -19.67194858052724, step = 0.0001, width = 600),
          numericInput("lon2", "Longitude final do foguete:", value = -43.21241653929604, step = 0.0001, width = 600),
        ),
        mainPanel(
          withSpinner(leafletOutput("map", width = "100%", height = "900px"), type = 3)
        )
      )
    ),
    tabPanel(
      title = "Sobre Nós",
      icon = icon("fa-regular fa-address-card", lib = "font-awesome"),
      includeMarkdown("md/about.md")
    )
  ), inverse = TRUE
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({

  coords <- reactiveValues(lat = c(input$lat1, input$lat2),
                             lon = c(input$lon1, input$lon2))
  rocket <-  makeAwesomeIcon(
    icon = "home",
    markerColor = "red",
    library = "fa",
  )
  parachute <-  makeAwesomeIcon(
    icon = "rocket",
    markerColor = "green",
    library = "fa",
    spin = TRUE
  )
  
  leaflet() %>%
     
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addAwesomeMarkers(lat = coords$lat[1], lng = coords$lon[1], label = "Ponto de partida do foguete", icon = rocket) %>%
      addAwesomeMarkers(lat = coords$lat[2], lng = coords$lon[2], label = "Onde está o nosso foguete agora", icon = parachute) %>%
      addCircleMarkers(lat = coords$lat[1], lng = coords$lon[1], 
                     radius = 15, stroke = FALSE, fillOpacity = 0.5)%>%
      addCircleMarkers(lat = coords$lat[2], lng = coords$lon[2], 
                     radius = 15, stroke = FALSE, fillOpacity = 0.5)%>%
      addPolylines(lat = coords$lat, lng = coords$lon, color = "#00FFFF")%>%
      addLayersControl(overlayGroups = c("Satélite"),
                   options = layersControlOptions(collapsed = FALSE), position = "topleft")%>%
     addMiniMap(zoomAnimation = TRUE)%>%
     addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")%>%
     addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Me localize",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")))%>%
     #addSearchOSM()%>%
     addReverseSearchOSM()
  })
  
}
  
shinyApp(ui, server)
