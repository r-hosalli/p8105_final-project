library(tidyverse)
library(rlang)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinyWidgets)

##data
pa_combined <- readRDS("pa_combined.RDS")
superfund_sites <- readRDS("superfund_sites.RDS")



ui = fluidPage(
  titlePanel("Health Outcomes by PA County with Superfund Sites"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Asthma", leafletOutput("asthma")),
      tabPanel("Cancer", leafletOutput("cancer"))
    )
  )
)

## Creates a html label for each measure to use in leaflet
label_function <- function(measure, measure_name){
  
  measure = deparse(substitute(measure))
  measure_name = deparse(substitute(measure_name))
  
  sprintf('<strong>%s</strong> 
                <br/> Total Population: %g 
                <br/> Number of Superfund Sites: %g
                <br/> Mean Site Score: %g
                <br/> <strong>Adjusted %s Prevalence: %g</strong>',
          pa_combined$county, 
          pa_combined$total_population, 
          pa_combined$n_superfund,
          pa_combined$mean_site_score,
          measure_name,
          pa_combined[[measure]]) %>% lapply(htmltools::HTML)
}

server <- function(input, output){
  output$asthma <- renderLeaflet({
   
    m1_labs <-label_function(casthma_adj_prev, Asthma)
    
    m1pal <- colorBin("YlOrBr", 6,  domain = pa_combined$casthma_adj_prev)
    
    leaflet(data = superfund_sites) %>% 
      addTiles() %>% 
      addPolygons(data = pa_combined,
                  label = m1_labs,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "1px 2px"),
                    textsize = "11px",  sticky = TRUE
                  ),
                  stroke = TRUE,
                  color = "black",
                  weight = 1,
                  opacity = 1,
                  smoothFactor = .5,
                  fillOpacity = .7,
                  fillColor = ~m1pal(casthma_adj_prev),
                  highlightOptions = highlightOptions(weight = 5, 
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1,
                                                      bringToFront = TRUE
                  )) %>% 
      addLegend(data = pa_combined,
                "bottomright",
                pal = m1pal,
                values = ~casthma_adj_prev,
                title = "Prevalence (%)",
                opacity = .7)%>% 
      addMarkers(label = ~htmlEscape(site_name))
    
  })
  
  output$cancer <- renderLeaflet({
    m2_labs <-label_function(cancer_adj_prev, Cancer)
    m2pal <- colorBin("YlOrBr", 6, domain = pa_combined$cancer_adj_prev)
    
    leaflet(data = superfund_sites) %>% 
      addTiles() %>% 
      addPolygons(data = pa_combined,
                  label = m2_labs,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "1px 2px"),
                    textsize = "11px",  sticky = TRUE
                  ),
                  stroke = TRUE,
                  color = "black",
                  weight = 1,
                  opacity = 1,
                  smoothFactor = .5,
                  fillOpacity = .7,
                  fillColor = ~m2pal(cancer_adj_prev),
                  highlightOptions = highlightOptions(weight = 5, 
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1,
                                                      bringToFront = TRUE
                  )) %>% 
      addLegend(data = pa_combined,
                "bottomright",
                pal = m2pal,
                values = ~cancer_adj_prev,
                title = "Prevalence (%)",
                opacity = .7)%>% 
      addMarkers(label = ~htmlEscape(site_name))
  })
}


shinyApp(ui = ui, server = server)
              