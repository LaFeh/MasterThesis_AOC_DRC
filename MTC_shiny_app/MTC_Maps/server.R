#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$plot_built_1 <- renderLeaflet({
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles() %>%  # OpenStreetMap
        addPolygons(
          data = st_transform(drc,4326),
          fillColor = "grey",
          fillOpacity = 0,
          color = "black",
          weight = 1
        )%>%  # OpenStreetMap
        addPolygons(
          data = st_transform(rwa,4326),
          fillColor = "grey",
          fillOpacity = 0,
          color = "black",
          weight = 1
        )%>%  # OpenStreetMap
        addPolygons(
          data = st_transform(relevant_regions,4326),
          fillColor = "purple",
          fillOpacity = 0.4,
          color = "black",
          weight = 1
        )

    })

}
