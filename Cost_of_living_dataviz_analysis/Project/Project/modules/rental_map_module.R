# modules/rental_map_module.R

rentalMapUI =  function(id) {
  ns =  NS(id)
  leafletOutput(ns("map"), height = 600)
}

rentalMapServer =  function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns =  session$ns

    output$map =  renderLeaflet({
      req(data())
      leaflet(data()) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 44.5126, lat = 40.1792, zoom = 13) %>%
        addMarkers(
          ~Longitude, ~Latitude,
          popup = ~Popup_Info,
          clusterOptions = markerClusterOptions()
        )
    })

    selected_listing =  reactive({
      req(input$map_marker_click)
      click =  input$map_marker_click
      data() %>%
        filter(abs(Longitude - click$lng) < 1e-6, abs(Latitude - click$lat) < 1e-6)
    })

    return(selected_listing)
  })
}
