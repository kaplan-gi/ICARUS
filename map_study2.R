# Title: COVID-19 Serology Multicentre Collaborative Shiny Application
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2023-Jan-17
# Updated: 2024-Feb-27
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5


# UI ----------------------------------------------------------------------------------------
map_study2UI <- function(id) {
  
  ns <- NS(id)

  layout_columns(
    style = "padding-top: 10px",
    col_widths = c(4, 8), 
    height = "82vh",
    fill = TRUE,
    fillable = TRUE,
           
    card(
      style = "background-color: #D4DADC;",
                
      card_body(
                
        HTML("<p style = 'font-size: 115%; color: #363538'><i>Click on a marker to view site specific information and update the summary below.</i><br></p>"),
                     
        div(
          class = "card",
          style = "background-color: #F6F6F6; padding: 15px; line-height: 125%",
          span(
            textOutput(ns("summary")),
            style = "font-size: 120%; font-weight: bold; color: #363538"
          ), #display: inline-block;
          span(
            HTML("&nbsp;"), 
            shiny::icon("users"),
            HTML("&nbsp;"), 
            textOutput(ns("sum_participants"), inline = TRUE),
            style = "margin-top: 12px;"
          )
        ),

        card(
          style = "background-color: #F6F6F6;",
          full_screen = TRUE, 
          card_body(
            uiOutput(ns("plotN")),
            uiOutput(ns("plotS"))
          )
        )
      )
    ),
           
    card(
      card_body(
        leafletOutput(ns("map"), 
                      height = "83vh") %>% 
          withSpinner(),
          as_fill_carrier(),
          class = "p-0"
      )
    )
  )
  
} 


# Server ----------------------------------------------------------------------------------------
map_study2Server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
      
  thematic_shiny()
        
  # default summary text and plot in side bar
  output$summary <- renderText({"All sites"})
  output$sum_participants <- renderText({paste0("  ", table_raw$n_cohort[table_raw$site == "overall"], " participants")})

  # Roche N plot default all data
  output$plotN <- renderUI({
    img_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/all_data_N_p.png"
    tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
  })
  
  # Roche S plot default all data
  output$plotS <- renderUI({
    img_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/all_data_S_p.png"
    tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
  })
  
  # style = "width:100%; height:auto; min-height: 400px; max-height: 800px; object-fit:contain; image-rendering:auto;"
        
  # map with markers
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 2, preferCanvas = TRUE)) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng = 5, lat = 25, zoom = 2) %>% 
      addPolygons(
        data = mergedStudy2,
        stroke = TRUE,
        color = "#363538",
        opacity = 1,
        weight = 1,
        fillColor = "#408697",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(color = "#363538", weight = 2, bringToFront = FALSE, opacity = 1)) %>% 
      addAwesomeMarkers(
        data = mergedStudy2,
        lat = mergedStudy2$marker_lat,
        lng = mergedStudy2$marker_long,
        layerId = mergedStudy2$site,
        icon = icons,
        label = mergedStudy2$format_name,
        popup = paste(mergedStudy2$popup),
        popupOptions = list(minWidth = 350, maxHeight = 250))
  })
    
  # update plot when a marker is clicked
  observeEvent(input$map_marker_click, {
 
    if (is.null(input$map_marker_click)) {
      return()
    }
    
    # use region data to subset across all maps
    click_data <- reactive({
      mergedStudy2 %>%
        filter(mergedStudy2$site == input$map_marker_click$id)
    })
    
    map_react <- reactive({
      mergedStudy2 %>%
        filter(site == input$map_marker_click$id)
    })
     
    output$summary <- renderText({map_react()$format_name})
    output$sum_participants <- renderText({paste0(map_react()$n_cohort, " participants")})
    
    # Roche N plot default all data
    output$plotN <- renderUI({
      img_url <- map_react()$plot_URL_RocheN
      tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
    })

    # Roche S plot default all data
    output$plotS <- renderUI({
      img_url <- map_react()$plot_URL_RocheS
      tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
    })
     
  })
  
  # Reset
  observeEvent(input$map_click, {
    
    output$summary <- renderText({"All sites"})
    output$sum_participants <- renderText({paste0("  ", table_raw$n_cohort[table_raw$site == "overall"], " participants")})
    
    # Roche N plot default all data
    output$plotN <- renderUI({
      img_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/all_data_N_p.png"
      tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
    })
    
    # Roche S plot default all data
    output$plotS <- renderUI({
      img_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/all_data_S_p.png"
      tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
    })
    
  })

  observeEvent(input$map_shape_click, {

    output$summary <- renderText({"All sites"})
    output$sum_participants <- renderText({paste0("  ", table_raw$n_cohort[table_raw$site == "overall"], " participants")})

    # Roche N plot default all data
    output$plotN <- renderUI({
      img_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/all_data_N_p.png"
      tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
    })
    
    # Roche S plot default all data
    output$plotS <- renderUI({
      img_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/all_data_S_p.png"
      tags$img(src = img_url, style = "width:100%; height:auto; object-fit:contain; image-rendering:auto;")
    })
    
  })

  }) # server

}
