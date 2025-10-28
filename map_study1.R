# Title: COVID-19 Serology Multicentre Collaborative Shiny Application
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2023-Jan-17
# Updated: 2024-Feb-27
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5


# UI ----------------------------------------------------------------------------------------
map_study1UI <- function(id) {
    ns <- NS(id)

    layout_columns(style = "padding-top: 10px",
           col_widths = c(4, 8), height = "82vh",
           fill = TRUE, fillable = TRUE,
           card(style = "background-color: #D4DADC;",
               card_body(
                   HTML("<p style = 'font-size: 115%; color: #363538'><i>Click on a marker to view site specific information and update the summary below.</i><br></p>"),
                   
                    div(class = "card",
                        style = "background-color: #F6F6F6; padding: 15px; line-height: 125%",
                        span(textOutput(ns("summary")),
                             style = "font-size: 120%; font-weight: bold; color: #363538"), #display: inline-block;
                        span(HTML("&nbsp;"), shiny::icon("users"), 
                             HTML("&nbsp;"), textOutput(ns("sum_participants"), inline = TRUE),
                             style = "margin-top: 12px;"),
                        span(HTML("&nbsp;"), shiny::icon("vials"),
                             HTML("&nbsp;&nbsp;"), textOutput(ns("sum_samples"), inline = TRUE),
                             style = "margin-top: 6px;")
                    ),

                   card(style = "background-color: #F6F6F6;",
                       card_body(
                           plotlyOutput(ns("plot"), width = "100%") 
                           )
                   )
               )
           ),
           
           card(
               card_body(
                   leafletOutput(ns("map"), height = "83vh") %>% withSpinner(),
                   as_fill_carrier(),
                   class = "p-0"
               )
           )
    )
} 


# Server ----------------------------------------------------------------------------------------
map_study1Server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        thematic_shiny()
        
   
        # default summary text and plot in side bar
        output$summary <- renderText({"Summary for all sites:"})
        output$sum_participants <- renderText({paste0("  ",length(unique(serology$PatientID)), " participants")})
        output$sum_samples <- renderText({paste0("  ", nrow(serology[!is.na(serology$Assay),]), " samples")})

        output$plot <- renderPlotly({
            validate(need(input$map_marker_click$id, 'Please select a site by clicking on a map marker to view plot'))
            serology %>%
                filter(!is.na(RBD_Value)) %>% 
                filter(!is.na(Serology_Date)) %>%
                spike_calendar(.)%>%
                layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6",
                       legend = list(orientation = "h", y = -0.4, yref = "paper"))})
        
        
        # map with markers
        output$map <- renderLeaflet({
            leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 2, preferCanvas = TRUE)) %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                setView(lng = 5, lat = 25, zoom = 2) %>% 
                addPolygons(data = regData,
                            stroke = TRUE,
                            color = "#363538",
                            opacity = 1,
                            weight = 1,
                            fillColor = "#408697",
                            fillOpacity = 0.8,
                            highlightOptions = highlightOptions(color = "#363538", weight = 2, bringToFront = FALSE, opacity = 1)) %>% 
            addAwesomeMarkers(data = regData,
                              lat = regData$marker_lat,
                              lng = regData$marker_long,
                              layerId = regData$site, # LH: likely going to have to create JSON for each site
                              icon = icons,
                              label = regData$format_name,
                              popup = paste(regData$popup),
                              popupOptions = list(minWidth = 350, maxHeight = 250))
        })
    
        # update plot when a marker is clicked
        observeEvent(input$map_marker_click, {
    
            if (is.null(input$map_marker_click)) {
                return()
            }
            
            # use region data to subset across all maps
            click_data <- reactive({
                regData %>%
                    filter(regData$site == input$map_marker_click$id)
            })
            
            # filter sero data by marker id
            # strip_react <- reactive({
            #     serology %>% 
            #         filter(Vaccine_Status != "", Vaccine_Status != "Post-5th") %>% 
            #         filter(Site == input$map_marker_click$id)
            # })
            
            serology_react <- reactive({
                serology %>%
                    filter(Site == input$map_marker_click$id)
            })
            
            output$summary <- renderText({paste0("Summary for ", unique(serology_react()$Site_format), ":")})
            output$sum_participants <- renderText({paste0(length(unique(serology_react()$PatientID)), " participants")})
            output$sum_samples <- renderText({paste0(nrow(serology_react()[!is.na(serology_react()$Assay),]), " samples")})
            
            output$plot <- renderPlotly({
                validate(need(input$map_marker_click$id, 'Please select a site by clicking on a map marker to view plot'))
                serology_react() %>% 
                    filter(!is.na(RBD_Value)) %>% 
                        spike_calendar(.) %>% 
                            layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6",
                                   legend = list(orientation = "h", y = -0.4, yref = "paper"))})
            
    
        })
        
        # Reset
        observeEvent(input$map_click, {
            #removeShape(leafletProxy("map"), layerId = "clicked")
            
            output$summary <- renderText({"Summary for all sites:"})
            output$sum_participants <- renderText({paste0(length(unique(serology$PatientID)), " participants")})
            output$sum_samples <- renderText({paste0(nrow(serology[!is.na(serology$Assay),]), " samples")})
            
            output$plot <- renderPlotly({
                validate(need(input$site, 'Please select a site by clicking on a map marker to view plot'))
                serology %>%
                    filter(!is.na(RBD_Value)) %>% 
                    spike_calendar(.) %>%
                    layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6",
                           legend = list(orientation = "h", y = -0.4, yref = "paper"))
            })
        })
    
        observeEvent(input$map_shape_click, {
            #removeShape(leafletProxy("map"), layerId = "clicked")
            
            output$summary <- renderText({"Summary for all sites:"})
            output$sum_participants <- renderText({paste0(length(unique(serology$PatientID)), " participants")})
            output$sum_samples <- renderText({paste0(nrow(serology[!is.na(serology$Assay),]), " samples")})
            
            output$plot <- renderPlotly({
                validate(need(input$site, 'Please select a site by clicking on a map marker to view plot'))
                serology %>%
                    filter(!is.na(RBD_Value)) %>% 
                    spike_calendar(.) %>%
                    layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6",
                           legend = list(orientation = "h", y = -0.4, yref = "paper"))
            })
        })

    }) # server

}
