# Title: COVID-19 Serology Multicentre Collaborative Shiny Application (Demographics tab)
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2023-Jan-17
# Updated: 2024-Feb-27
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5


# UI -------------------------------------------------------------------------------------------
demographics_study2UI <- function(id) {
    ns <- NS(id)
    
    page_fillable(
      
        layout_columns(
            style = "padding-top: 10px",
            col_widths = c(4, 3, 5), height = "20vh",
            fill = TRUE, fillable = TRUE,
            
            pickerInput(inputId = ns("site_picker"), 
                        label = HTML("<span style = 'font-size: 115%; color: #363538'><i>Select study sites to customize the dashboard:</i></span>"),
                        choices = list(
                          "University Hospitals Leuven, Leuven, Belgium" = "Leuven",
                          "Mount Sinai Hospital, University of Toronto, Toronto, Canada" = "Toronto",
                          "Hospital Saint-Louis, Paris, France" = "Paris",
                          "Chinese University of Hong Kong - Prince of Wales Hospital, Hong Kong" = "Hong_Kong",
                          "All India Institute of Medical Sciences, New Delhi, India" = "New_Delhi",
                          "Postgraduate Institute of Medical Education and Research, Chandigarh, India" = "Chandigarh",
                          "Hyogo College of Medicine, Nishinomiya, Japan" = "Nishinomiya",
                          "Örebro University Hospital, Örebro, Sweden" = "Orebro",
                          "University of Oxford, Oxford, UK" = "Oxford",
                          "Royal London Hospital, London, UK" = "London",
                          "Icahn School of Medicine at Mount Sinai, New York, USA" = "New_York",
                          "University of Miami, Miami, USA" = "Miami",
                          "University of Chicago, Chicago, USA" = "Chicago"
                        ), 
                        selected = list(
                          "University Hospitals Leuven, Leuven, Belgium" = "Leuven",
                          "Mount Sinai Hospital, University of Toronto, Toronto, Canada" = "Toronto",
                          "Hospital Saint-Louis, Paris, France" = "Paris",
                          "Chinese University of Hong Kong - Prince of Wales Hospital, Hong Kong" = "Hong_Kong",
                          "All India Institute of Medical Sciences, New Delhi, India" = "New_Delhi",
                          "Postgraduate Institute of Medical Education and Research, Chandigarh, India" = "Chandigarh",
                          "Hyogo College of Medicine, Nishinomiya, Japan" = "Nishinomiya",
                          "Örebro University Hospital, Örebro, Sweden" = "Orebro",
                          "University of Oxford, Oxford, UK" = "Oxford",
                          "Royal London Hospital, London, UK" = "London",
                          "Icahn School of Medicine at Mount Sinai, New York, USA" = "New_York",
                          "University of Miami, Miami, USA" = "Miami",
                          "University of Chicago, Chicago, USA" = "Chicago"
                        ), 
                        multiple = TRUE, options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE, width = "auto")
            ),
  
            value_box(
              style = "background-color: #D4DADC;",
              title = "Sites:",
              value = textOutput(ns("sites")),
              p(textOutput(ns("participants"))),
              p(textOutput(ns("samples"))),
              showcase = bs_icon("clipboard-data"),
              showcase_layout = showcase_left_center(width = 0.5)
            ),
              
            card(
                full_screen = TRUE,
                card_body(
                    leafletOutput(ns("map")),
                    as_fill_carrier(),
                    class = "p-0"
                )
            )
        ),
        
        layout_columns(
            col_widths = c(6,3,3), height = "60vh",
            fill = TRUE, fillable = TRUE,
                       
            # DEMOGRAPHICS
            card(style = "background-color: #F6F6F6;",
                #card_header("Demographics"),
                card_body(
                    layout_column_wrap(
                        width = 1/2,
                        card(card_body(style = "padding-bottom: 0px;", plotlyOutput(ns("disease")))),
                        card(card_body(style = "padding-bottom: 0px;", plotlyOutput(ns("sex"))))
                    ),
                    card(card_body(plotlyOutput(ns("BMI"))))
                )
            ),
            
            # MEDICATIONS
            card(style = "background-color: #F6F6F6;",
                #card_header("Medications"),
                card_body(
                  card(
                    card_body(style = "padding-left: 4px; padding-right: 4px;",
                              plotlyOutput(ns("medications")))
                  )
                )
            ),
            
            # VACCINES
            card(
                style = "background-color: #F6F6F6;",
                card_body(

                    card(
                      card_body(
                        style = "padding-top: -20px; padding-left: 4px; padding-right: 4px;",
                        plotlyOutput(ns("vaccines")))
                    )
                )
            )
        ) #layout_columns2
    ) #page
} 


# Server ----------------------------------------------------------------------------------------
demographics_study2Server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
      
        study2_react <- reactive({
            mergedStudy2 %>%
                filter(site %in% input$site_picker)
        })
        
        map_react <- reactive({
          validate(
            need(input$site_picker != "", "Please select a site"))
          mergedStudy2 %>% 
            st_drop_geometry %>% 
            filter(site %in% input$site_picker)
        })
        
        output$map <- renderLeaflet({
          leaflet(options = leafletOptions(worldCopyJump = TRUE, preferCanvas = TRUE)) %>%
              addProviderTiles("CartoDB.Positron") %>%
              setView(lng = -5, lat = 40, zoom = 1.3) %>%
              addCircleMarkers(data = mergedStudy2,
                               fillColor = "gray",
                               stroke = FALSE,
                               fillOpacity = 0.8,
                               lat = mergedStudy2$marker_lat,
                               lng = mergedStudy2$marker_long,
                               layerId = mergedStudy2$site,
                               label = mergedStudy2$format_name) %>% 
              addAwesomeMarkers(data = map_react(),
                                lat = map_react()$marker_lat,
                                lng = map_react()$marker_long,
                                icon =  icons,
                                label = map_react()$format_name)
        })

        
        output$sites <- renderText({paste0(length(unique(study2_react()$site)), " of ", length(unique(mergedStudy2$site)))})
        output$participants <- renderText({paste0("Participants: ", sum(study2_react()$n_cohort))})

        output$disease <- renderPlotly({

          data <- study2_react()

          totals <- data %>%
            st_drop_geometry() %>%
            select(CD, UC, IBD.U) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 

            plot_ly(
              labels = names(totals),
              values = as.numeric(totals[1, ]),
              marker = list(colors = c(
                viridis(n = 14, option = "D")[3],
                viridis(n = 14, option = "D")[7],
                viridis(n = 14, option = "D")[11])),
              texttemplate = paste("%{label}", "%{percent:.1%}"),
              textposition = "outside",
              customdata = as.numeric(totals[1, ]),
              hovertemplate = paste(
                "<b>%{label}</b>: %{percent:.1%}",
                "<br>n = %{customdata}",
                "<extra></extra>")) %>%
              add_pie(hole = 0.4) %>%
              layout(
                title = "<b>IBD Type</b>",
                showlegend = FALSE,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                margin = list(l = 30, r = 30, b = 30),
                font = list(family = "Ubuntu")) %>%
              config(displayModeBar = FALSE)
        })

        output$sex <- renderPlotly({
          
          data <- study2_react()
          
          totals <- data %>%
            st_drop_geometry() %>%
            select(female, male) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
          
          plot_ly(
            labels = tools::toTitleCase(names(totals)),
            values = as.numeric(totals[1, ]),
            marker = list(colors = c(
              viridis(n = 14, option = "D")[3],
              viridis(n = 14, option = "D")[7],
              viridis(n = 14, option = "D")[11])),
            texttemplate = paste("%{label}", "%{percent:.1%}"),
            textposition = "outside",
            customdata = as.numeric(totals[1, ]),
            hovertemplate = paste(
              "<b>%{label}</b>: %{percent:.1%}",
              "<br>n = %{customdata}",
              "<extra></extra>")) %>%
            add_pie(hole = 0.4) %>%
            layout(
              title = "<b>Sex</b>",
              showlegend = FALSE,
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              margin = list(l = 30, r = 30, b = 30),
              font = list(family = "Ubuntu")) %>%
            config(displayModeBar = FALSE)
        })
        
        output$BMI <- renderPlotly({
          
          data <- study2_react()
          
          totals <- data %>%
            st_drop_geometry() %>%
            select(underweight, healthy_weight, overweight, obese) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
            rename(
              "Underweight" = underweight,
              "Healthy weight" = healthy_weight,
              "Overweight" = overweight,
              "Obese" = obese
            )
          
          labels <- names(totals)
          counts <- as.numeric(totals[1, ])
          percents <- 100 * counts / sum(counts)
  
          plot_ly(
            x = labels,
            y = percents,
            type = "bar",
            marker = list(color = viridis(n = 14, option = "D")[11]),
            text = paste0(round(percents, 1), "%"),
            textposition = "outside",
            customdata = counts,
            hovertemplate = paste(
              "<b>%{x}</b>: %{y:.1f}%",
              "<br>n = %{customdata}",
              "<extra></extra>")) %>%
            layout(
              title = "<b>BMI Distribution</b>",
              showlegend = FALSE,
              xaxis = list(title = "BMI category"),
              yaxis = list(title = "Percent of participants", 
                           ticksuffix = "%",
                           range = c(0, max(percents) * 1.15) ),
              font = list(family = "Ubuntu")) %>%
            config(displayModeBar = FALSE)
        })


        # Check for labeling horizontal barplot: https://community.plotly.com/t/labels-above-horizontal-bars-better-way-than-subplots/70693/7
        output$medications <- renderPlotly({
          
          data <- study2_react()
          
          totals <- data %>%
            st_drop_geometry() %>%
            select(antiTNF_no_imm, antiTNF_plus_imm, vedolizumab_no_imm, vedolizumab_plus_imm, imm_alone, none_of_combinations, JAK_inhibitor, X5ASA_only, antiIL_12_23, none_of_meds) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
            rename(
              "Anti-TNF with no immunomodulator" = antiTNF_no_imm,
              "Anti-TNF plus an immunomodulator" = antiTNF_plus_imm,
              "Vedolizumab with no immunomodulator" = vedolizumab_no_imm,
              "Vedolizumab plus an immunomodulator" = vedolizumab_plus_imm,
              "Immunomodulator alone" = imm_alone,
              "No medication combinations" = none_of_combinations,
              "JAK inhibitor" = JAK_inhibitor,
              "5ASA only" = X5ASA_only,
              "Anti-IL 12/23" = antiIL_12_23,
              "None of specified categories" = none_of_meds
            )
            
            labels <- names(totals)
            counts <- as.numeric(totals[1, ])
            percents <- 100 * counts / sum(counts)
          
            
            plot_ly(
              x = percents,
              y = labels,
              type = "bar",
              orientation = "h",
              marker = list(color = viridis(n = 14, option = "D")[3]),
              text = paste0(round(percents, 1), "%"),
              textposition = "outside",
              customdata = counts,
              hovertemplate = paste(
                # "<b>%{x}</b>: %{y:.1f}%",
                "<b>%{y}</b>",
                "<br>%{x:.1f}% (n = %{customdata})",
                "<extra></extra>")) %>%
              layout(
                title = "<b>Medication Exposure</b>",
                showlegend = FALSE,
                xaxis = list(title = "Percent of participants", 
                             ticksuffix = "%",
                             range = c(0, max(percents) * 1.25)),
                yaxis = list(title = NA,
                             categoryorder = "total ascending"),
                font = list(family = "Ubuntu")) %>%
              config(displayModeBar = FALSE)
        })
        
        output$vaccines <- renderPlotly({
          
          data <- study2_react()
          
          totals <- data %>%
            st_drop_geometry() %>%
            select(Astra_Zeneca, Covaxin, Covishield, J_J, Moderna, Pfizer, Sinovac_Coronavac) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
            rename(
              "AstraZeneca" = Astra_Zeneca,
              "Covaxin" = Covaxin,
              "Covishield" = Covishield,
              "Johnson & Johnson" = J_J,
              "Moderna" = Moderna,
              "Pfizer" = Pfizer,
              "Sinovac-CoronaVac" = Sinovac_Coronavac
            )
          
          labels <- names(totals)
          counts <- as.numeric(totals[1, ])
          percents <- 100 * counts / sum(counts)
          
          
          plot_ly(
            x = percents,
            y = labels,
            type = "bar",
            orientation = "h",
            marker = list(color = viridis(n = 14, option = "D")[7]),
            text = paste0(round(percents, 1), "%"),
            textposition = "outside",
            customdata = counts,
            hovertemplate = paste(
              # "<b>%{x}</b>: %{y:.1f}%",
              "<b>%{y}</b>",
              "<br>%{x:.1f}% (n = %{customdata})",
              "<extra></extra>")) %>%
            layout(
              title = "<b>Vaccine Type</b>",
              showlegend = FALSE,
              xaxis = list(title = "Percent of participants", 
                           ticksuffix = "%",
                           range = c(0, max(percents) * 1.25)),
              yaxis = list(title = NA,
                           categoryorder = "total ascending"),
              font = list(family = "Ubuntu")) %>%
            config(displayModeBar = FALSE)
        })
        
      
    }) #moduleServer    
}


