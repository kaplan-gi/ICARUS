# Title: COVID-19 Serology Multicentre Collaborative Shiny Application (Demographics tab)
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2023-Jan-17
# Updated: 2024-Feb-27
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5


# UI -------------------------------------------------------------------------------------------
demographics_study1UI <- function(id) {
    ns <- NS(id)
    
    page_fillable(
      
        layout_columns(
            style = "padding-top: 10px",
            col_widths = c(4, 3, 5), height = "20vh",
            fill = TRUE, fillable = TRUE,
            
            pickerInput(inputId = ns("picker"), 
                        label = HTML("<span style = 'font-size: 115%; color: #363538'><i>Select study sites to customize the dashboard:</i></span>"),
                        choices = sort(unique(serology$Site)), selected = unique(serology$Site),
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
                    card(card_body(plotlyOutput(ns("age"))))
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
            
            # LOCATIONS
            card(
                style = "background-color: #F6F6F6;",
                #card_header("COVID-19 Vaccines"),
                card_body(

                    card(
                      card_body(
                        style = "padding-top: -20px; padding-left: 4px; padding-right: 4px;",
                        plotlyOutput(ns("locations")))
                    )

                    # value_box(
                    #     height = "24vh",
                    #     style = "background-color: #D4DADC;",
                    #     title = "",
                    #     value = textOutput(ns("diagnoses")),
                    #     p("of participants were diagnosed with COVID-19"),
                    #     showcase = bs_icon("virus"),
                    #     showcase_layout = showcase_top_right(width = 0.2)
                    # )
                )
            )
        ) #layout_columns2
    ) #page
} 


# Server ----------------------------------------------------------------------------------------
demographics_study1Server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
      
        serology_react <- reactive({
            serology %>%
                filter(Site %in% input$picker)
        })
        
        reg_react <- reactive({
          validate(
            need(input$picker != "", "Please select a site"))
          regData %>% 
            st_drop_geometry %>% 
            filter(site %in% input$picker)
        })
        
        # transform for vaccine plot on demographics page
        dose1 <- c("1", "12", "123", "1234")
        dose2 <- c("12", "123", "1234")
        dose3 <- c("123", "1234")
        
        # vaccine_data <- serology %>% 
        #   mutate(doses = case_when(pv4 != "" ~ "1234",
        #                            pv3 != "" ~ "123",
        #                            pv2 != "" ~ "12",
        #                            pv1 != "" ~ "1",
        #                            TRUE ~ "0")) %>% 
        #   select(Site, PatientID, pv1, pv2, pv3, pv4, doses) %>% 
        #   distinct(PatientID, .keep_all = TRUE) %>% 
        #   group_by(Site, doses) %>% 
        #   summarize(participants = n()) %>% 
        #   group_by(Site) %>% 
        #   summarize(site_total = sum(participants),
        #             `No Doses` = sum(participants[doses == "0"]),
        #             `1st Dose` = sum(participants[doses %in% dose1]),
        #             `2nd Dose` = sum(participants[doses %in% dose2]),
        #             `3rd Dose` = sum(participants[doses %in% dose3]),
        #             `4th Dose` = sum(participants[doses == "1234"])) %>% 
        #   pivot_longer(cols = `No Doses`:`4th Dose`,
        #                names_to = "cum_dose",
        #                values_to = "participants")
        # vaccine_react <- reactive({
        #   vaccine_data %>% 
        #     filter(Site %in% input$picker)
        # })

        output$map <- renderLeaflet({
          leaflet(options = leafletOptions(worldCopyJump = TRUE, preferCanvas = TRUE)) %>%
              addProviderTiles("CartoDB.Positron") %>%
              setView(lng = -5, lat = 40, zoom = 1.3) %>%
              addCircleMarkers(data = regData,
                               fillColor = "gray",
                               stroke = FALSE,
                               fillOpacity = 0.8,
                               lat = regData$marker_lat,
                               lng = regData$marker_long,
                               layerId = regData$site, # LH: likely going to have to create JSON for each site
                               label = regData$format_name) %>% 
              addAwesomeMarkers(data = reg_react(),
                                lat = reg_react()$marker_lat,
                                lng = reg_react()$marker_long,
                                icon =  icons,
                                label = reg_react()$format_name)
        })

        
        output$sites <- renderText({paste0(length(unique(serology_react()$Site)), " of ", length(unique(serology$Site)))})
        output$participants <- renderText({paste0("Participants: ", length(unique(serology_react()$PatientID)))})
        output$samples <- renderText({paste0("Samples: ", nrow(serology_react()[!is.na(serology_react()$Assay),]))})
        
        output$disease <- renderPlotly({
            serology_react() %>% 
                group_by(IBD_Type) %>% 
                summarize(count = length(unique(PatientID))) %>% 
                arrange(factor(IBD_Type, levels = c("CD", "UC", "IBD-U", "Unknown", "Other"))) %>% 
                plot_ly(labels = ~IBD_Type, values = ~count,
                        marker = list(colors = c(viridis(n = 14, option = "D")[3], 
                                                 viridis(n = 14, option = "D")[5], 
                                                 viridis(n = 14, option = "D")[7], 
                                                 viridis(n = 14, option = "D")[9], 
                                                 viridis(n = 14, option = "D")[11])), # c("#E6AB02", "#408697","#D95F02", "#5c5a5f", "#1B9E77")
                        #textinfo = "label+percent", 
                        texttemplate = paste("%{label}", "%{percent:.1%}"),
                        textposition = "outside", 
                        customdata = ~count, 
                        hovertemplate = paste("<b>%{label}</b>: %{percent:.1%}",
                                              "<br>N = %{customdata}",
                                              "<extra></extra>")) %>% 
                add_pie(hole = 0.4) %>% 
                layout(title = "<b>IBD Type</b>",
                       showlegend = FALSE,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       margin = list(l = 30, r = 30, b = 30),
                       font = list(family = "Ubuntu")) %>% 
            config(displayModeBar = FALSE)
        })
        
        output$sex <- renderPlotly({
            serology_react() %>% 
                group_by(Sex) %>% 
                summarize(count = length(unique(PatientID))) %>% 
                arrange(Sex) %>% 
                plot_ly(labels = ~Sex, values = ~count,
                        texttemplate = paste("%{label}", "%{percent:.1%}"),
                        textposition = "outside",
                        #marker = list(colors = c("#E6AB02", "#408697","#D95F02"))
                        marker = list(colors = c(viridis(n = 14, option = "D")[3], 
                                                 viridis(n = 14, option = "D")[7], 
                                                 viridis(n = 14, option = "D")[11])), # c("#E6AB02", "#408697", "#5c5a5f")
                        customdata = ~count, 
                        hovertemplate = paste("<b>%{label}</b>: %{percent:.1%}",
                                              "<br>N = %{customdata}",
                                              "<extra></extra>")) %>% 
                add_pie(hole = 0.4) %>% 
                layout(title = "<b>Sex</b>",
                       showlegend = FALSE,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       margin = list(l = 30, r = 30, b = 30),
                       font = list(family = "Ubuntu")) %>% 
            config(displayModeBar = FALSE)
        })
        
        
        output$age <- renderPlotly({
          serology_react() %>% 
            select(PatientID, decade) %>% 
            distinct(PatientID, .keep_all = TRUE) %>% 
            group_by(decade) %>% 
            summarize(count = n()) %>% 
            mutate(percent = round(count/sum(count)*100,1)) %>% 
            mutate(label = paste0(sprintf("%.1f", percent), "%")) %>% 
            plot_ly(x = ~decade, y = ~percent, type = "bar",
                    marker = list(color = viridis(n = 14, option = "D")[11]), # "#D95F02"
                    text = ~label,
                    customdata = ~count, 
                    hovertemplate = paste("<b>Age %{x}</b>: %{text}",
                                          "<br>N = %{customdata}",
                                          "<extra></extra>")) %>% 
            layout(title = "<b>Age Distribution</b>",
                   showlegend = FALSE,
                   xaxis = list(title = "Age by Decade"),
                   yaxis = list(title = "Percent of Participants",
                                ticksuffix = "%"),
                   font = list(family = "Ubuntu")) %>% 
            config(displayModeBar = FALSE)
        })
        
        
        # Check for labeling horizontal barplot: https://community.plotly.com/t/labels-above-horizontal-bars-better-way-than-subplots/70693/7
        output$medications <- renderPlotly({
          serology_react() %>% 
            dplyr::select(PatientID, baseantitnf:baseimmuno) %>%
            distinct() %>%
            pivot_longer(cols = baseantitnf:baseimmuno,
                         names_to = "drug",
                         values_to = "drug_yn") %>%
            filter(drug_yn == 1) %>%
            group_by(drug) %>%
            summarize(count = n()) %>%
            mutate(percent = round(count/length(unique(serology_react()$PatientID))*100,1)) %>%
            mutate(label = paste0(sprintf("%.1f", percent), "%"),
                   drug = case_when(drug == "basevedo" ~ "Vedolizumab",
                                    drug == "baseuste" ~ "Ustekinumab",
                                    drug == "basesteroid" ~ "Steroids",
                                    drug == "basejak" ~ "JAK Inhibitors",
                                    drug == "baseimmuno" ~ "Immunomod.",
                                    drug == "baseasa" ~ "5-ASA",
                                    drug == "baseantitnf" ~ "Anti-TNF",
                                    TRUE ~ "None")) %>%
            plot_ly(x = ~percent, y = ~drug, type = "bar", orientation = "h",
                    marker = list(color = viridis(n = 14, option = "D")[3]), # "#7570B3"
                    text = ~label,
                    customdata = ~count,
                    hovertemplate = paste("<b>%{y}</b>: %{text}",
                                          "<br>N = %{customdata}",
                                          "<extra></extra>")) %>%
            layout(title = "<b>Medication Exposure</b>",
                   xaxis = list(title = "Percent of Participants",
                                ticksuffix = "%"),
                   yaxis = list(title = NA,
                                categoryorder = "total ascending"),
                   font = list(family = "Ubuntu")) %>%
            config(displayModeBar = FALSE)
        })
        
        
       # output$vaccines <- renderPlotly({
       #   vaccine_react() %>% 
       #     mutate(total = sum(unique(site_total))) %>% 
       #     group_by(cum_dose, total) %>% 
       #     summarize(count = sum(participants)) %>% 
       #     mutate(percent = round(count/total*100, 1)) %>% 
       #     mutate(label = paste0(sprintf("%.0f", percent), "%")) %>% 
       #     plot_ly(x = ~cum_dose, y = ~percent, type = "bar",
       #             marker = list(color = "#1B9E77"),
       #             text = ~label,
       #             customdata = ~count,
       #             hovertemplate = paste("<b>%{x}</b>",
       #                                   "<br>N:  %{customdata}",
       #                                   "<br>percent:  %{text}",
       #                                   "<extra></extra>")) %>% 
       #     layout(title = "<b>Vaccine Uptake</b>",
       #            xaxis = list(title = "Vaccine Dose Record"),
       #            yaxis = list(title = "Percent of Participants",
       #                         ticksuffix = "%"),
       #            font = list(family = "Ubuntu")) %>% 
       #     config(displayModeBar = FALSE)
       # })
       # 
        output$locations <- renderPlotly({
          
         serology_react() %>% 
            select(PatientID, Country) %>% 
            distinct(PatientID, .keep_all = TRUE) %>% 
            group_by(Country) %>% 
            summarize(count = n()) %>% 
            mutate(percent = round(count/sum(count)*100,1)) %>% 
            mutate(label = paste0(Country, ": ", sprintf("%.1f", percent), "%"),
                   id = as.factor(1))  %>% 
            plot_ly(x = id, y = ~percent, color = ~Country, type = "bar",
                    colors = c("Belgium" = viridis(14, option = "D")[1], # "#408697", 
                               "Canada" = viridis(n = 14, option = "D")[3], # "#D95F02",
                               "Hong Kong" = viridis(14, option = "D")[5], # "#5c5a5f",
                               "India" = viridis(14, option = "D")[7], # "#e7298a",
                               "Japan" = viridis(14, option = "D")[9], # "#7570B3",
                               "United Kingdom" = viridis(14, option = "D")[11], # "#E6AB02",
                               "United States" = viridis(14, option = "D")[13]), # "#1B9E77"),
                    text = ~label, 
                    textposition = "inside", 
                    textfont = list(color = "white"),
                    customdata = ~paste0("<b>", Country, "</b>: ", percent, "%<br>", "(N = ", count, ")"),
                    hovertemplate = paste("%{customdata}",
                                           "<extra></extra>")) %>% 
            layout(title = "<b>Location</b>",
                   barmode = "stack",
                   showlegend = FALSE,
                   xaxis = list(title = "", showticklabels = FALSE),
                   yaxis = list(title = "Percent of Participants",
                                showticklabels = FALSE),
                   font = list(family = "Ubuntu")) %>% 
            config(displayModeBar = FALSE)
            
        })
        
        # output$diagnoses <- renderText({paste0(round(
        #   serology_react() %>%
        #     filter(Prior_COVID_Diagnosis == "Yes") %>%
        #     distinct(PatientID) %>%
        #     nrow()/length(unique(serology_react()$PatientID))*100), "%")
        # 
        # })

    }) #moduleServer    
}


