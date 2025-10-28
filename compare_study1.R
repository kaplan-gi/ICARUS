# Title: COVID-19 Serology Multicentre Collaborative Shiny Application (Comparison tab)
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2023-Jan-17
# Updated: 2024-Feb-27
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5

# UI -----------------------------------------------------------------------------------------
compare_study1UI <- function(id) {
     ns <- NS(id)
     
     page_fillable(
         layout_sidebar(style = "overflow-y: auto;",
             height = "82vh",
             border = FALSE,
             fillable = FALSE,
             sidebar = sidebar(style = "white-space: normal; overflow-y: auto; background-color: #D4DADC", 
                 width = "22%",
                 
                 HTML("<p style = 'font-size: 115%; color: #363538'><i>Select which sites you would like to compare and then customize the plots using the filters below.</i><br></p>"),
                 
                 hr(style = "border-top:2px solid #363538;"),
                 
                 prettyRadioButtons(inputId = ns("protein"), 
                                    label = HTML("<span style = 'font-size: 115%;'>Protein of interest:</span>"), 
                                    choices = list("Spike Protein Antibody",
                                                   "Nucleocapsid Protein"),
                                    status = "info"
                 ),
                 
                 # conditionalPanel(condition = "input.protein == 'Spike Protein Antibody'", ns = ns,
                 #                  prettyRadioButtons(inputId = ns("spike_measure"), 
                 #                                     label = HTML("<span style = 'font-size: 115%;'>Plot type:</span>"), 
                 #                                     choices = list("Boxplot by Dose",
                 #                                                    "Scatterplot by Calendar Date",
                 #                                                    "Scatterplot by Days Since Vaccine"),
                 #                                     status = "info"
                 #                  ),
                 #                  conditionalPanel(condition = "input.spike_measure == 'Scatterplot by Days Since Vaccine'", ns = ns, 
                 #                                   pickerInput(inputId = ns("vac_status"), 
                 #                                               label = HTML("<span style = 'font-size: 115%;'>Select vaccination status:</span>"), 
                 #                                               choices =  list("Post-1st vaccination" = "Post-1st", 
                 #                                                               "Post-2nd vaccination" = "Post-2nd ", 
                 #                                                               "Post-3rd vaccination" = "Post-3rd ",
                 #                                                               "Post-4th vaccination" = "Post-4th"), 
                 #                                               selected = list("Post-1st vaccination" = "Post-1st"),
                 #                                               options = list(style = "btn-primary"),
                 #                                               width = "100%"
                 #                                   ),
                 #                                   HTML("<span style = 'font-size: 90%;color: #316673'><i>&nbsp;* Trendlines will not be calculated for <b>Selection</b> subsets below due to small sample size.</i></span>")
                 #                  )
                 #                  
                 # ),
                 # 
                 # conditionalPanel(condition = "input.protein == 'Nucleocapsid Protein'", ns = ns,
                 #                  prettyRadioButtons(inputId = ns("nuc_measure"), 
                 #                                     label = HTML("<span style = 'font-size: 115%;'>Plot type:</span>"),
                 #                                     choices = list("Scatterplot by Calendar Date",
                 #                                                    "Scatterplot by Days Since COVID Diagnosis"),
                 #                                     status = "info"
                 #                  ) 
                 # ),
                 
                 prettyRadioButtons(inputId = ns("subset"), 
                                    label = HTML("<span style = 'font-size: 115%;'>Data selection:</span>"),
                                    choices = list("All data", 
                                                   "Selection"),
                                    status = "info"
                 ),
                 
                 conditionalPanel(condition = "input.subset == 'Selection'", ns = ns,
                                  prettyCheckboxGroup(inputId = ns("disease"),
                                                      label = HTML("<span style = 'font-size: 115%;'>Type of IBD:</span>"),
                                                      choices = list("Crohn's Disease" = "CD", 
                                                                     "Ulcerative Colitis" = "UC", 
                                                                     "IBD Unclassified" = "IBD-U"),
                                                      selected = list("Crohn's Disease" = "CD", 
                                                                      "Ulcerative Colitis" = "UC", 
                                                                      "IBD Unclassified" = "IBD-U"),
                                                      status = "info", 
                                                      icon = icon("check")
                                  ),

                                  prettyCheckboxGroup(inputId = ns("sex"),
                                                      label = HTML("<span style = 'font-size: 115%;'>Sex:</span>"),
                                                      choices = list("Male", "Female"),
                                                      selected = list("Male", "Female"),
                                                      status = "info", 
                                                      icon = icon("check")
                                  ),
                                  
                                  prettyCheckboxGroup(inputId = ns("age_group"),
                                                      label = HTML("<span style = 'font-size: 115%;'>Age group:</span>"),
                                                      choices = list("Adult (18 to 64)" = "Adult", "Elderly (≥65)" = "Elderly"),
                                                      selected = list("Adult (18 to 64)" = "Adult", "Elderly (≥65)" = "Elderly"),
                                                      status = "info", 
                                                      icon = icon("check")
                                  ) 
                 )
             ), #sidebar
         
             layout_columns(style = "margin-top: -10px;",
                 card(
                     height = "76vh",
                     card_header(
                         layout_column_wrap(
                             width = NULL, 
                             fill = FALSE,
                             style = css(grid_template_columns = "1fr 3fr"),
                             HTML("<p style = 'font-size: 115%;color: #363538'><b>Site A: </b></p>"),
                             pickerInput(inputId = ns("groupA"),
                                         choices = sort(unique(serology$Site)), 
                                         selected = "Canada - University of Calgary",
                                         choicesOpt = list(content = str_trunc(sort(unique(serology$Site)), width = 40, side = "right")),
                                         multiple = FALSE, options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE, width = "auto")
                             )
                         )
                     ),
                     card_body(plotlyOutput(ns("groupA_plot")))
                 ),
                 
                 card(
                     height = "76vh",
                     card_header(
                         layout_column_wrap(
                             width = NULL, 
                             style = css(grid_template_columns = "1fr 3fr"),
                             HTML("<p style = 'font-size: 115%;color: #363538'><b>Site B: </b></p>"),
                             pickerInput(inputId = ns("groupB"),
                                         choices = sort(unique(serology$Site)), 
                                         selected = "Belgium - University Hospitals Leuven",
                                         choicesOpt = list(content = str_trunc(sort(unique(serology$Site)), width = 40, side = "right")),
                                         multiple = FALSE, options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE, width = "auto")
                             )
                         )
                     ),
                     card_body(plotlyOutput(ns("groupB_plot")))
                 )
             )
         ) #layout_sidebar
     ) #page_fillable
}


# Server -------------------------------------------------------------------------------------
compare_study1Server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        serology_react <- reactive({
            serology %>%
                {if (input$subset == "Selection") {
                    filter(., IBD_Type %in% input$disease,
                           Sex %in% input$sex,
                           Age_Group %in% input$age_group)
                } else {.}
                }
        })

        output$groupA_plot <- renderPlotly({
            if(input$protein == "Spike Protein Antibody"){
                # PLOT 2
                serology_react() %>% 
                    filter(!is.na(RBD_Value)) %>% 
                    filter(Site %in% input$groupA) %>% 
                    spike_calendar(.)
            } else {
                # PLOT 5
                serology_react() %>% 
                    filter(!is.na(NC_Value)) %>% 
                    filter(Site %in% input$groupA) %>% 
                    nuc_calendar(.)
            }
        })
            
                
        #         # PLOT 1
        #         if (input$spike_measure == "Boxplot by Dose"){
        #             serology_react() %>% 
        #                 filter(Vaccine_Status != "", Vaccine_Status != "Post-5th") %>% 
        #                 filter(Site %in% input$groupA) %>% 
        #                 strip_plot(.)
        #         # PLOT 2
        #         } else if (input$spike_measure == "Scatterplot by Calendar Date"){
        #             serology_react() %>% 
        #                 filter(Site %in% input$groupA) %>% 
        #                 spike_calendar(.)
        #         # PLOT 3
        #         } else {
        #             if (input$subset == "All data"){
        #                 serology_react() %>% 
        #                     filter(Site %in% input$groupA,
        #                            Vaccine_Status_cat == input$vac_status,
        #                            !is.na(RBD_Value)) %>%
        #                     arrange(Days_since_pv) %>% 
        #                     spike_scatter(.)
        #             } else {
        #                 serology_react() %>% 
        #                     filter(Site %in% input$groupA,
        #                            Vaccine_Status_cat == input$vac_status,
        #                            !is.na(RBD_Value)) %>%
        #                     arrange(Days_since_pv) %>% 
        #                     spike_scatterb(.)
        #             }
        #            
        #         }
        #     } else {
        #         # PLOT 4
        #         if (input$nuc_measure == "Scatterplot by Calendar Date") {
        #             serology_react() %>% 
        #                 filter(Site %in% input$groupA) %>% 
        #                 nuc_calendar(.)
        #         # PLOT 5
        #         } else {
        #             serology_react() %>% 
        #                 filter(!is.na(DX_sero_days),
        #                        Site %in% input$groupA) %>% 
        #                 nuc_diagnosis(.)
        #         }
        #     }
        # })
        
        output$groupB_plot <- renderPlotly({
            if(input$protein == "Spike Protein Antibody"){
                serology_react() %>% 
                    filter(!is.na(RBD_Value)) %>% 
                    filter(Site %in% input$groupB) %>% 
                    spike_calendar(.)
            } else {
                serology_react() %>% 
                    filter(!is.na(NC_Value)) %>% 
                    filter(Site %in% input$groupB) %>% 
                    nuc_calendar(.)
            }
        })
            
    }) #moduleServer    
}
