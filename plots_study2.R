# Title: COVID-19 Serology Multicentre Collaborative Shiny Application (Study 2 plots tab)
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2025-Jul-30
# Updated: 
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5

# Read the manifest
# https://raw.githubusercontent.com/{user}/{repo}/{branch}/{path-to-file}
manifest <- read.csv("https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/plot_file_manifest.csv", header = TRUE, stringsAsFactors = FALSE)

# Add base GitHub raw path to create URL
base_url <- "https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/Plots/"
manifest$url <- paste0(base_url, manifest$filename)

# -------- Dictionaries: metrics, variables, and levels for the COMPARE and MULTIPLE VARIABLE plots --------

metrics_map <- c("N" = "Nucleocapsid", "S" = "Spike")

# Variables that can appear in the single-variable ("comp") schema:
valid_variables <- c(
  "age_group",
  "BMI_category",
  "capped_data",
  "Gender",
  "IBD_Diagnosis",
  "ICARUS_site",
  "Individual_Vaccine_formulation",
  "Medication_Groups",
  "Region"
)

# Levels per variable (match filenames exactly)
levels_by_variable <- list(
  age_group = c("Adult","Elderly","Pediatric"),
  BMI_category = c("Underweight","HealthyWeight","Overweight","Obese"),
  capped_data = c("Maximum dilution of 250 achieved","Serial dilution"),
  Gender = c("Male","Female"),
  IBD_Diagnosis = c("UC","CD"),
  ICARUS_site = c("Chandigarh", "Chicago", "HongKong", "Leuven", "London", "Miami", "NewDelhi", "NewYork", "Nishinomiya", "Orebro", "Oxford", "Paris", "Toronto"),
  Individual_Vaccine_formulation = c("Adenovirus", "Inactivated", "mRNA"), 
  Medication_Groups_3_TNF_Imm = c("NoOr5ASA", "AntiTNFonly","AnyImm", "Other"), 
  Region = c("West","East") 
)

# -------- Helpers --------

# Map protein radio to metric code used in filenames
protein_to_metric <- c(spike = "S", nucleo = "N")

# Map UI variable keys -> filename variable tokens
variable_key_map <- c(
  age = "age_group",
  BMI = "BMI_category",
  dilution = "capped_data",
  gender = "Gender",
  diagnosis = "IBD_Diagnosis",
  site = "ICARUS_site",
  meds = "Medication_Groups_3_TNF_Imm",
  region = "Region",
  vaccine = "Individual_Vaccine_formulation"
)

# Pretty labels for levels; only override where you want a prettier label
# names = tokens in filenames; values = labels users see
pretty_levels_by_variable <- list(
  BMI_category = c(
    "HealthyWeight" = "Health weight",
    "Obese" = "Obese",
    "Overweight" = "Overweight", 
    "Underweight" = "Underweight"
  ),
  IBD_Diagnosis = c(
    "CD" = "Crohn's disease",
    "UC" = "Ulcerative colitis"
  ),
  ICARUS_site = c(
    "Leuven" = "University Hospitals Leuven, Leuven, Belgium",
    "Toronto" = "Mount Sinai Hospital, University of Toronto, Toronto, Canada",
    "Paris" = "Hospital Saint-Louis, Paris, France",
    "HongKong" = "Chinese University of Hong Kong - Prince of Wales Hospital, Hong Kong, China",
    "NewDelhi" = "All India Institute of Medical Sciences, New Delhi, India",
    "Chandigarh" = "Postgraduate Institute of Medical Education and Research, Chandigarh, India",
    "Nishinomiya" = "Hyogo College of Medicine, Nishinomiya, Japan",
    "Orebro" = "Örebro University Hospital, Örebro, Sweden",
    "Oxford" = "University of Oxford, Oxford, UK",
    "London" = "Royal London Hospital, London, UK",
    "NewYork" = "Icahn School of Medicine at Mount Sinai, New York, USA",
    "Miami" = "University of Miami, Miami, USA",
    "Chicago" = "University of Chicago, Chicago, USA"
  ),
  Medication_Groups_3_TNF_Imm = c(
    "NoOr5ASA" = "5ASA or no IBD medication",
    "AntiTNFonly" = "Anti-TNF only",
    "AnyImm" = "Any Immunomodulator",
    "Other" = "Other advanced therapies"
  )
)

# Get the display label for a single token given a variable token
pretty_level <- function(variable_token, level_token) {
  m <- pretty_levels_by_variable[[variable_token]]
  if (!is.null(m) && level_token %in% names(m)) return(m[[level_token]])
  # optional fallback to make readable
  gsub("_", " ", level_token)
}

# Levels for a given UI variable key; pulls from levels_by_variable
levels_for_key <- function(key) {
  var_token <- variable_key_map[[key]]
  if (is.null(var_token)) return(character(0))
  levels_by_variable[[var_token]] %||% character(0)
}

# Build choices (names = labels, values = tokens) for a UI variable key (e.g., "site")
level_choices_for_key <- function(variable_key) {
  var_token <- variable_key_map[[variable_key]]
  tokens <- levels_for_key(variable_key)
  if (length(tokens) == 0) return(character(0))
  labels <- vapply(tokens, function(tok) pretty_level(var_token, tok), character(1))
  stats::setNames(tokens, labels)  # names shown to user; values used in input$level
}

# Build the single-variable filename for a given metric, variable key, and level for COMPARE UI
build_comp_filename <- function(metric_comp, variable_key, level_token) {
  var_token <- variable_key_map[[variable_key]]
  if (is.null(var_token) || !nzchar(level_token)) return(NA_character_)
  paste0("ROCHE_", metric_comp, "_VAL_", var_token, "_", level_token, ".png")
}

# Build the single-variable filename from UI input for MULTIPLE VARIABLE UI
build_multi_filename <- function(metric_multi, site, diagnosis, gender, age, vaccine, meds, BMI) {
  parts <- c(site, diagnosis, gender, age, vaccine, meds, BMI)
  
  if (any(is.null(parts) | parts == "")) return(NA_character_)
  
  paste0("ROCHE_", metric_multi, "_VAL_", paste(parts, collapse = "_"), ".png")
}

# -------- Helpers and subsetting for manifest filenames --------

# Existence helper to determine if filename is contained in the manifest
has_plot <- function(filename) {
  !is.na(filename) && filename %in% manifest$filename
}

# Parse strings to create variable for display mode of plots
filename_df <- manifest %>%
  mutate(
    display_mode = case_when(
      str_detect(filename, "all_data") ~ "all",
      str_detect(filename, "age_group|BMI_category|capped_data|Gender|IBD_Diagnosis|ICARUS_site|Individual_Vaccine_formulation|Medication_Groups|Region") ~ "comp",
      TRUE ~ "multi"
    )
  )

# Create data subsets for each display mode
all_list <- filename_df %>%
  filter(display_mode == "all" )

compare_list <- filename_df %>%
  filter(display_mode == "comp" )

multi_list <- filename_df %>%
  filter(display_mode == "multi" )


# UI -----------------------------------------------------------------------------------------
plots_study2UI <- function(id) {
     
  ns <- NS(id)
     
  # Restrict image scaling so that static images are not stretched
  tags$head(tags$style(HTML("
    img.no-upscale {
      width: auto !important; /* never force full width */
      height: auto !important; /* keep proportions */
      max-height: 75%; /* cap vertical size */
      display: block; /* centers with margin:auto */
      margin: 0 auto;
    }
  ")))   
     
     
     page_fillable(
         layout_sidebar(style = "overflow-y: auto;",
             height = "82vh",
             border = FALSE,
             fillable = FALSE,
             sidebar = sidebar(style = "white-space: normal; overflow-y: auto; background-color: #D4DADC", 
                 width = "17.5%",
                 
                 # Level for left compare based on sidebar's variable choice
                 pickerInput(inputId = ns("data_type"),
                             label = HTML("<span style = 'font-size: 100%;'>Select which data to view:</span>"),
                             choices = list("Roche S and N titres" = "titres",
                                            "Seroconversion by region and BMI" = "serocon",
                                            "Anti-Nucleocapsid by dose" = "antiN"
                             ),
                             selected = list("Roche S and N titres" = "titres"),
                             multiple = FALSE,
                             options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
                             width = "100%"),
                 
                 # HTML("<p style = 'font-size: 85%; color: #363538';><i>Customize the display:</i></p>"),
                 
                 conditionalPanel(condition = "input.data_type == 'titres'", ns = ns,
                                  
                                  prettyRadioButtons(inputId = ns("protein"), 
                                                     label = HTML("<span style = 'font-size: 90%;'>Protein of interest</span>"), 
                                                     choices = list("Spike protein" = "spike",
                                                                    "Nucleocapsid protein" = "nucleo"),
                                                     selected = "spike",
                                                     status = "info"
                                  ),
                                  
                                  prettyRadioButtons(inputId = ns("plot_mode"), 
                                                     label = HTML("<span style = 'font-size: 90%;'>Plot display mode</span>"),
                                                     choices = list("All data" = "all_data",
                                                                    "Compare by variable type" = "compare",
                                                                    "Multiple variable selection" = "multi_var"), # Full variable stratification
                                                     selected = "all_data",
                                                     status = "info"
                                  ),
                                  
                                  conditionalPanel(condition = "input.plot_mode == 'compare'", ns = ns,
                                                   prettyRadioButtons(inputId = ns("variable"),
                                                                      label = HTML("<span style = 'font-size: 90%;'>Variable to compare</span>"),
                                                                      choices = list("Age group" = "age",
                                                                                     "BMI category" = "BMI",
                                                                                     "Dilution" = "dilution",
                                                                                     "Gender" = "gender",
                                                                                     "IBD diagnosis" = "diagnosis",
                                                                                     "ICARUS site" = "site",
                                                                                     "Medication group" = "meds",
                                                                                     "Region" = "region",
                                                                                     "Vaccine formulation" = "vaccine"),
                                                                      selected = list("Age group" = "age"),
                                                                      status = "info"
                                                   )
                                  ), # conditionalPanel plot_mode == 'compare'
                                  
                                  conditionalPanel(condition = "input.plot_mode == 'multi_var'", ns = ns,
                                                   
                                                   HTML("<p style = 'font-size: 90%; color: #363538'; margin: -10px;>Levels to compare</p>"),
                                                   
                                                   # Level for left compare based on sidebar's variable choice
                                                   pickerInput(inputId = ns("site_multi"),
                                                               label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>ICARUS site</span>"),
                                                               choices = list("Leuven" = "Leuven",
                                                                              "Toronto" = "Toronto",
                                                                              "Paris" = "Paris",
                                                                              "Hong Kong" = "HongKong",
                                                                              "New Delhi" = "NewDelhi",
                                                                              "Chandigarh" = "Chandigarh",
                                                                              "Nishinomiya" = "Nishinomiya",
                                                                              "Örebro" = "Orebro",
                                                                              "Oxford" = "Oxford",
                                                                              "London" = "London",
                                                                              "New York" = "NewYork",
                                                                              "Miami" = "Miami",
                                                                              "Chicago" = "Chicago"
                                                               ),
                                                               selected = list("Leuven" = "Leuven"),
                                                               multiple = FALSE,
                                                               options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
                                                               width = "100%"),
                                                   
                                                   prettyRadioButtons(inputId = ns("age_multi"),
                                                                      label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>Age group</span>"),
                                                                      choices = list("Adult" = "Adult",
                                                                                     "Elderly" = "Elderly",
                                                                                     "Pediatric" = "Pediatric"),
                                                                      selected = list("Adult" = "Adult"),
                                                                      status = "info", 
                                                                      inline = TRUE,
                                                                      bigger = FALSE
                                                   ), 
                                                   
                                                   prettyRadioButtons(inputId = ns("BMI_multi"),
                                                                      label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>BMI category</span>"),
                                                                      choices = list("Healthy weight" = "HealthyWeight",
                                                                                     "Obese" = "Obese",
                                                                                     "Overweight" = "Overweight",
                                                                                     "Underweight" = "Underweight"),
                                                                      selected = list("Healthy weight" = "HealthyWeight"),
                                                                      status = "info", 
                                                                      inline = TRUE,
                                                                      bigger = FALSE
                                                   ), 
                                                   
                                                   prettyRadioButtons(inputId = ns("gender_multi"),
                                                                      label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>Gender</span>"),
                                                                      choices = list("Female" = "Female",
                                                                                     "Male" = "Male",
                                                                                     "Other gender identity" = "Other"),
                                                                      selected = list("Female" = "Female"),
                                                                      status = "info", 
                                                                      inline = TRUE,
                                                                      bigger = FALSE
                                                   ), 
                                                   
                                                   prettyRadioButtons(inputId = ns("diagnosis_multi"),
                                                                      label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>IBD diagnosis</span>"),
                                                                      choices = list("Crohn's disease" = "CD",
                                                                                     "Ulcerative colitis" = "UC"),
                                                                      selected = list("Crohn's disease" = "CD"),
                                                                      status = "info", 
                                                                      inline = TRUE,
                                                                      bigger = FALSE
                                                   ), 
                                                   
                                                   prettyRadioButtons(inputId = ns("meds_multi"),
                                                                      label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>Medication group</span>"),
                                                                      choices = list("Anti-TNF only" = "AntiTNFonly", 
                                                                                     "Any Immunomodulator" = "AnyImm",
                                                                                     "Other advanced therapies" = "Other",
                                                                                     "5ASA or no IBD medication" = "NoOr5ASA"),
                                                                      selected = list("Anti-TNF only" = "AntiTNFonly"),
                                                                      status = "info", 
                                                                      inline = TRUE,
                                                                      bigger = FALSE
                                                   ),
                                                   
                                                   prettyRadioButtons(inputId = ns("vaccine_multi"),
                                                                      label = HTML("<span style = 'font-size: 90%; text-decoration: underline;'>Vaccine formulation</span>"),
                                                                      choices = list("Adenovirus" = "Adenovirus", 
                                                                                     "Inactivated" = "Inactivated",
                                                                                     "mRNA" = "mRNA"),
                                                                      selected = list("mRNA" = "mRNA"),
                                                                      status = "info", 
                                                                      inline = TRUE,
                                                                      bigger = FALSE
                                                   ),
                                  ) # conditionalPanel plot_mode == 'multi_var'

                 ), # conditionalPanel data_type == 'titres'
                 
                 conditionalPanel(condition = "input.data_type == 'serocon'", ns = ns,
                                  
                                  prettyRadioButtons(inputId = ns("serocon_type"),
                                                     label = HTML("<span style = 'font-size: 90%;'>Select data</span>"),
                                                     choices = list("Percent seroconversion" = "per_sero", 
                                                                    "Percent reaching max. Roche S" = "per_max"),
                                                     selected = list("Percent seroconversion" = "per_sero"),
                                                     status = "info", 
                                                     inline = FALSE,
                                                     bigger = FALSE
                                  ),
                                  
                                  HTML("<p style = 'font-size: 100%; color: #408697';><i>Click on the plot to enable interactive tools such as zooming, panning, and viewing exact values by hovering.</i></p>"),
                                  
                 ), # conditionalPanel data_type == 'serocon'
                 
                 conditionalPanel(condition = "input.data_type == 'antiN'", ns = ns,
                                  
                                  HTML("<p style = 'font-size: 100%; color: #408697';><i>Click on the plot to enable interactive tools such as zooming, panning, and viewing exact values by hovering.</i></p>"),
                                  
                 ), # conditionalPanel data_type == 'antiN'
                 
             ), #sidebar
         
            layout_columns(style = "margin-top: -10px;",
                           
                           uiOutput(ns("dynamicLayout"))
              
            )
             
            
         ) #layout_sidebar
     ) #page_fillable
}


# Server -------------------------------------------------------------------------------------
plots_study2Server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # -------------------------------------------------- 
    # Roche S and N titres plots
    # -------------------------------------------------- 
    
    list_react <- reactive({
      
      req(input$plot_mode)
      req(input$protein)  # ensure input$protein is available
    
      if (input$plot_mode == "all_data") {
        input_data <- all_list
        if (input$protein == "spike") {
          input_data <- filter(input_data, grepl("_S_", filename))
        } else {
          input_data <- filter(input_data, grepl("_N_", filename))
        }
      } else if (input$plot_mode == "compare") {
        input_data <- compare_list
        if (input$protein == "spike") {
          input_data <- filter(input_data, grepl("_S_", filename))
        } else {
          input_data <- filter(input_data, grepl("_N_", filename))
        }
      } else if (input$plot_mode == "multi_var") {
        input_data <- multi_list
        if (input$protein == "spike") {
          input_data <- filter(input_data, grepl("_S_", filename))
        } else {
          input_data <- filter(input_data, grepl("_N_", filename))
        }
      }
      
      return(input_data)
      
    })
    
    # ---------- Plots for all data ----------
    
    output$all_plot <- renderUI({
      img_url <- list_react()$url
      tags$img(src = img_url, 
               class = "no-upscale",
               style = "border: 1px solid #ccc;")
    })
    
    # ---------- Plots for compare ----------
    
    # Construct filenames for compare plots
    comp_filename1 <- reactive({
      req(input$plot_mode == "compare")     
      req(input$protein, input$variable) # needed to build the name
      req(!is.null(input$level1)) # level1 exists (card rendered) and chosen
      
      metric_comp <- protein_to_metric[[input$protein]]
      build_comp_filename(metric_comp, input$variable, input$level1)
    })
    
    comp_filename2 <- reactive({
      req(input$plot_mode == "compare")     
      req(input$protein, input$variable) # needed to build the name
      req(!is.null(input$level2)) # level2 exists (card rendered) and chosen
      
      metric_comp <- protein_to_metric[[input$protein]]
      build_comp_filename(metric_comp, input$variable, input$level2)
    })
    
    # Render compare left card
    output$comp_plot1 <- renderUI({
      if (input$plot_mode != "compare") return(NULL)
      req(comp_filename1())
      fn <- comp_filename1()
      if (!has_plot(fn)) {
        return(tagList(
          tags$div(
            style = "font-size: 125%; color: #363538;",
            strong("Your selection resulted in fewer than 10 data points. Please make another selection.")
          )
        ))
      }
      img_url <- manifest$url[manifest$filename == fn]
      tags$img(
        src = img_url, 
        style = "width:100%; height:auto; min-height: 400px; max-height: 800px; object-fit:contain; image-rendering:auto;"
      ) 
    })
    
    # Render compare right card
    output$comp_plot2 <- renderUI({
      if (input$plot_mode != "compare") return(NULL)
      req(comp_filename2())
      fn <- comp_filename2()
      if (!has_plot(fn)) {
        return(tagList(
          tags$div(
            style = "font-size: 125%; color: #363538;",
            strong("Your selection resulted in fewer than 10 data points. Please make another selection.")
          )
        ))
      }
      img_url <- manifest$url[manifest$filename == fn]
      tags$img(
        src = img_url, 
        style = "width:100%; height:auto; min-height: 400px; max-height: 800px; object-fit:contain; image-rendering:auto;"
      )
    })

    
    # ---------- Plot for multiple variable selection ----------
    
    # Construct filenames for multi plots
    multi_filename <- reactive({
      req(input$plot_mode == "multi_var")     
      req(input$protein) # needed to build the name
      
      filename <- build_multi_filename(
        metric_multi = protein_to_metric[[input$protein]],
        site = input$site_multi,
        diagnosis = input$diagnosis_multi,
        gender = input$gender_multi,
        age = input$age_multi,
        vaccine = input$vaccine_multi,
        meds = input$meds_multi,
        BMI = input$BMI_multi
      )
      
      filename
      
    })
  
  
    # Render multiple variable plot
    output$multi_plot <- renderUI({
      if (input$plot_mode != "multi_var") return(NULL)
      req(multi_filename())
      fn <- multi_filename()
      if (!has_plot(fn)) {
        return(tagList(
          tags$div(
            style = "font-size: 125%; color: #363538;",
            strong("Your selection resulted in fewer than 10 data points. Please make another selection.")
          )
        ))
      }
      img_url <- manifest$url[manifest$filename == fn]
      tags$img(src = img_url, height = "600px", style = "border: 1px solid #ccc;")
    })
    
    # -------------------------------------------------- 
    # Seroconversion by region and BMI plot
    # -------------------------------------------------- 
    
    output$sero_plot <- renderPlotly({
      
      if (input$serocon_type == "per_sero") {
    
        plot_ly(
          data = serocon_data, 
          x = ~BMI_category, 
          y = ~percent_seroconvert, 
          color = ~region,
          colors = c(viridis(n = 14, option = "D")[3], 
                     viridis(n = 14, option = "D")[9]),
          type = "bar",
          hovertemplate = ~label_serocon,
          cliponaxis = FALSE
        ) %>%
        layout(
          barmode = "group",
          title = "<b>Percentage of participants who seroconverted</b>",
          xaxis = list(
            title = "Body mass index group",
            tickmode = "array"
          ),
          yaxis = list(
            title = "Percent seroconversion",
            ticksuffix = "%" 
          ),
          margin = list(l = 50, r = 0, b = 150, t = 75),
          annotations = list(
            x = 0.5,
            y = -0.25,
            text = paste(
              "BMI categories East: Underweight <18.5; Healthy weight 18.5–<23; Overweight 23–<27; Obese ≥27",
              "BMI categories West: Underweight <18.5; Healthy weight 18.5–<25; Overweight 25–<30; Obese ≥30",
              sep = "<br>"
            ),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            font = list(size = 12, color = "#444")
          )
        )
        
      } else {
        
        plot_ly(
          data = serocon_data, 
          x = ~BMI_category, 
          y = ~percent_max_RocheS, 
          color = ~region,
          colors = c(viridis(n = 14, option = "D")[5], 
                     viridis(n = 14, option = "D")[11]),
          type = "bar",
          hovertemplate = ~label_maxRocheS,
          cliponaxis = FALSE
        ) %>%
          layout(
            barmode = "group",
            title = "<b>Percentage of participants who reached maximum Roche S value</b>",
            xaxis = list(
              title = "Body mass index group",
              tickmode = "array"
            ),
            yaxis = list(
              title = "Percent maximum Roche S",
              ticksuffix = "%" 
            ),
            margin = list(l = 50, r = 0, b = 150, t = 75),
            annotations = list(
              x = 0.5,
              y = -0.25,
              text = paste(
                "BMI categories East: Underweight <18.5; Healthy weight 18.5–<23; Overweight 23–<27; Obese ≥27",
                "BMI categories West: Underweight <18.5; Healthy weight 18.5–<25; Overweight 25–<30; Obese ≥30",
                sep = "<br>"
              ),
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "top",
              font = list(size = 12, color = "#444")
            )
          )
        
      }
   
    })
    
    # -------------------------------------------------- 
    # Anti-Nucleocapsid by dose
    # -------------------------------------------------- 
    
    output$antiN_plot <- renderPlotly({
      
      plot_ly(
        data = antiN_data, 
        x = ~time_period, 
        y = ~percent_pos_antiN, 
        color = ~vaccine_type,
        colors = c(viridis(n = 14, option = "D")[2], 
                   viridis(n = 14, option = "D")[7],
                   viridis(n = 14, option = "D")[12]),
        type = "bar",
        hovertemplate = ~label,
        cliponaxis = FALSE
      ) %>%
        layout(
          barmode = "group",
          title = "<b>Percentage of participants with a positive anti-Nucleocapsid test<br>by vaccine type and time period</b>",
          xaxis = list(
            title = "Time period (days from vaccine dose)",
            tickmode = "array"
            # tickvals = c("post1_14to112", "post2_14to84", "post2_85to168", "post2_169plus", "post3_14to84", "post3_85plus"),
            # ticktext = c("Post 1st (14–112)", "Post-2nd (14–84)", "Post-2nd (85–168)", "Post-2nd (169+)", "Post-3rd (14–84)", "Post-3rd (85+)")
          ),
          yaxis = list(
            title = "Percent positive anti-Nucleocapsid",
            ticksuffix = "%" 
          ),
          margin = list(l = 50, r = 0, b = 50, t = 75)
        )      
      
    })
    
    
    # ---------- Build dynamic layout to be displayed with user selection ----------
    
    output$dynamicLayout <- renderUI({
      
      if (input$data_type == "titres") {
        
        # ALL DATA
        if (input$plot_mode == "all_data") {
          
          card_title <- ifelse(input$protein == "spike", "Roche S Titres All Study Data", "Roche N Titres All Study Data")
          
          card(
            height = "75vh",
            full_screen = TRUE,
            style = "max-width: 1000px; margin: 0 auto;",  # cap width + center
            card_body(
              uiOutput(ns("all_plot")),
            )
          )
          
          # COMPARE  
        } else if (input$plot_mode == "compare") {
          
          choices <- level_choices_for_key(input$variable)  # named vector: labels -> values
          left_default  <- if (length(choices) >= 1) unname(choices[[1]]) else NULL
          right_default <- if (length(choices) >= 2) unname(choices[[2]]) else left_default
          
          layout_columns(style = "margin-top: 0px;",
                         card(
                           height = "70vh",
                           #fill = FALSE,
                           #style = css(grid_template_columns = "1fr 3fr"), # 
                           full_screen = TRUE,
                           card_header(
                             # Level for left compare based on sidebar's variable choice
                             pickerInput(inputId = ns("level1"),
                                         choices = level_choices_for_key(input$variable),
                                         selected = left_default,
                                         multiple = FALSE,
                                         options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
                                         width = "100%")
                           ),
                           card_body(
                             uiOutput(ns("comp_plot1")),
                           )
                         ),
                         card(
                           height = "70vh",
                           full_screen = TRUE,
                           card_header(
                             # Level for right compare based on sidebar's variable choice
                             pickerInput(inputId = ns("level2"),
                                         choices = level_choices_for_key(input$variable),
                                         selected = right_default,
                                         multiple = FALSE,
                                         options = pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
                                         width = "100%")   
                           ),
                           card_body(
                             uiOutput(ns("comp_plot2")),
                           )
                         )
          )
          
          # MULTIPLE VARIABLE SELECTION    
        } else if (input$plot_mode == "multi_var") {
          
          card(
            height = "75vh",
            full_screen = TRUE,
            style = "max-width: 1000px; margin: 0 auto;",  # cap width + center
            card_body(
              uiOutput(ns("multi_plot"))
            )
          )
          
        }
        
      } else if (input$data_type == "serocon") {
        
        card(
          height = "75vh",
          style = "max-width: 1000px; margin: 0 auto;",  # cap width + center
          card_body(
            plotlyOutput(ns("sero_plot"))
          )
        )
        
      } else {
        
        card(
          height = "75vh",
          style = "max-width: 1000px; margin: 0 auto;",  # cap width + center
          card_body(
            plotlyOutput(ns("antiN_plot"))
          )
        )
        
        
      }
        
      
      
    }) # dynamicLayout
                  
  }) # moduleServer    
}