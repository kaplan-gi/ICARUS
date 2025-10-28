# Title: COVID-19 Serology Multicentre Collaborative Shiny Application
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2025-Oct-28
# Updated: 
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5

# link: https://kaplan-gi.shinyapps.io/COVID_Serology_Multicentre/

# version notes:
# 1. update to include study landing page
# 2. include serology study with only aggregate data shared between sites
# 3. recommend the following for UI design:
# https://rstudio.github.io/bslib/articles/dashboards/index.html
# https://www.youtube.com/watch?v=avZ7TDTRnVo
# 4. the more intensive data processing to a script outside of the app

source("global.R", local = TRUE)$value
source("map_study1.R", local = TRUE)$value
source("demographics_study1.R", local = TRUE)$value
source("compare_study1.R", local = TRUE)$value
source("map_study2.R", local = TRUE)$value
source("demographics_study2.R", local = TRUE)$value
source("plots_study2.R", local = TRUE)$value

ui <- fluidPage(
  
  useShinyjs(),
  
  title = "Collaborative COVID-19 Serology Visualization",
  
  # Specify various styling elements
  # theme = shinytheme("united"),
  theme = bs_theme(bootswatch = "united",
                   primary = "#408697",
                   base_font = "Ubuntu"),
  
  # Format Shiny error messages
  tags$head(
    tags$style(
      HTML(
        ".shiny-output-error-validation {
          text-align: center; 
          margin: auto; 
          width: 100%; 
          color: #D95F02; 
          font-style: italic; 
          font-weight: bold; 
          font-size: 115%;
        }"
      )
    )
  ),
  
  # Format Action button
  tags$head(
    tags$style(
      HTML(
        ".btn {
          color:rgb(255,255,255); 
          border-color: #408697; 
          background-color: #408697;
        }"
      )
    )
  ),
  
  # Format Card header
  tags$head(
    tags$style(
      HTML(
        ".card-header {
          background-color: #D4DADC;
        }"
      )
    )
  ),
  
  # Format Leaflet popup font
  tags$head(
    tags$style(
      HTML(
        ".leaflet-pane {
          font-family: Ubuntu;
        }"
      )
    )
  ),
  
  # Format sidebar text wrapping for pretty widgets
  tags$style(
    ".pretty { 
      white-space: normal; 
      margin-bottom: 4px;
    }
    .pretty .state label {
      line-height: 1.25em; 
      margin-top: -2px;
    }
    .pretty .state label::after, .pretty .state label::before {
      top: -2px;
    }"
  ),
  
  # Format sidebar transparency for mobile
  tags$head(
    tags$style(
      HTML(
        ".bslib-gap-spacing .sidebar {
          background: rgba(212, 218, 220, 0.7)
        }"
      )
    )
  ),
  
  # Format clickable cards for each of the studies on the site
  tags$head(
    # Load Font Awesome 6 from CDN
    tags$link(
      rel = "stylesheet", 
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    
    # Format pretty radio buttons
    tags$style(
      HTML("
        .pretty.p-default .state label {
          line-height: 15px; /* Align text with the radio button */
          font-size: 15px; /* Increase text size */
        }
      ")
    ),
    
    # # Format shiny picker dropdown
    # tags$style(
    #   HTML("
    #     .bootstrap-select .dropdown-menu li a span.text {
    #       white-space: normal !important;
    #       word-wrap: break-word !important;
    #     }
    #   ")
    # ),
    
    # Custom card styling
    tags$style(
      HTML(
        ".study-card {
          border: 1px solid #ccc;
          border-radius: 10px;
          padding: 20px;
          margin: 20px;
          text-align: center;
          cursor: pointer;
          transition: transform 0.2s ease-in-out;
          box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
          background-color: #F6F6F6 ;
        }
        .study-card:hover {
          transform: scale(1.03);
          background-color: #F8F8F8;
        }
        .study-card i {
          font-size: 50px;
          margin-bottom: 10px;
          color: #408697;
          transition: color 0.3s ease;
        }
        .study-card:hover i {
          color: #52D6F4;
        }"
      )
    )
  ),
  
  uiOutput("main_ui")
  
)

server <- function(input, output, session) {
  
  map_study1Server("map_study1")
  demographics_study1Server("demographics_study1")
  compare_study1Server("compare_study1")
  map_study2Server("map_study2")
  demographics_study2Server("demographics_study2")
  plots_study2Server("plots_study2")
  #metaServer("meta")
  
  # Session storage to ensure welcome pop-up only shows up on first load of page
  observe({
    runjs("
    if (!sessionStorage.getItem('welcome_shown')) {
      Shiny.setInputValue('showWelcome', true, {priority: 'event'});
      sessionStorage.setItem('welcome_shown', 'true');
    }
  ")
  })
  
  # Modal with welcome message
  observeEvent(input$showWelcome, {
    showModal(
      modalDialog(
        title = "Welcome to the COVID-19 Serosurveillance in IBD Visualization Tool",
        HTML(
          "<p>
          <b>About</b><br>
          This interactive tool integrates pre- and post-vaccination serosurveillance data from the STOP COVID-19 in IBD and ICARUS-IBD cohorts. Visual trends in SARS-CoV-2 anti-S and anti-N antibody levels in IBD populations from around the world are presented.<br><br>
        
          <b>How to Cite</b><br>
          Please cite the following publications to use figures or information found on this site:
            <ul style='font-size:90%;'>
              <li>STOP COVID-19 in IBD serosurveillance: Kaplan, G.G., Ma, C., Charlton, C., et. al. Antibody response to SARS-CoV-2 among individuals with IBD diminishes over time: a serosurveillance cohort study. <i>Gut</i>. 2022;71(6):1229-1231. https://doi.org/10.1136/gutjnl-2021-325238</li>
              <li>ICARUS-IBD seroprevalence: Wong, S.Y., Wellens, J., Helmus, D., Marlow, L., Brann, S., et. al. Geography Influences Susceptibility to SARS-CoV-2 Serological Response in Patients With Inflammatory Bowel Disease: Multinational Analysis From the ICARUS-IBD Consortium. <i>Inflammatory Bowel Diseases</i>. 2023 Nov;29(11):1693–1705. https://doi.org/10.1093/ibd/izad097</li>
            </ul><br>
        
          <b>Funding</b><br>
          Development of this web tool was made possible by the Helmsley Charitable Trust.
        </p>"
        ),
        
        div(
          class = "logo", 
          tags$a(
            href = "https://helmsleytrust.org/", 
            target = "_blank",
            img(
              src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD-GIVES_HemsleyLogo.png", # Need to request/find a higher resolution image
              height = "80px", 
              width = "200px",          
              style = "margin: 0px 40px; display: block; margin-left: auto; margin-right: auto"
            )
          )
        ),
        
        size = "l",
        footer = modalButton("Close"),
        easyClose = TRUE
      )
    )
  })
  # showModal(
  #   modalDialog(
  #     title = "Welcome to the COVID-19 Serosurveillance in IBD Visualization Tool",
  #     HTML(
  #       "<p>
  #         <b>About</b><br>
  #         This interactive tool integrates pre-vaccination serosurveillance data from the STOP COVID-19 in IBD and ICARUS-IBD cohorts. Visual trends in SARS-CoV-2 anti-S and anti-N antibody levels in IBD populations from around the world are presented.<br><br>
  #       
  #         <b>How to Cite</b><br>
  #         Please cite the following publications to use figures or information found on this site:
  #           <ul style='font-size:90%;'>
  #             <li>STOP COVID-19 in IBD serosurveillance: Kaplan, G.G., Ma, C., Charlton, C., et. al. Antibody response to SARS-CoV-2 among individuals with IBD diminishes over time: a serosurveillance cohort study. <i>Gut</i>. 2022;71(6):1229-1231. https://doi.org/10.1136/gutjnl-2021-325238</li>
  #             <li>ICARUS-IBD seroprevalence: Wong, S.Y., Wellens, J., Helmus, D., Marlow, L., Brann, S., et. al. Geography Influences Susceptibility to SARS-CoV-2 Serological Response in Patients With Inflammatory Bowel Disease: Multinational Analysis From the ICARUS-IBD Consortium. <i>Inflammatory Bowel Diseases</i>. 2023 Nov;29(11):1693–1705. https://doi.org/10.1093/ibd/izad097</li>
  #           </ul><br>
  #       
  #         <b>Funding</b><br>
  #         Development of this web tool was made possible by the Helmsley Charitable Trust.
  #       </p>"
  #     ),
  #   
  #     div(
  #       class = "logo", 
  #       tags$a(
  #         href = "https://helmsleytrust.org/", 
  #         target = "_blank",
  #         img(
  #           src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD-GIVES_HemsleyLogo.png", # Need to request/find a higher resolution image
  #           height = "80px", 
  #           width = "200px",          
  #           style = "margin: 0px 40px; display: block; margin-left: auto; margin-right: auto"
  #         )
  #       )
  #     ),
  #   
  #     size = "l",
  #     footer = modalButton("Close"),
  #     easyClose = TRUE
  #   )
  # )
  
  # Track what the user is viewing
  current_view <- reactiveVal("landing")

  # Observer to read query string on app launch to ensure app starts on correct study based on URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$study)) {
      current_view(query$study)
    } else {
      current_view("landing")
    }
  })
  
  observeEvent(input$go_study1, {
    updateQueryString("?study=study1", mode = "replace")
    current_view("study1")
  })
  
  observeEvent(input$go_study2, {
    updateQueryString("?study=study2", mode = "replace")
    current_view("study2")
  })
  
  observeEvent(input$back, {
    updateQueryString("?study=landing", mode = "replace")
    current_view("landing")
  })         
  
  observeEvent(input$back, {
    shinyjs::runjs("location.reload();")
  })
  
  # JS click triggers for the boxes
  runjs("
    $(document).on('click', '#go_study1', function() {
        Shiny.setInputValue('go_study1', true, {priority: 'event'});
    });
    $(document).on('click', '#go_study2', function() {
        Shiny.setInputValue('go_study2', true, {priority: 'event'});
    });
  ")
  
  # Dynamically generate UI based on the current view
  output$main_ui <- renderUI({
    if (current_view() == "landing") {
      
      tagList(
        div(style = "text-align: center; padding: 20px; background-color: #363538;",
            h1("Multicentre COVID-19 Serology in IBD Dashboard", style = "margin: 0; font-family: Ubuntu; color: #F6F6F6;")
        ),
        
        fluidRow(
          column(2), # blank column for spacing
          column(4,
                 div(class = "study-card", id = "go_study1",
                     tags$i(class = "fa-solid fa-virus-covid"),
                     h4("COVID-19 Serosurveillance Study")
                 )
          ),
          column(4,
                 div(class = "study-card", id = "go_study2",
                     tags$i(class = "fa-solid fa-syringe"),
                     h4("COVID-19 Vaccine Serology Study")
                 )
          ),
          column(2) # blank column for spacing
        ),
        
        fluidRow(
          column(1), # blank column for spacing
          column(10,
                 HTML("<p style = 'margin: 20px 40px; text-align: center; font-size: 100%;'>
                       
                      <b>Support of research and infrastructure for this web application was generously provided by the Leona M. and Harry B. Helmsley Charitable Trust.<br></b></p>"
                 ),
                 div(class = "logo", 
                     tags$a(
                       href = "https://helmsleytrust.org/", 
                       target = "_blank",
                       img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD-GIVES_HemsleyLogo.png", # Need to request/find a higher resolution image
                           height = "80px", width = "200px",          
                           style = "margin: 0px 40px; display: block; margin-left: auto; margin-right: auto")
                     ),
                 ),
                 
                 HTML("<p style = 'margin: 20px 40px; font-size: 100%;'><br>
                       <b>Application Development</b><br>
                       This web application was developed by the Kaplan Global Epidemiology Lab at the University of Calgary. It was constructed using the <i>Shiny</i> framework with dependancies on the R-compatible <i>leaflet</i>, <i>plotly</i>, and <i>ggplot2</i>  modules.<br><br>
                      
                      <b>Using and Citing this Work</b><br>
                      Please cite the following if using any of the figures or information found on this site:<br>
                 
                        <ul style= 'font-size:90%; margin-left: 30px'>
                          <li>STOP COVID-19 in IBD serosurveillance: Kaplan, G.G., Ma, C., Charlton, C., et. al. Antibody response to SARS-CoV-2 among individuals with IBD diminishes over time: a serosurveillance cohort study. <i>Gut</i>. 2022;71(6):1229-1231. https://doi.org/10.1136/gutjnl-2021-325238</li><br>
                          <li>ICARUS-IBD seroprevalence: Wong, S.Y., Wellens, J., Helmus, D., Marlow, L., Brann, S., et. al. Geography Influences Susceptibility to SARS-CoV-2 Serological Response in Patients With Inflammatory Bowel Disease: Multinational Analysis From the ICARUS-IBD Consortium. <i>Inflammatory Bowel Diseases</i>. 2023 Nov;29(11):1693–1705. https://doi.org/10.1093/ibd/izad097</li>
                        </ul><br>
                </p>"),
          ),
          column(1) # blank column for spacing
        )
        
      ) # tagList closing bracket
      
    } else if (current_view() == "study1") {
      tagList(
        h2(),
        actionButton("back", "← Go to Study Selection Landing Page"),  
        h2(),
        div(style = "text-align: center; padding: 20px; background-color: #363538;",
            h1("COVID-19 Serosurveillance Study", style = "margin: 0; font-family: Ubuntu; color: #F6F6F6;")
        ),
        
        # Tabs
        # Dynamically presenting tabs so tab list goes in server, not UI
        navset_tab(
          
          nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Study Sites"),
                    map_study1UI("map_study1")         
          ),
          
          nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Demographics"),
                    demographics_study1UI("demographics_study1")         
          ),
          
          nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125%;", "Compare Studies"),
                    compare_study1UI("compare_study1")
          ),
          
          #nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125%;", "Results"),
          #          resultsUI("results")
          #),
          
          # nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Meta-Analysis"),
          #           metaUI("meta")         
          #  ),
          
          nav_spacer(),
          
          nav_panel(tags$header(style = "text-align:center; padding-right:10px; padding-left:10px; font-weight:bold; font-size:125% ;", 
                                shiny::icon("circle-info")),
                    HTML("<p style = 'margin: 20px 40px; font-size: 115%;'>
                        <b>COVID-19 in the IBD Population</b><br>
                        Since individuals with IBD often live with an altered immune system due to disease activity or therapeutic treatment, it is important to measure the seroprevalence of COVID-19 in this subpopulation. COVID-19 vaccines are designed to stimulate an adaptive immune response to the SARS-CoV-2 spike protein antigen. Therefore, the immune response to the vaccine can be measured by the level of SARS-CoV-2 spike protein antibody that an individual produces in the period following vaccination. Natural infection also produces an immune response that is commonly measured either by the level of spike protein antibody or by the level of nucleocapsid protein antibody present. Nucleocapsid protein antibody testing is used to differentiate between the immune response from a natural SARS-CoV-2 infection verses the immune response from a vaccine.<br><br>
                        
                      <b>Multi-site Collaboration</b><br>
                      Robust research requires the recruitment of many participants and the careful consideration of confounding factors. Multi-site collaboration has the benefits of increasing the size of the study population and illuminating trends that remain consistant despite the regional differences observed during the course of the pandemic. This project brings together several independant but similar studies conducted in different locations around the world.<br><br>
                      
                      <b>Funding</b><br>
                      Support of research and infrastructure for this web application was generously provided by the Leona M. and Harry B. Helmsley Charitable Trust.<br></p>"),
                    
                    div(class = "logo", 
                        tags$a(
                          href = "https://helmsleytrust.org/", 
                          target = "_blank",
                          img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD-GIVES_HemsleyLogo.png", # Need to request/find a higher resolution image
                              height = "80px", width = "200px",          
                              style = "margin: 0px 40px; display: block; margin-left: auto; margin-right: auto")
                        ),
                    ),
                    
                    HTML("<p style = 'margin: 20px 40px; font-size: 115%;'><br>
                       <b>Application Development</b><br>
                       This web application was developed by the Kaplan Global Epidemiology Lab at the University of Calgary. It was constructed using the <i>Shiny</i> framework with dependancies on the R-compatible <i>leaflet</i>, <i>plotly</i>, and <i>ggplot2</i> modules.<br><br>
                      
                      <b>Using and Citing this Work</b><br>
                      Please cite the following if using any of the figures or information found on this site:
                      
                      <ul style= 'font-size: 100%; margin-left: 30px'>
                          <li>STOP COVID-19 in IBD serosurveillance: Kaplan, G.G., Ma, C., Charlton, C., et. al. Antibody response to SARS-CoV-2 among individuals with IBD diminishes over time: a serosurveillance cohort study. <i>Gut</i>. 2022;71(6):1229-1231. https://doi.org/10.1136/gutjnl-2021-325238</li><br>
                          <li>ICARUS-IBD seroprevalence: Wong, S.Y., Wellens, J., Helmus, D., Marlow, L., Brann, S., et. al. Geography Influences Susceptibility to SARS-CoV-2 Serological Response in Patients With Inflammatory Bowel Disease: Multinational Analysis From the ICARUS-IBD Consortium. <i>Inflammatory Bowel Diseases</i>. 2023 Nov;29(11):1693–1705. https://doi.org/10.1093/ibd/izad097</li>
                      </ul><br>
                  
                    </p>"), 
                    
          )
        )
        
        
      )
    
    } else if (current_view() == "study2") {
      tagList(
        h2(),
        actionButton("back", "← Go to Study Selection Landing Page"),  
        h2(),
        div(style = "text-align: center; padding: 20px; background-color: #363538;",
            h1("COVID-19 Vaccine Serology Study", style = "margin: 0; font-family: Ubuntu; color: #F6F6F6;")
        ),
        # Tabs
        # Dynamically presenting tabs so tab list goes in server, not UI
        navset_tab(
          
          nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Study Sites"),
                    map_study2UI("map_study2") 
          ),
          
          nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125%;", "Demographics"),
                    demographics_study2UI("demographics_study2") 
          ),
          
          nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "View Plots"),
                    plots_study2UI("plots_study2")   
          ),
          
          nav_spacer(),
          
          nav_panel(tags$header(style = "text-align:center; padding-right:10px; padding-left:10px; font-weight:bold; font-size:125% ;", 
                                shiny::icon("circle-info")),
                    HTML("<p style = 'margin: 20px 40px; font-size: 115%;'>
                        <b>COVID-19 in the IBD Population</b><br>
                        Since individuals with IBD often live with an altered immune system due to disease activity or therapeutic treatment, it is important to measure the seroprevalence of COVID-19 in this subpopulation. COVID-19 vaccines are designed to stimulate an adaptive immune response to the SARS-CoV-2 spike protein antigen. Therefore, the immune response to the vaccine can be measured by the level of SARS-CoV-2 spike protein antibody that an individual produces in the period following vaccination. Natural infection also produces an immune response that is commonly measured either by the level of spike protein antibody or by the level of nucleocapsid protein antibody present. Nucleocapsid protein antibody testing is used to differentiate between the immune response from a natural SARS-CoV-2 infection verses the immune response from a vaccine.<br><br>
                        
                      <b>Multi-site Collaboration</b><br>
                      Robust research requires the recruitment of many participants and the careful consideration of confounding factors. Multi-site collaboration has the benefits of increasing the size of the study population and illuminating trends that remain consistant despite the regional differences observed during the course of the pandemic. This project brings together several independant but similar studies conducted in different locations around the world.<br><br>
                      
                      <b>Funding</b><br>
                      Support of research and infrastructure for this web application was generously provided by the Leona M. and Harry B. Helmsley Charitable Trust.<br></p>"),
                    
                    div(class = "logo", 
                        tags$a(
                          href = "https://helmsleytrust.org/", 
                          target = "_blank",
                          img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD-GIVES_HemsleyLogo.png", # Need to request/find a higher resolution image
                              height = "80px", width = "200px",          
                              style = "margin: 0px 40px; display: block; margin-left: auto; margin-right: auto")
                        ),
                    ),
                    
                    HTML("<p style = 'margin: 20px 40px; font-size: 115%;'><br>
                       <b>Application Development</b><br>
                       This web application was developed by the Kaplan Global Epidemiology Lab at the University of Calgary. It was constructed using the <i>Shiny</i> framework with dependancies on the R-compatible <i>leaflet</i>, <i>plotly</i>, and <i>ggplot2</i> modules.<br><br>
                      
                      <b>Using and Citing this Work</b><br>
                      Please cite the following if using any of the figures or information found on this site:
                      
                      <ul style= 'font-size: 100%; margin-left: 30px'>
                          <li>STOP COVID-19 in IBD serosurveillance: Kaplan, G.G., Ma, C., Charlton, C., et. al. Antibody response to SARS-CoV-2 among individuals with IBD diminishes over time: a serosurveillance cohort study. <i>Gut</i>. 2022;71(6):1229-1231. https://doi.org/10.1136/gutjnl-2021-325238</li><br>
                          <li>ICARUS-IBD seroprevalence: Wong, S.Y., Wellens, J., Helmus, D., Marlow, L., Brann, S., et. al. Geography Influences Susceptibility to SARS-CoV-2 Serological Response in Patients With Inflammatory Bowel Disease: Multinational Analysis From the ICARUS-IBD Consortium. <i>Inflammatory Bowel Diseases</i>. 2023 Nov;29(11):1693–1705. https://doi.org/10.1093/ibd/izad097</li>
                      </ul><br>
                  
                    </p>"), 
                    
          )
        )
      ) # tagList closing bracket
    }
  })
}

shinyApp(ui, server)

