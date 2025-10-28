# Title: COVID-19 Serology Multicentre Collaborative Shiny Application
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2023-Jan-17
# Updated: 2025-Jul-22
# R version 4.4.2 (2024-10-31)
# Platform: Apple M4 (ARM64)
# Running under: macOS Sequoia 15.5

# Libraries ----------------------------------------------------------------------------------------#
library(rsconnect)
library(shiny) 
library(shinyjs) # show/hide function
library(bslib) 
library(bsicons)
library(shinyWidgets)
library(htmltools)
library(tidyverse) 
library(thematic) # trying to change ggplot font to Ubuntu
library(plotly) 
library(RColorBrewer)
library(shinycssloaders)
library(leaflet) 
library(leaflet.esri)
library(sf)
library(geojsonsf)
library(fontawesome)
library(viridis)
library(later) # for updating URL strings after delay
library(DT)

# Set Options ----------------------------------------------------------------------------------------#
options(spinner.color="#408697")


# Data Prep ----------------------------------------------------------------------------------------#

# load site data
#regData <- geojson_sf("~/Dropbox/COVID_ShinyApp/In_development/HelmsleyCOVID/collab_geofiles.geojson")
regData <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/Sero/main/collab_geofiles.geojson")
mapData_study2 <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/collab_geofiles_study2.geojson")

# load serology data
#serology_raw <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Sero/main/sero_old_format_08Aug22.csv")
#serology_raw <- read.csv("~/Dropbox/COVID_ShinyApp/In_development/HelmsleyCOVID/COVID_collab_TEST.csv")
#serology_raw <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Sero/main/COVID_collab_TEST.csv")
#serology_raw <- read.csv("~/Dropbox/COVID_ShinyApp/In_development/HelmsleyCOVID/COVID_collab_COMBINE.csv", na.strings = c("", "NA"))
serology_raw <- read.csv("https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/COVID_collab_COMBINE.csv", na.strings = c("", "NA"))

table_raw <- read.csv("https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/ICARUS_data_table.csv")

serocon_raw <- read.csv("https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/ICARUS_seroconversion.csv")

antiN_raw <- read.csv("https://raw.githubusercontent.com/kaplan-gi/ICARUS/main/ICARUS_antiN.csv")

# Merge study 2 map data with study 2 data table
mergedStudy2 <- merge(mapData_study2, table_raw, by = "site")

assays <- data.frame(Assay = c("Abbott", "Roche", "COV2T"),
                     assay_color = c(viridis(n = 14, option = "D")[7], viridis(n = 14, option = "D")[11], viridis(n = 14, option = "D")[3]), # "#408697","#1B9E77","#7570B3"
                     y_minN = c(0.01,0.01,NA),
                     y_maxN = c(12,350,NA),
                     #y_maxN = c(0.1,0.8,NA),
                     thresholdN = c(1.4,1,NA),
                     threshold2N = c(0.7,NA,NA),
                     y_minS = c(-0.1,-0.5,-1.25),
                     y_maxS = c(130000,7500, 100),
                     #y_maxS = c(5.2,4.5,2),
                     thresholdS = c(50,0.8,1),
                     messageN = c("Abbott SARS-CoV IgG II Quant assay threshold",
                                  "Roche Elecsys SARS-CoV-2 anti-N assay threshold",
                                  NA),
                     message2N = c("validated threshold based on Bolotin S, Tran V, Osman S, et al. J Infect Dis. 2021:223(8):1334-1338",NA,NA),
                     messageS = c("Abbott SARS-CoV IgG II Quant assay threshold",
                                  "Roche Elecsys SARS-CoV-2 anti-S assay threshold",
                                  "Siemens Healthineers COV2T assay threshold"))

serology <- serology_raw %>% 
  filter(Vaccine_Status == "Pre-1st",
         !is.na(Serology_Date)) %>% 
  mutate(Serology_Date = as.Date(Serology_Date),
         Site_format = gsub(".*\\s*-\\s*", "", Site),
         Country = gsub("\\s*-\\s*.*", "", Site),
         strip_label = factor(strip_label, levels = c("Pre-1st", "Post-1st", "Post-2nd<br>(1-8 Weeks)", "Post-2nd<br>(8+ Weeks)", 
                                                      "Post-3rd<br>(1-8 Weeks)", "Post-3rd<br>(8+ Weeks)", "Post-4th"))) %>% 
  mutate_at(vars(Sex, IBD_Type, Age_Group, decade), ~replace_na(., "Unknown")) %>% 
  left_join(.,assays, by = "Assay")

sum <- serology %>% 
  group_by(Assay) %>% 
  summarize(maxS = max(RBD_Value))

# create factors and labels for serocon dataset
serocon_data <- serocon_raw
serocon_data$region <- factor(serocon_data$region,
                              levels = c("east", "west"),
                              labels = c("East", "West"))
serocon_data$BMI_category <- factor(serocon_data$BMI_category,
                                    levels = c("underweight", "healthy_weight", "overweight", "obese"),
                                    labels = c("Underweight", "Healthy weight", "Overweight", "Obese"))
serocon_data$label_serocon <- paste0(serocon_data$BMI_category, "<br>", serocon_data$percent_seroconvert, "%<br>(n = ", serocon_data$n_seroconvert, "/", serocon_data$total_sero, ")")
serocon_data$label_maxRocheS <- paste0(serocon_data$BMI_category, "<br>", serocon_data$percent_max_RocheS, "%<br>(n = ", serocon_data$n_max_RocheS, "/", serocon_data$total_max_RocheS, ")")

# create factors and labels for antiN dataset
antiN_data <- antiN_raw
antiN_data$time_period <- factor(antiN_data$time_period,
                                 levels = c("post1_14to112", "post2_14to84", "post2_85to168", "post2_169plus", "post3_14to84", "post3_85plus"),
                                 labels = c("Post 1st (14–112)", "Post-2nd (14–84)", "Post-2nd (85–168)", "Post-2nd (169+)", "Post-3rd (14–84)", "Post-3rd (85+)"))
antiN_data$vaccine_type <- factor(antiN_data$vaccine_type, 
                                  levels = c("mRNA", "adenovirus", "inactivated"),
                                  labels = c("mRNA", "Adenovirus", "Inactivated"))
antiN_data$label <- paste0(antiN_data$time_period, "<br>", antiN_data$percent_pos_antiN, "%<br>(n = ", antiN_data$n_pos_antiN, "/", antiN_data$total_antiN, ")")


# Plot Design ----------------------------------------------------------------------------------------#
 
# palette <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D") 

palette <- viridis(n = 7)

icons <- awesomeIcons(
  icon = "chart-bar",
  iconColor = "black",
  library = "fa",
  markerColor = "orange"
) 

# Plot 1 - SPIKE by DOSE
# strip_plot <- function(df){
#   validate(
#     need((nrow(df) >= 10), "Your selection resulted in fewer than 10 data points. Please make another selection to view the plot."))
#   plot_ly(df) %>% 
#     add_trace(x = ~strip_label, y = ~RBD_Value, color = ~strip_label, 
#               type = "box",
#               colors = palette,
#               boxpoints = "all",
#               pointpos = 0,
#               jitter = 0.6,
#               hoverinfo = "skip",
#               marker= list(opacity = 0.6),
#               whiskerwidth = 0,
#               fillcolor = "white",
#               showlegend = FALSE) %>% 
#     add_trace(x = c(0, 7), y = 50, xaxis = "x2",
#               type = "scatter",
#               mode = "lines",
#               name = "Abbott SARS-CoV-2 IgG II Quant assay threshold",
#               line = list(color = "#408697"),
#               hoverinfo = "skip") %>% 
#     add_annotations(x = c(6.5, 6.5), xref = "x",
#                     y = c(log(5), log(6.2)), yref = "y",
#                     text = c(" - ve    ", "+ ve    "),
#                     showarrow = FALSE,
#                     font = list(color = "#408697")) %>% 
#     layout(title = "<b>Spike Protein Antibody Levels\nby Vaccination Status</b>",
#            xaxis = list(title = "Vaccination Status",
#                         automargin = TRUE,
#                         tickangle = -52),
#            xaxis2 = list(overlaying = "x",
#                          showticklabels = FALSE),
#            yaxis = list(title = list(text = "Spike Protein Antibody Levels (AU/mL)",
#                                      standoff = 15L),
#                         type = "log",
#                         range = list(log(0.9),5.2),
#                         ticktext = c("1", "10", "100", "1,000", "10,000", "100,000"),
#                         tickvals = c(1, 10, 100, 1000, 10000, 100000)),
#            showlegend = TRUE,
#            legend = list(orientation = "h", y = -0.3, yref = "paper"),
#            #plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6",
#            margin = list(t = 50, r = 0, b = 100, l = 0, pad = 4),
#            font = list(family = "Ubuntu", color = "#363538")) %>% 
#     config(displayModeBar = FALSE)
# }

# Plot 2 - SPIKE by CALENDAR DATE
spike_calendar <- function(df){
  validate(
    need((nrow(df) >= 10), "Your selection resulted in fewer than 10 data points. Please make another selection to view the plot."))
  df %>% 
    plot_ly() %>%
      add_trace(x = ~Serology_Date, y = ~RBD_Value,
                type = "scatter", mode = "markers",
                marker = list(color = "black"),
                hoverinfo = "skip",
                showlegend = FALSE) %>%
      add_trace(x = c(min(df$Serology_Date)-10,max(df$Serology_Date)+15), y = unique(df$thresholdS),
                type = "scatter", mode = "lines",
                name = unique(df$messageS),
                line = list(color = unique(df$assay_color)),
                hoverinfo = "skip") %>%
      add_annotations(x = c(0.95, 0.95), xref = "paper",
                      #x = c(max(df$Serology_Date)+10, max(df$Serology_Date)+10), xref = "paper",
                      y = c(log10(.85*unique(df$thresholdS)), log10(1.25*unique(df$thresholdS))), yref = "y",
                      text = c(" - ve", "+ ve"),
                      showarrow = FALSE,
                      font = list(color = unique(df$assay_color))) %>%
      layout(title = "<b>Spike Protein Antibody Levels\nby Date of Serology</b>",
             xaxis = list(title = "Date of Serology"),
             yaxis = list(title = list(text = "Spike Protein Antibody Levels",
                                       standoff = 15L),
                          type = "log",
                          range = list(unique(df$y_minS),log10(unique(df$y_maxS))),
                          ticktext = c("1", "10", "100", "1,000", "10,000", "100,000"),
                          tickvals = c(1, 10, 100, 1000, 10000, 100000)),
             showlegend = TRUE,
             legend = list(orientation = "h", y = -0.15),
             margin = list(t = 50, r = 0, b = 10, l = 0, pad = 4),
             font = list(family = "Ubuntu"))
}

# Plot 3 - SPIKE by DAYS FROM VACCINE
spike_scatter <- function(df){
  trend <- lm(RBD_Value ~ Days_since_pv, data = df)
  plot_ly(df) %>%
    add_trace(x = ~Days_since_pv, y = ~RBD_Value,
              type = "scatter", mode = "markers",
              marker = list(color = "black"),
              hoverinfo = "skip",
              showlegend = FALSE) %>%
    add_trace(data = df, x = ~Days_since_pv, y = fitted(trend),
              type = "scatter", mode = "lines",
              line = list(color = "darkred"),
              hoverinfo = "skip",
              showlegend = FALSE) %>%
    add_trace(x = c(0, max(df$Days_since_pv) + (.05*max(df$Days_since_pv))), y = 50,
              type = "scatter", mode = "lines",
              name = "Abbott SARS-CoV-2 IgG II Quant assay threshold",
              line = list(color = viridis(n = 14, option = "D")[7]), #"#408697"
              hoverinfo = "skip") %>%
    add_annotations(x = c(max(df$Days_since_pv), max(df$Days_since_pv)), xref = "x",
                    y = c(log(5), log(6.2)), yref = "y",
                    text = c(" - ve", "+ ve"),
                    showarrow = FALSE,
                    font = list(color = viridis(n = 14, option = "D")[7])) %>% # "#408697"
    layout(title = "<b>Spike Protein Antibody Levels\nby Days Since COVID-19 Vaccine</b>",
           xaxis = list(title = "Days Since Vaccine Dose"),
           yaxis = list(title = list(text = "Spike Protein Antibody Levels (AU/ml)",
                                     standoff = 15L),
                        type = "log",
                        range = list(log(0.9),5.1),
                        ticktext = c("1", "10", "100", "1,000", "10,000", "100,000"),
                        tickvals = c(1, 10, 100, 1000, 10000, 100000)),
           showlegend = TRUE,
           legend = list(orientation = "h", y = -0.15),
           margin = list(t = 50, r = 0, b = 10, l = 0, pad = 4),
           font = list(family = "Ubuntu"))
}

spike_scatterb <- function(df){
  validate(
    need((nrow(df) >= 10), "Your selection resulted in fewer than 10 data points. Please make another selection to view the plot."))
  plot_ly(df) %>%
    add_trace(x = ~Days_since_pv, y = ~RBD_Value,
              type = "scatter", mode = "markers",
              marker = list(color = "black"),
              hoverinfo = "skip",
              showlegend = FALSE) %>%
    add_trace(x = c(0, max(df$Days_since_pv) + (.05*max(df$Days_since_pv))), y = 50,
              type = "scatter", mode = "lines",
              name = "Abbott SARS-CoV-2 IgG II Quant assay threshold",
              line = list(color = viridis(n = 14, option = "D")[7]), # "#408697"
              hoverinfo = "skip") %>%
    add_annotations(x = c(max(df$Days_since_pv), max(df$Days_since_pv)), xref = "x",
                    y = c(log(5), log(6.2)), yref = "y",
                    text = c(" - ve", "+ ve"),
                    showarrow = FALSE,
                    font = list(color = viridis(n = 14, option = "D")[7])) %>% # "#408697"
    layout(title = "<b>Spike Protein Antibody Levels\nby Days Since COVID-19 Vaccine</b>",
           xaxis = list(title = "Days Since Vaccine Dose"),
           yaxis = list(title = list(text = "Spike Protein Antibody Levels (AU/ml)",
                                     standoff = 15L),
                        type = "log",
                        range = list(log(0.9),5.1),
                        ticktext = c("1", "10", "100", "1,000", "10,000", "100,000"),
                        tickvals = c(1, 10, 100, 1000, 10000, 100000)),
           showlegend = TRUE,
           legend = list(orientation = "h", y = -0.15),
           margin = list(t = 50, r = 0, b = 10, l = 0, pad = 4),
           font = list(family = "Ubuntu"))
}


#Plot 4 - NUC by CALENDAR DATE
nuc_calendar <- function(df){
  validate(
    need((nrow(df) >= 10), "Your selection resulted in fewer than 10 data points. Please make another selection to view the plot."))
  
  # Include all thresholdN annotations
  annotation1 <- list(
    list(
      x = 0.95, xref = "paper",
      y = log10(0.85 * unique(df$thresholdN)), yref = "y",
      text = " - ve",
      showarrow = FALSE,
      font = list(color = unique(df$assay_color))
    ),
    list(
      x = 0.95, xref = "paper",
      y = log10(1.25 * unique(df$thresholdN)), yref = "y",
      text = " + ve",
      showarrow = FALSE,
      font = list(color = unique(df$assay_color))
    )
  )
  
  # Include threshold2N annotations if message2N is not NA
  annotation2 <- if (!is.na(unique(df$message2N))) {
    list(
      list(
        x = 0.95, xref = "paper",
        y = log10(0.85 * unique(df$threshold2N)), yref = "y",
        text = " - ve",
        showarrow = FALSE,
        font = list(color = "#D95F02")
      ),
      list(
        x = 0.95, xref = "paper",
        y = log10(1.25 * unique(df$threshold2N)), yref = "y",
        text = " + ve",
        showarrow = FALSE,
        font = list(color = "#D95F02")
      )
    )
  } else {
    list()
  }
  
  all_annotations <- c(annotation1, annotation2)
  
  plot_ly(df) %>%
    add_trace(x = ~Serology_Date, y = ~NC_Value,
              type = "scatter", mode = "markers",
              marker = list(color = "black"),
              hoverinfo = "skip",
              showlegend = FALSE) %>%
    add_trace(x = c(min(df$Serology_Date)-10,max(df$Serology_Date)+20), y = unique(df$thresholdN),
              type = "scatter", mode = "lines",
              name = unique(df$messageN),
              line = list(color = unique(df$assay_color)),
              hoverinfo = "skip") %>%
    # add_annotations(x = c(0.95, 0.95), xref = "paper",
    #                 y = c(log10(.85*unique(df$thresholdN)), log10(1.25*unique(df$thresholdN))), yref = "y",
    #                 text = c(" - ve", "+ ve"),
    #                 showarrow = FALSE,
    #                 font = list(color = unique(df$assay_color))) %>%
    add_trace(x = c(min(df$Serology_Date)-10,max(df$Serology_Date)+20), y = unique(df$threshold2N),
              type = "scatter", mode = "lines",
              name = unique(df$message2N),
              line = list(color = "#D95F02"),
              hoverinfo = "skip") %>%
   #add_annotations(x = c(0.95, 0.95), xref = "paper",
  #                 y = c(log10(.85*unique(df$threshold2N)), log10(1.25*unique(df$threshold2N))), yref = "y",
  #                 text = c(" - ve", "+ ve"),
  #                 showarrow = FALSE,
  #                 font = list(color = ~ifelse(!is.na(df$message2N), "#D95F02", "#D95F0200"))) %>%
    layout(title = "<b>Nucleocapsid Levels\nby Date of Serology</b>",
           annotations = all_annotations,
           xaxis = list(title = "Date of Serology"),
           yaxis = list(title = list(text = "Nucleocapsid Levels",
                                     standoff = 15L),
                        type = "log",
                        range = list(log10(unique(df$y_minN)),log10(unique(df$y_maxN))),
                        ticktext = c("0.01", "0.10", "1.00", "10.0", "100"),
                        tickvals = c(0.01, 0.1, 1, 10, 100),
                        showline = FALSE),
           showlegend = TRUE,
           legend = list(orientation = "h", y = -0.2),
           margin = list(t = 50, r = 0, b = 10, l = 0, pad = 4),
           font = list(family = "Ubuntu"))
}


# Plot 5 - NUC by DAYS FROM C19 DIAGNOSIS
nuc_diagnosis <- function(df){
  validate(
    need((nrow(df) >= 10), "Your selection resulted in fewer than 10 data points. Please make another selection to view the plot."))
  plot_ly(df) %>%
    add_trace(x = ~DX_sero_days, y = ~NC_Value,
              type = "scatter", mode = "markers",
              marker = list(color = "black"),
              hoverinfo = "skip",
              showlegend = FALSE) %>%
    add_trace(x = c(-5, 800), y = 1.4,
              type = "scatter", mode = "lines",
              name = "Abbott SARS-CoV-2 IgG II Quant assay threshold",
              line = list(color = viridis(n = 14, option = "D")[7]), # "#408697"
              hoverinfo = "skip") %>%
    add_annotations(x = c(770, 770), xref = "x",
                    y = c(1.28, 1.55), yref = "y",
                    text = c(" - ve", "+ ve"),
                    showarrow = FALSE,
                    font = list(color = viridis(n = 14, option = "D")[7])) %>% # "#408697"
    add_trace(x = c(-5, 800), y = 0.7,
              type = "scatter", mode = "lines",
              name = "validated threshold based on results from\nBolotin S, Tran V, Osman S, et al. J Infect Dis. 2021:223(8):1334-1338",
              line = list(color = "#D95F02"),
              hoverinfo = "skip") %>%
    add_annotations(x = c(770, 770), xref = "x",
                    y = c(0.58, 0.85), yref = "y",
                    text = c(" - ve", "+ ve"),
                    showarrow = FALSE,
                    font = list(color = "#D95F02")) %>%
    layout(title = "<b>Nucleocapsid Levels\nby Days from COVID-19 Diagnosis</b>",
           xaxis = list(title = "Days from COVID-19 Diagnosis",
                        range = list(0,800)),
           yaxis = list(title = list(text = "Nucleocapsid Levels",
                                     standoff = 15L),
                        range = list(0,8),
                        showline = FALSE),
           showlegend = TRUE,
           legend = list(orientation = "h", y = -0.15),
           margin = list(t = 50, r = 0, b = 10, l = 0, pad = 4),
           font = list(family = "Ubuntu"))
}


# old ggplot version of summary plot design
# strip_plot <- function(df){
#     ggplot(df, aes(x = Vaccine_Status, y = RBD_Value, color = Vaccine_Status)) +
#     geom_boxplot(outlier.shape = NA, fill = '#FFFFFF') +
#     geom_jitter(width = 0.2, alpha = 0.6, size = 1) +
#     scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000), labels = scales::comma) +
#     scale_x_discrete(labels = c("Pre-1st Dose", "Post-1st Dose", "Post-2nd Dose\n(1-8 Weeks)", "Post-2nd Dose\n(8+ Weeks)", 
#                                 "Post-3rd Dose\n(1-8 Weeks)", "Post-3rd Dose\n(8+ Weeks)", "Post-4th Dose")) +
#     scale_color_manual(values = palette, guide = "none") +
#     geom_hline(aes(yintercept= 50, linetype = "Abbott SARS-CoV-2 IgG II Quant assay threshold"), color = "#408697") + 
#     scale_linetype_manual(name = "", values = 1, guide = guide_legend(override.aes = list(color = "#408697"))) +
#     annotate(geom = "text", x = 7.4, y = 73, label = "+ve", color = "#408697", size = 4) + 
#     annotate(geom = "text", x = 7.42, y = 40, label = "-ve", color = "#408697", size = 4) +
#     ggtitle("Spike Protein Antibody Concentration\nby Vaccination Status") +
#     labs(x = "Vaccination Status",
#          y = "Spike Protein Antibody Levels (AU/mL)") +
#     theme(text = element_text(color = "#363538"),
#           plot.title = element_text(size = 15, hjust = 0.5),
#           axis.text.x = element_text(angle = 45, hjust = 1),
#           panel.background = element_rect(fill='#F6F6F6'),
#           panel.grid.minor = element_blank(),
#           panel.grid.major = element_blank(),
#           plot.background = element_rect(fill = '#F6F6F6', color=NA),
#           legend.position = "bottom",
#           legend.justification = "left",
#           legend.text=element_text(size = 12),
#           legend.background = element_rect(fill = '#F6F6F6'))
# }

