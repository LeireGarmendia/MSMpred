library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyalert) 
library(shinyjs)
library(reactable)
library(DiagrammeR)
library(plotly)
library(DT) 

#Long texts to show in the app
source('ui_text.R')

shinyUI(dashboardPage(
  dashboardHeader(title = "MSMpred"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", 
                         startExpanded = TRUE,
                         menuSubItem("MSMpred",
                                     tabName = "MSMpred"),
                         menuSubItem("Example data",
                                     tabName = "exampleData"),
                         menuSubItem("Format data to upload",
                                     tabName = "formatData")),
                
                menuItem("Data", tabName = "data"),
                
                menuItem("Model specification", tabName = "model"),
                
                menuItem("Exploring the data", tabName = "descriptive",
                         menuSubItem("Length of stay",
                                     tabName = "lengthStay"),
                         menuSubItem("Time until absorbing states",
                                     tabName = "timeUntil"),
                         menuSubItem("Instantaneous hazards",
                                     tabName = "nonParam")),
                
                menuItem("Model output", tabName = "fit",
                         menuSubItem("Fitted model",
                                     tabName = "fitted"),
                         menuSubItem("Forest plot",
                                     tabName = "forest"),
                         menuSubItem("Validation",
                                     tabName = "validation"),
                         menuSubItem("Model comparison",
                                     tabName = "comparison")),
                
                menuItem("Predictions", tabName = "predictions"),
                
                menuItem("Help", href = "help_app.html"),
                
                menuItem("About", tabName = "about")
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "MSMpred",
              column(4,
                     box(title = "What is MSMpred?", status = "warning", width = NULL,
                         HTML(text_aboutMSMpred))),
              column(4,
                     box(title = "Sections of MSMpred", status = "warning", width = NULL,
                         HTML(text_sectionsMSMpred))),
              column(4,
                     box(title = "Warning!", status = "warning", width = NULL,
                         HTML(text_security),
                         br(),
                         HTML("Leire Garmendia Bergés:"), 
                         tags$a(href = "mailto:leire.garmendia@upc.edu?Subject=[MSMpred%20Web]-Contact", "leire.garmendia@upc.edu"),
                         br(),
                         HTML("Jordi Cortés Martínez:"), 
                         tags$a(href = "mailto:jordi.cortes-martinez@upc.edu?Subject=[MSMpred%20Web]-Contact", "jordi.cortes-martinez@upc.edu"),
                         br(),
                         HTML("Guadalupe Gómez Melis:"), 
                         tags$a(href = "mailto:lupe.gomez@upc.edu?Subject=[MSMpred%20Web]-Contact", "lupe.gomez@upc.edu")))),
      
      
      tabItem(tabName = "exampleData", 
              fluidRow(column(6, 
                              box(title = "DIVINE", status = "warning", width = NULL,
                                  HTML(text_DIVINE))),
                       column(6,
                              box(title = "States", status = "warning", width = NULL,
                                  HTML(text_states)))), 
              fluidRow(column(6,
                              box(title = "Transitions", status = "warning", width = NULL,
                                  img(src = "MSM_DIVINE.png", width = "100%"))),
                       
                       column(6, 
                              box(title = "Covariates", status = "warning", width = NULL,
                                  HTML(text_covariates))))),
      
      
      tabItem(tabName = "formatData",
              fluidRow(column(4, 
                              box(title = "File format", status = "warning", width = NULL,
                                  HTML(text_format_file))),
                       column(4, 
                              box(title = "States", status = "warning", width = NULL,
                                  HTML(text_format_states))),
                       column(4, 
                              box(title = "Covariates", status = "warning", width = NULL,
                                  HTML(text_format_covariates))))),
      
      
      tabItem(tabName = "data",
              
              navbarPage(title="Data",
                         tabPanel("Data table",
                                  fluidRow(column(10,
                                                  p("Once a dataset is uploaded you need to refresh the page to use the example data.",
                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                  fileInput("filein", "Upload file")),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#data', '_blank')"))),
                                  
                                  p(text_DIVINE_cohort,
                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                  br(),
                                  DT::dataTableOutput("data")),
                         
                         tabPanel("Filter",
                                  fluidRow(column(3,
                                                  uiOutput("filter_covar")),
                                           column(3,
                                                  uiOutput("filter_value")),
                                           column(4,
                                                  actionBttn("save_filter", "Apply", style = "float", color = "warning"),
                                                  actionBttn("reset_filter", "Reset", style = "float", color = "warning")),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#data', '_blank')")))),
                         
                         tabPanel("Labels",
                                  fluidRow(column(10),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#data', '_blank')"))),
                                  fluidRow(column(3,
                                                  uiOutput("names_states")),
                                           column(3,
                                                  textInput("text1", label = "Write the label:", value = "Enter text..."),
                                                  actionBttn("save_state", "Save state label", style = "float", color = "warning")),
                                           column(3,
                                                  uiOutput("names_covar")),
                                           column(3,
                                                  textInput("text2", label = "Write the label:", value = "Enter text..."),
                                                  actionBttn("save_covar", "Save covariate label", style = "float", color = "warning")))),
                         
                         tabPanel("Covariates: descriptive plots",
                                  fluidRow(column(10),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#data', '_blank')"))),
                                  uiOutput("plot_covar_height")),
                         
                         tabPanel("Covariates: descriptive table",
                                  fluidRow(column(10),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#data', '_blank')"))),
                                  htmlOutput("covar_num")))),
      
      
      tabItem(tabName = "model", 
              actionBttn("save_model", "Save model specification", style = "float", color = "warning"),
              hr(),
              navbarPage(title="Model specification",
                         tabPanel("Model",
                                  fluidRow(column(6,
                                                  p("Every time you add/delete a transition or covariate the app automatically makes the needed computations.",
                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                                           column(4,
                                                  p(HTML('<span style="color:orange";>Orange &rarr; Initial state</span>; <span style="color:blue";>Blue &rarr; Transient state</span>; 
                                                         <span style="color:magenta";>Magenta &rarr; Absorbing state</span>; <span style="color:green";>Green &rarr; Isolate state</span>'),
                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#model-specification', '_blank')"))),
                                  fluidRow(column(6, 
                                                  box(title = "Define the transitions", status = "warning", width = NULL,
                                                      uiOutput("from_select"),
                                                      uiOutput("to_select"),
                                                      actionBttn("add", "Add", style = "float", color = "success"),
                                                      actionBttn("delete", "Delete", style = "float", color = "danger")),
                                                  
                                                  p("In order to include several covariates, transitions need a minimum of individuals for each covariate.",
                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                                           column(6,
                                                  box(title = "Multistate model diagram", status = "warning", width = NULL,
                                                      grVizOutput("diagram")),
                                                  box(title = "Number of events for each transition", status = "warning", width = NULL,
                                                      DT::dataTableOutput("num_events"))))),
                         
                         tabPanel("Covariates/time",
                                  fluidRow(column(10),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#model-specification', '_blank')"))),
                                  box(title = "Time specification", status = "warning", width = NULL,
                                      uiOutput("follow_up_time"), 
                                      uiOutput("time_unit")),
                                  
                                  box(title = "Covariates per transition", status = "warning", width = NULL,
                                      dataTableOutput("myTable"))
                         ))),
      
            
      tabItem(tabName = "lengthStay",
              fluidRow(column(10),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#length-of-stay', '_blank')"))),
              box(title = "Box-plots of the length of stay in each state", status = "warning", width = NULL,
                  plotlyOutput("plot_length_stay", height = "700px"))),
      
      
      tabItem(tabName = "timeUntil",
              fluidRow(column(10),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#time-until-absorbing-states', '_blank')"))),
              
              fluidRow(column(6,
                              box(title = "Cumulative incidence curves of the absorbing states", status = "warning", width = NULL,
                                  plotlyOutput("cum_inc_absorbing", height = "700px"))))),
                       # column(6,
                              # box(title = "Survival curves of the absorbing states", status = "warning", width = NULL,
                                  # plotlyOutput("surv_absorbing", height = "700px"))))),
      
      
      tabItem(tabName = "nonParam",
              fluidRow(column(3,
                              uiOutput("start_state")),
                       column(3,
                              uiOutput("selec_covariate")),
                       column(3,
                              uiOutput("end_state")),
                       column(1),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#instantaneous-hazards', '_blank')"))),
              
              bsCollapse(id = "collapseHome2", multiple = TRUE, open = NULL,
                         bsCollapsePanel(HTML('<h4><font color = "white">Instantaneous hazard plot of the transitions 
                                              starting in the selected state</font></h4>'),
                                         plotlyOutput("plot_insthaz_start", height = "700px"),
                                         value = "start",style = "primary"),
                         bsCollapsePanel(HTML('<h4><font color = "white">Instantaneous hazard plot of the transitions 
                                              ending in the selected state</font></h4>'),
                                         plotlyOutput("plot_insthaz_end", height = "700px"),
                                         value = "end",style = "primary"))),
      
      
      tabItem(tabName = "fitted", 
              fluidRow(column(3, 
                              pickerInput(
                                inputId = "select_model",
                                label = "Choose the type of model:", 
                                choices = c("Cox"),
                                options = list(
                                  title = "Choose the type of model")
                              )),
                       column(6, 
                              p("You can use the search box to find the covariates related with a specific transition.",
                                style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                              box(title = "Table of model coefficients:", status = "warning", width = NULL,
                                  DT::dataTableOutput("model")),
                              box(title = "Table of likelihood:", status = "warning", width = NULL,
                                  DT::dataTableOutput("model_loglik"),
                                  DT::dataTableOutput("model_AIC")),
                              box(title = "Table of goodness of fit:", status = "warning", width = NULL,
                                  DT::dataTableOutput("model_tests"))),
                       column(1),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#fitted-model', '_blank')"))),
              fluidRow(column(3, 
                              actionBttn("LS", "Compute the logarithmic score", style = "float", color = "warning"),
                              #Progress bar
                              tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
                              tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),
                              tags$div(
                                id = "my-modal",
                                class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
                                tags$div(
                                  class="modal-dialog",
                                  tags$div(
                                    class = "modal-content",
                                    tags$div(class="modal-header", tags$h4(class="modal-title", "Calculation in progress")),
                                    tags$div(
                                      class="modal-body",
                                      shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE, striped = TRUE)
                                    ),
                                    tags$div(class="modal-footer", tags$button(type="button", class="btn btn-default", `data-dismiss`="modal", "Dismiss"))
                                  )
                                ))
                              ,
                                
                              
                              br(),
                              p(text_log_score,
                                style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       column(6,
                              box(title = "Table of logarithmic score:", status = "warning", width = NULL,
                                  DT::dataTableOutput("logar_score"))))),
      
      
      tabItem(tabName = "forest",
              fluidRow(column(2, 
                              uiOutput("trans"),
                              strong("Select the units to plot for each numerical covariate:"),
                              uiOutput("num_unit")),
                       column(8,
                              box(title = "Forest plot of the covariates taken into account in the selected transition", status = "warning", width = NULL,
                                  plotlyOutput("forest_plot", height = "700px"))),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#forest-plot', '_blank')")))),
      
      
      tabItem(tabName = "validation",
              
              navbarPage(title="Model validation",
                         tabPanel("Linearity",
                                  fluidRow(column(2,
                                                  uiOutput("trans_lin"),
                                                  uiOutput("trans_lin_cov")),
                                           column(8,
                                                  box(title = "Martingale-based residuals of the selected covariate and transition",
                                                      status = "warning", width = NULL, plotlyOutput("martingalas_res", height = "700px"))),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#validation', '_blank')")))),
                         
                         
                         tabPanel("Influential observations",
                                  fluidRow(column(2,
                                                  uiOutput("trans_influ")),
                                           column(8,
                                                  box(title = "dfbetas residuals of the covariates related with the selected transition", 
                                                      status = "warning", width = NULL, plotlyOutput("score_res", height = "700px"))),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#validation', '_blank')")))),
                         
                         
                         tabPanel("Proportionality of the hazards",
                                  fluidRow(column(2,
                                                  uiOutput("trans_prop")),
                                           column(8,
                                                  box(title = "Schoenfeld residuals of the covariates related with the selected transition", 
                                                      status = "warning", width = NULL, plotlyOutput("schoenfeld_res", height = "700px"))),
                                           column(2,
                                                  actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                                               width = '100%',
                                                               icon = icon("list-alt"),
                                                               onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#validation', '_blank')")))))),
      
      
      tabItem(tabName = "comparison",
              fluidRow(column(10),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#model-comparison', '_blank')"))),
              
              box(title = "Table of model comparison:", status = "warning", width = NULL,
                  reactableOutput("model_comp")
              ),
              
              box(title = "Save the models:", status = "warning", width = NULL,
                  actionBttn("save", "Save", style = "float", color = "warning"),
                  br(),
                  p("Save the following session id to use it to upload this information in other session:"),
                  br(),
                  textOutput("session_id")),
              
              box(title = "Load other models:", status = "warning", width = NULL,
                  p("Introduce the session id you want to recover:"),
                  textInput("session_user", label = NULL),
                  br(),
                  actionBttn("load", "Load", style = "float", color = "warning"))),
      
      
      tabItem(tabName = "predictions",
              fluidRow(column(10,
                              p(text_predictions,
                                style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                              
                              p("Every time you add/delete a transition or covariate the app automatically makes the needed computations.",
                                style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")),
                       column(2,
                              actionButton(inputId = 'ab111', label = HTML("<b>Help</b>"),
                                           width = '100%',
                                           icon = icon("list-alt"),
                                           onclick = "window.open('https://www.grbio.eu/pubs/MSMpred/help_app.html#predictions', '_blank')"))),
              
              fluidRow(column(6, 
                              bsCollapse(id = "collapseHome2", multiple = TRUE, open = 'bs1',
                                         bsCollapsePanel(HTML('<h4><font color="white">Characteristics of patient 1</font></h4>'),
                                                         uiOutput("initial_state_1"),
                                                         uiOutput("pred_1"),
                                                         uiOutput("time_pred_1"),
                                                         value = "bs1",style = "primary",
                                                         
                                                         bsCollapsePanel(HTML('<h4><font color = "black">Probability of being in each state</font></h4>'),
                                                                         textOutput("indiv_1"),
                                                                         br(),
                                                                         uiOutput("pred_box_1"),
                                                                         value = "bs2"),
                                                         
                                                         bsCollapsePanel(HTML('<h4><font color = "black">Transition probability plot</font></h4>'),
                                                                         uiOutput("stacked_pred_1"),
                                                                         plotlyOutput("plot_pred_1", height = "500px"),
                                                                         value = "bs3")))),
                       
                       column(6, 
                              bsCollapse(id = "collapseHome2", multiple = TRUE, open = NULL,
                                         bsCollapsePanel(HTML('<h4><font color = "white">Characteristics of patient 2</font></h4>'),
                                                         uiOutput("initial_state_2"),
                                                         uiOutput("pred_2"),
                                                         uiOutput("time_pred_2"),
                                                         value = "bs1",style = "primary",
                                                         
                                                         bsCollapsePanel(HTML('<h4><font color = "black">Probability of being in each state</font></h4>'),
                                                                         textOutput("indiv_2"),
                                                                         br(),
                                                                         uiOutput("pred_box_2"),
                                                                         value = "bs2"),
                                                         
                                                         bsCollapsePanel(HTML('<h4><font color = "black">Transition probability plot</font></h4>'),
                                                                         uiOutput("stacked_pred_2"),
                                                                         plotlyOutput("plot_pred_2", height = "500px"),
                                                                         value = "bs3")))))),
      
      
      tabItem(tabName = "about",
              fluidRow(column(6, 
                              box(title = "GRBIO", status = "warning", width = NULL,
                                  HTML(text_GRBIO),
                                  tags$a(href = "https://grbio.upc.edu/en", "Click here!", target = "_blank")),
                              
                              box(title = "Funding", status = "warning", width = NULL,
                                  HTML(text_funding)),
                              fluidRow(column(4,
                                              img(src = "DIVINE_rectangle.png", width = "75%")),
                                       column(4,
                                              img(src = "GRBIO.png", width = "75%")),
                                       column(4,
                                              img(src = "UPC.png", width = "75%")))),
                       
                       column(6,
                              box(title = "Authors", status = "warning", width = NULL,
                                  userBox(
                                    title = userDescription(
                                      title = "Leire Garmendia Bergés",
                                      type = 2,
                                      image = "https://grbio.upc.edu/en/shared/members-photos/leire_garmendia.jpg",
                                    ),
                                    status = "warning",
                                    collapsed = TRUE,
                                    tags$a(href="https://grbio.upc.edu/en/about-us/cv/leire-garmendia", "Click here!", target = "_blank")),
                                  userBox(
                                    title = userDescription(
                                      title = "Jordi Cortés Martínez",
                                      type = 2,
                                      image = "https://grbio.upc.edu/en/shared/members-photos/jordi_cortes.jpg",
                                    ),
                                    status = "warning",
                                    collapsed = TRUE,
                                    tags$a(href = "https://jordi-cortes.netlify.app/", "Click here!", target = "_blank")),
                                  userBox(
                                    title = userDescription(
                                      title = "Guadalupe Gómez Melis",
                                      type = 2,
                                      image = "https://grbio.upc.edu/en/shared/members-photos/lupe4.jpg",
                                    ),
                                    status = "warning",
                                    collapsed = TRUE,
                                    tags$a(href = "https://grbio.upc.edu/en/about-us/cv/lupe-gomez", "Click here!", target = "_blank")),
                                  userBox(
                                    title = userDescription(
                                      title = "GRBIO",
                                      type = 2,
                                      image = "https://grbio.upc.edu/en/shared/newsletter/grbio.png",
                                    ),
                                    status = "warning",
                                    collapsed = TRUE,
                                    tags$a(href = "https://grbio.upc.edu/en", "Click here!", target = "_blank"))),
                              
                              fluidRow(
                                column(6,
                                       box(title = "Contact info", status = "warning", width = NULL,
                                           "C/ Jordi Girona 1-3, Edifici C5 planta 2 Barcelona, Barcelona 08034 · Spain")),
                                column(6,
                                       box(title = "Leave us a message", status = "warning", width = NULL,
                                           tags$a(href = "mailto:leire.garmendia@upc.edu?Subject=[MSMpred%20Web]-Contact", "Leire Garmendia Bergés"),
                                           br(),
                                           tags$a(href = "mailto:jordi.cortes-martinez@upc.edu?Subject=[MSMpred%20Web]-Contact", "Jordi Cortés Martínez"),
                                           br(),
                                           tags$a(href = "mailto:lupe.gomez@upc.edu?Subject=[MSMpred%20Web]-Contact", "Guadalupe Gómez Melis")))))))
    )
  )
))