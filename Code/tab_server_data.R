#--------------
#Data selection
#--------------
observeEvent(input$filein,{
  #Guardamos los nuevos datos e inicializamos datoslong
  Rvalues$data <- read.table(input$filein$datapath, sep = ",", header = TRUE, stringsAsFactors = TRUE)
  Rvalues$datoslong <- data.frame()
  
  if(nrow(Rvalues$data)==1){
    shinyalert("Oops!", HTML("Warning: columns are not separated by commas."), type = "warning",html=TRUE) 
  }else{
    
    #Identificamos las covariables de la nueva base de datos
    Rvalues$all_covar <- names(Rvalues$data[!str_detect(names(Rvalues$data), "_time") &
                                              !str_detect(names(Rvalues$data), "_status") &
                                              names(Rvalues$data) != "id" &
                                              names(Rvalues$data) != "inistat"])
    names(Rvalues$all_covar) <- Rvalues$all_covar
    Rvalues$labels_covar <- Rvalues$all_covar
    
    #Identificamos de que tipo es cada covariable
    Rvalues$covar_type <- sapply(Rvalues$data[,Rvalues$all_covar],covar_type)


    #Identificamos los nombres de las variables que contienen los tiempos hasta cada
    #estado y el indicador de cada estado
    Rvalues$all_time <- names(Rvalues$data[str_detect(names(Rvalues$data), "_time")])
    Rvalues$all_status <- names(Rvalues$data[str_detect(names(Rvalues$data), "_status")])
    
    if(length(Rvalues$all_time)==0){
      shinyalert("Oops!", HTML("Warning: the time variables are not named as x_time."), type = "warning",html=TRUE) 
    }else{
      if(length(Rvalues$all_status)==0){
        shinyalert("Oops!", HTML("Warning: the status variables are not named as x_status."), type = "warning",html=TRUE) 
      }else{
        if(length(Rvalues$all_status)!=length(Rvalues$all_status)){
          shinyalert("Oops!", HTML("Warning: the number of x_time and x_status variables is different."), type = "warning",html=TRUE) 
        }else{
          if(any(!sapply(Rvalues$data[,Rvalues$all_time], is.numeric))){
            shinyalert("Oops!", HTML("Warning: the time variables are not numerical."), type = "warning",html=TRUE) 
          }else{
            
            #Identificamos los nombres de los estados y el número de estados
            Rvalues$states <- str_remove_all(Rvalues$all_time, "_time")
            Rvalues$labels_states <- Rvalues$states
            Rvalues$num_states <- length(Rvalues$states)
            
            if(!("inistat" %in% colnames(Rvalues$data))){
              shinyalert("Oops!", HTML("Warning: there is not a variable named inistat."), type = "warning",html=TRUE) 
            }else{
              
              problems <- 0
              for(i in length(levels(Rvalues$data[,"inistat"]))){
                if(!(levels(Rvalues$data[,"inistat"])[i] %in% Rvalues$states)){
                  problems <- 1
                }}
              if(problems == 1){
                shinyalert("Oops!", HTML("Warning: at least one of the levels of inistat is not a state."), type = "warning",html=TRUE) 
              }else{
                
                #Creamos la matriz tmat que de momento es todo NA y el vector con el nombre de las
                #transiciones, de momento vacio
                Rvalues$tmat <- matrix(rep(NA,length(Rvalues$states)^2), length(Rvalues$states),
                                       dimnames=list(Rvalues$states, Rvalues$states))
                Rvalues$names_tmat <- c()
                
                #Creamos el data frame nodes, que necesitaremos para crear el diagrama, 
                #donde asignamos un número a cada estado
                Rvalues$id <- c(1:length(Rvalues$states))
                Rvalues$nodes <- data.frame(id = Rvalues$id, names = Rvalues$states)
                Rvalues$colores <- c()
                
                #Creamos el data frame transit, que necesitaremos para crear el diagrama, 
                #donde mediante las columnas from y to especificamos las transiciones del modelo
                Rvalues$transit <- data.frame(from=0, to=0)
                names(Rvalues$transit) = c("from", "to")
                
                #Identificamos los estados iniciales del modelo
                #Para ello miramos las categroias de la variable inistat
                Rvalues$initial_states <- levels(as.factor(Rvalues$data$inistat))
                
                #Inicializamos los vectores donde guardaremos que estados son transitorios, absorbentes...
                Rvalues$transient_states <- character(0)
                Rvalues$absorbing_states <- character(0)
                Rvalues$initial_not_transient_states <- character(0)
                Rvalues$initial_or_transient_states <- character(0)
                Rvalues$out_states <- character(0)
              } 
            }
          }
        }
      }
    }
  }
})


#-------------
#Show the data
#-------------
output$data <- DT::renderDataTable({
  if(is.null(input$filein)){
    Rvalues$data[1:20,]
  }else{
    Rvalues$data
  }
}, rownames= FALSE)


#---------------
#Filter the data
#---------------
#Creamos el selector de covariables
output$filter_covar <- renderUI({
  selectInput("filter_covar", "Choose the covariate:", Rvalues$all_covar)
})

#Creamos el selector de las categorias/valores
observeEvent(input$filter_covar,{
  if(Rvalues$covar_type[which(Rvalues$all_covar==input$filter_covar)]=="factor"){
    output$filter_value <- renderUI({
      checkboxGroupInput("filter_value", "Choose the values:", unique(Rvalues$data[,input$filter_covar]))
    })
  }else{
    output$filter_value <- renderUI({
      sliderInput("filter_value", "Choose the range:", 
                  min = min(Rvalues$data[,input$filter_covar], na.rm = TRUE), 
                  max = max(Rvalues$data[,input$filter_covar], na.rm = TRUE), 
                  value = c(min(Rvalues$data[,input$filter_covar], na.rm = TRUE), max(Rvalues$data[,input$filter_covar], na.rm = TRUE)))
    })
  }
})

#Cuando se clicka el boton reset recuperamos los datos iniciales (sin filtrar)
observeEvent(input$reset_filter,{
  if(is.null(input$filein)){
    Rvalues$data <- divine_data
  }else{
    Rvalues$data <- read.table(input$filein$datapath, sep = ",", header = TRUE, stringsAsFactors = TRUE)
  }
})

#Cuando se clicka el boton save aplicamos el filtro a los datos para quedarnos con el subconjunto de interes
observeEvent(input$save_filter,{
  if(Rvalues$covar_type[which(Rvalues$all_covar==input$filter_covar)]=="factor"){
    Rvalues$data <- filter(Rvalues$data, get(input$filter_covar)==input$filter_value)
  }else{
    Rvalues$data <- filter(Rvalues$data, get(input$filter_covar)>input$filter_value[1] & get(input$filter_covar)<input$filter_value[2])
  }
})


#---------------------------
#Change labels of the states
#---------------------------
output$names_states <- renderUI({
  pickerInput(
    inputId = "names_states",
    label = "Choose the state:", 
    choices = Rvalues$states,
    options = list(
      title = "Choose the state")
  )
})


observeEvent(input$save_state,{
  for(i in 1:length(Rvalues$labels_states)){
    if(Rvalues$labels_states[i]==input$names_states){
      Rvalues$labels_states[i] <- input$text1}}
})



#-------------------------------
#Change labels of the covariates
#-------------------------------
output$names_covar <- renderUI({
  pickerInput(
    inputId = "names_covar",
    label = "Choose the covariate:", 
    choices = Rvalues$all_covar,
    options = list(
      title = "Choose the covariate")
  )
})


observeEvent(input$save_covar,{
  
  Rvalues$labels_covar[Rvalues$all_covar==input$names_covar] <- input$text2

})



#-----------------------------------
#Descriptive plots of the covariates
#-----------------------------------

output$plot_covar <- renderPlotly({
  #Creamos una lista vacia donde iremos guardando los plots de cada covariable
  myplots <- list()
  for (i in 1:length(Rvalues$all_covar)) {
    if(Rvalues$covar_type[i]=="numeric"){
      #Si la variable i es numérica hace su histograma
      gg <- ggplotly(ggplot(Rvalues$data, aes_string(x=Rvalues$all_covar[i])) +
                       geom_histogram(col='black', fill='darkorange2') +
                       labs(y = "Frequency", x = Rvalues$labels_covar[i]) +
                       common_theme)
      
    } else if(Rvalues$covar_type[i]=="factor"){
      #Si la variable i es un factor hace su diagrama de barras
      gg <- ggplotly(ggplot(Rvalues$data, aes_string(x=Rvalues$all_covar[i])) +
                       geom_bar(col='black', fill='darkorange2') +
                       labs(y = "Frequency", x = Rvalues$labels_covar[i]) +
                       common_theme)
                     
    }
    
    #Vamos guardando los plots de cada variable en esta lista 
    #Luego graficaremos todos los plots juntos
    myplots[[i]] <- gg
  }
  
  #Creamos un único gráfico con todos los plots
  nrows <- ceiling(length(Rvalues$all_covar)/2)
  graph <-  subplot(plotlist = myplots, nrows =  nrows, 
                    titleY = TRUE, titleX = TRUE, margin = 0.075-0.0038*nrows)
  annotate_figure(graph)
  

})


output$plot_covar_height <- renderUI({
  plotlyOutput("plot_covar",height=paste0(ceiling(length(Rvalues$all_covar)/2)*300,'px'))})


#-----------------------------------
#Descriptive table of the covariates
#-----------------------------------

output$covar_num <- renderUI({
  print(dfSummary(Rvalues$data[,Rvalues$all_covar], varnumbers = FALSE, graph.col = FALSE, na.col = FALSE),
        method = "render")
})

