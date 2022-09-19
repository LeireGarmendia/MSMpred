#------------------------------
#Analysis of the length of stay
#------------------------------

output$plot_length_stay <- renderPlotly({
  if(length(Rvalues$initial_or_transient_states)>0){
    #Creamos un dataframe con todos los tiempos de estancia de cada estado
    #boxplot: el estado de interés, time: el tiempo de estancia
    #Nos quedamos con datoslong$timepo de las personas que han hecho una de las
    #posibles transiciones desde ese estado
    for(i in 1:length(Rvalues$initial_or_transient_states)){
      if(i==1){
        dd <- data.frame(boxplot=Rvalues$labels_states[which(Rvalues$states == Rvalues$initial_or_transient_states[i])],
                         time=subset(Rvalues$datoslong, (status == 1 & trans %in% Rvalues$tmat[which(Rvalues$nodes$names == Rvalues$initial_or_transient_states[i]),
                                                                                               !is.na(Rvalues$tmat[which(Rvalues$nodes$names == Rvalues$initial_or_transient_states[i]),])]))$time)
      }else{
        dd <- rbind(dd,
                    data.frame(boxplot=Rvalues$labels_states[which(Rvalues$states == Rvalues$initial_or_transient_states[i])],
                               time=subset(Rvalues$datoslong, (status == 1 & trans %in% Rvalues$tmat[which(Rvalues$nodes$names == Rvalues$initial_or_transient_states[i]),
                                                                                                     !is.na(Rvalues$tmat[which(Rvalues$nodes$names == Rvalues$initial_or_transient_states[i]),])]))$time))
      }
    }
    
    #Creamos los boxplot teniendo en cuenta el tiempo y el estado que hemos guardado
    
    ggplotly(ggplot(dd,aes(x=as.factor(boxplot),y=time)) +
      geom_boxplot(fill="darkorange2") +
      labs(y = Rvalues$time_unit, x="") +
        common_theme)
  }
})


#---------------------------------------------
#Analysis of the time until an absorbing state
#---------------------------------------------

#Incidencia acumulada
output$cum_inc_absorbing <- renderPlotly({
  #Guardamos los datos de las columnas que indican si el paciente ha llegado a cada
  #estado absorbente o no
  d_absorbing_status <- Rvalues$data[,paste0(Rvalues$absorbing_states,'_status'),drop=FALSE]
  
  #Cremos una nueva variable last_status para saber cual es el último estado de
  #cada paciente. Si no llega a ningún estado absorbente lo guardamos como censored
  last_status <- rep("Censored",nrow(Rvalues$data))
  pos_ones <- which(d_absorbing_status==1,arr.ind = TRUE)

  last_status[pos_ones[,1]] <- Rvalues$absorbing_states[pos_ones[,2]]
  
  #Calculamos las incidencias acumuladas
  cum_inc <- cuminc(Rvalues$data[,paste0(Rvalues$absorbing_states[1],'_time')], last_status)
  
  #Vemos cuantos estados finales hay (absorbentes + censura) y vemos donde están las censuras
  states_to_plot <- attributes(cum_inc)$names
  pos_cens <- grep(pattern = "Censored",states_to_plot)
  if(length(pos_cens)==0) pos_cens <- 100000

  #Creamos un dataframe con: state: el estado final de interés,
  #time: los tiempos hasta ese estado; incidence: la incidencia acumulada en ese tiempo
  dd <- data.frame(state=c(), time=c(), incidence=c())
  for(i in 1:length(states_to_plot)){
    if(i==pos_cens){
      dd
    }else{
      dd <- rbind(dd,data.frame(state = Rvalues$labels_states[which(Rvalues$states == substr(states_to_plot[i],3,100))],
                                time=cum_inc[[i]]$time,
                                incidence=cum_inc[[i]]$est))
    }
  }
  
  #Creamos el gráfico de las incidencias acumuladas basandonos en el dataframe
  ggplotly(ggplot(dd, aes(x=time,y=incidence,color=state)) +
    geom_line(size = 1) +
    scale_x_continuous(limits = c(0,Rvalues$follow_up_time),breaks = pretty(c(0,Rvalues$follow_up_time))) +
    labs(x = Rvalues$time_unit, y="Cumulative incidence") +
    common_theme)
})


#------------------------------------------------
#Instantaneous hazard of the selected transitions
#------------------------------------------------

#Seleccionamos el estado de salida de interés
#(se plotarán las transiciones que salgan de este estado)
output$start_state <- renderUI({
  radioButtons("start_state", "Choose the starting state of interest:",
               choiceNames = Rvalues$labels_states[which(Rvalues$states %in% Rvalues$initial_or_transient_states)],
               choiceValues = which(Rvalues$states %in% Rvalues$initial_or_transient_states))
})

#Seleccionamos la covariable de interés
output$selec_covariate <- renderUI({
  selectInput("selec_covariate", "Choose the covariate of interest:", c("None", Rvalues$all_covar))
})



output$plot_insthaz_start <- renderPlotly({
  if(!is.null(input$start_state)){
    
    #Paleta de colores
    dd.col <- cols25(max(Rvalues$tmat, na.rm = T))
    names(dd.col)  <- Rvalues$names_tmat
    
    #Cuando no hay covariables calculamos el riesgo de transición basal
    if(input$selec_covariate == "None"){
      #Creamos un dataframe donde guardamos: Transition: la transición de interés,
      #time: el tiempo, hazard: el riesgo de transición en ese tiempo
      dd <- data.frame()
      for(i in Rvalues$tmat[as.numeric(input$start_state),]){
        if(!is.na(i))(
          try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1, subset(Rvalues$datoslong, trans == i), degree = 3)
          
          dd <- rbind(dd,
                      data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard))},silent = TRUE))}
      
      dd_y <- subset(dd, dd$time<Rvalues$follow_up_time)
      ymax <- max(dd_y$hazard,na.rm=TRUE)
      
      #Hacemos el gráfico en función de los valores guardados diferenciando transiciones
      ggplotly(ggplot(dd, aes(x=time,y=hazard,color=Transition)) +
        geom_line(size = 1.2) +
        xlim(0,Rvalues$follow_up_time) +
        labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
        scale_color_manual("Legend", values = dd.col) +
        ylim(0,ymax) +
        common_theme)
      
    }else{
      #Cuando incluimos covariables la idea es la misma pero tenemos en cuenta las
      #covariables como variables estratificadoras
      #Cuando hay más de una covariable hace dos plots para cada una de ellas
      
      if(Rvalues$covar_type[which(Rvalues$all_covar == input$selec_covariate)]=="numeric"){
        #Si la covariable es numérica hacemos dos plots usando como punto de corte la mediana
        dd1 <- dd2 <- data.frame()
        
        for(i in Rvalues$tmat[as.numeric(input$start_state),]){
          if(!is.na(i)){
            
            #Individuos con valor menor o igual a la mediana: ------------------------------------
            try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1,
                                    subset(Rvalues$datoslong, trans == i &
                                             Rvalues$datoslong[,input$selec_covariate] <= median(Rvalues$data[,input$selec_covariate],
                                                                                                 na.rm = T)), degree = 3)
            
            dd1 <- rbind(dd1,
                         data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard))},silent = TRUE)
            
            #Individuos con valor mayor a la mediana: ------------------------------------
            try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1,
                                    subset(Rvalues$datoslong, trans == i &
                                             Rvalues$datoslong[,input$selec_covariate] > median(Rvalues$data[,input$selec_covariate],
                                                                                                na.rm = T)), degree = 3)
            
            dd2 <- rbind(dd2,
                         data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard))},silent = TRUE)}}
        
        dd1_y <- subset(dd1, dd1$time<Rvalues$follow_up_time)
        dd2_y <- subset(dd2, dd2$time<Rvalues$follow_up_time)
        ymax <- max(c(dd1_y$hazard,dd2_y$hazard),na.rm=TRUE)
        
        ## Titles of the graphics
        a <- list(
          text = paste(input$selec_covariate,"\u2264",median(Rvalues$data[,input$selec_covariate],na.rm = T)),
          xref = "paper",
          yref = "paper",
          yanchor = "bottom",
          xanchor = "center",
          align = "center",
          x = 0.5,
          y = 1,
          showarrow = FALSE
        )
        b <- a
        b$text <- paste(input$selec_covariate,">",median(Rvalues$data[,input$selec_covariate],na.rm = T))
        
        gg1 <- ggplotly(ggplot(dd1, aes(x=time,y=hazard,color=Transition)) +
          geom_line(size = 1.2) +
          xlim(0,Rvalues$follow_up_time) +
          labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
          scale_color_manual("Legend", values = dd.col) +
          ylim(0,ymax) +
          common_theme) %>% 
          layout(annotations = a)
        
        gg2 <- ggplotly(ggplot(dd2, aes(x=time,y=hazard,color=Transition)) +
          geom_line(size = 1.2) +
          xlim(0,Rvalues$follow_up_time) +
          labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
          scale_color_manual("Legend", values = dd.col) +
          ylim(0,ymax) +
          common_theme) %>% 
           layout(annotations = b)
        
        plot <- subplot(gg1, gg2) 
        
        #Para que no se duplique la leyenda
        #Mira que nombres estan duplicados y elimina el duplicado
        #https://stackoverflow.com/questions/59613787/grouping-elements-in-legend-fails-with-ggplotly
        f_name <- function(x) x$name
        sel_dup <- which(duplicated(sapply(plot$x$data,f_name)))
        for (i in sel_dup){
          plot$x$data[[i]]$showlegend <- FALSE
        }
        
      }else if(Rvalues$covar_type[which(Rvalues$all_covar == input$selec_covariate)]=="factor"){
        #Si la covariable es categórica hacemos un plot para cada categoría
        myplots <- list()
        dd <- data.frame()
        for(j in 1:length(unique(na.omit(Rvalues$data[,input$selec_covariate])))){
          for(i in Rvalues$tmat[as.numeric(input$start_state),]){
            if(!is.na(i))(
              try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1,
                                      subset(Rvalues$datoslong, trans == i &
                                               as.data.frame(Rvalues$datoslong)[,input$selec_covariate] == unique(na.omit(Rvalues$data[,input$selec_covariate]))[j]),
                                      degree = 3)
              
              dd <- rbind(dd,
                          data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard, categ=j))},silent = TRUE))}}
        
        
        dd_y <- subset(dd, dd$time<Rvalues$follow_up_time)
        ymax <- max(dd_y$hazard,na.rm=TRUE)
        
        for(j in 1:length(unique(na.omit(Rvalues$data[,input$selec_covariate])))){
          
          b <- list(
            text = unique(na.omit(Rvalues$data[,input$selec_covariate]))[j],
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
          )
          
          gg <- ggplotly(ggplot(subset(dd, dd$categ==j), aes(x=time,y=hazard,color=Transition)) +
            geom_line(size = 1.2) +
            xlim(0,Rvalues$follow_up_time) +
            labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
            scale_color_manual("Legend", values = dd.col) +
            ylim(0,ymax) +
            common_theme) %>% 
            layout(annotations = b)

          #Guardamos el plot de cada categoría en esta lista
          myplots[[j]] <- gg
        }
        
        plot <- subplot(plotlist = myplots, titleY = TRUE, titleX = TRUE)
        
        f_name <- function(x) x$name
        sel_dup <- which(duplicated(sapply(plot$x$data,f_name)))
        for (i in sel_dup){
          plot$x$data[[i]]$showlegend <- FALSE
        }
      }
      plot
    }}
})


#Repetimos la misma idea pero en función del estado de llegada

#Seleccionamos el estado de llegada de interés
#(se plotarán las transiciones que lleguen a este estado)
output$end_state <- renderUI({
  radioButtons("end_state", "Choose the ending state of interest:",
               choiceNames = subset(Rvalues$states, !(Rvalues$states %in% Rvalues$initial_not_transient_states) & !(Rvalues$states %in% Rvalues$out_states)),
               choiceValues = which(!(Rvalues$states %in% Rvalues$initial_not_transient_states) & !(Rvalues$states %in% Rvalues$out_states)))
})



#El gráfico decidiendo el estado final
output$plot_insthaz_end <- renderPlotly({
  if(!is.null(input$end_state)){
    
    #Paleta de colores
    dd.col <- cols25(max(Rvalues$tmat, na.rm = T))
    names(dd.col)  <- Rvalues$names_tmat
    
    if(input$selec_covariate == "None"){
      dd <- data.frame()
      for(i in Rvalues$tmat[,as.numeric(input$end_state)]){
        if(!is.na(i))(
          try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1, subset(Rvalues$datoslong, trans == i), degree = 3)
          
          dd <- rbind(dd,
                      data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard))},silent = TRUE))}
      
      dd_y <- subset(dd, dd$time<Rvalues$follow_up_time)
      ymax <- max(dd_y$hazard,na.rm=TRUE)
      
      ggplotly(ggplot(dd, aes(x=time,y=hazard,color=Transition)) +
        geom_line(size = 1.2) +
        xlim(0,Rvalues$follow_up_time) +
        labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
        scale_color_manual("Legend", values = dd.col) +
        ylim(0,ymax) +
        common_theme)
      
      
      
    }else{
      #Tomamos las covariables como variables estratificadoras
      #Cuando hay más de una covariable hace dos plots para cada una de ellas
      
      if(Rvalues$covar_type[which(Rvalues$all_covar == input$selec_covariate)]=="numeric"){
        #Si la covariable es numérica usamos como punto de corte la mediana
        dd1 <- dd2 <- data.frame()
        for(i in Rvalues$tmat[,as.numeric(input$end_state)]){
          if(!is.na(i)){
            try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1,
                                    subset(Rvalues$datoslong, trans == i &
                                             Rvalues$datoslong[,input$selec_covariate] <= median(Rvalues$data[,input$selec_covariate],
                                                                                                 na.rm = T)), degree = 3)
            
            dd1 <- rbind(dd1,
                         data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard))},silent = TRUE)
            
            try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1,
                                    subset(Rvalues$datoslong, trans == i &
                                             Rvalues$datoslong[,input$selec_covariate] > median(Rvalues$data[,input$selec_covariate],
                                                                                                na.rm = T)), degree = 3)
            
            dd2 <- rbind(dd2,
                         data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard))},silent = TRUE)}}
        
        dd1_y <- subset(dd1, dd1$time<Rvalues$follow_up_time)
        dd2_y <- subset(dd2, dd2$time<Rvalues$follow_up_time)
        ymax <- max(c(dd1_y$hazard,dd2_y$hazard),na.rm=TRUE)
        
        ##-- Graphic titles
        a <- list(
          text = paste(input$selec_covariate,"\u2264",median(Rvalues$data[,input$selec_covariate],na.rm = T)),
          xref = "paper",
          yref = "paper",
          yanchor = "bottom",
          xanchor = "center",
          align = "center",
          x = 0.5,
          y = 1,
          showarrow = FALSE
        )
        b <- a
        b$text <- paste(input$selec_covariate,">",median(Rvalues$data[,input$selec_covariate],na.rm = T))
      
        gg1 <- ggplotly(ggplot(dd1, aes(x=time,y=hazard,color=Transition)) +
                          geom_line(size = 1.2) +
                          xlim(0,Rvalues$follow_up_time) +
                          labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
                          scale_color_manual("Legend", values = dd.col) +
                          ylim(0,ymax) +
                          common_theme) %>% 
          layout(annotations = a)
        
        gg2 <- ggplotly(ggplot(dd2, aes(x=time,y=hazard,color=Transition)) +
                          geom_line(size = 1.2) +
                          xlim(0,Rvalues$follow_up_time) +
                          labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
                          scale_color_manual("Legend", values = dd.col) +
                          ylim(0,ymax) +
                          common_theme) %>% 
          layout(annotations = b)
        
        plot <- subplot(gg1, gg2) 
        
        #Para que no se duplique la leyenda
        #Mira que nombres estan duplicados y elimina el duplicado
        #https://stackoverflow.com/questions/59613787/grouping-elements-in-legend-fails-with-ggplotly
        f_name <- function(x) x$name
        sel_dup <- which(duplicated(sapply(plot$x$data,f_name)))
        for (i in sel_dup){
          plot$x$data[[i]]$showlegend <- FALSE
        }
        
      }else if(Rvalues$covar_type[which(Rvalues$all_covar == input$selec_covariate)]=="factor"){
        #Si la covariable es categórica hacemos un plot para cada categoría
        myplots <- list()
        dd <- data.frame()
        for(j in 1:length(unique(na.omit(Rvalues$data[,input$selec_covariate])))){
          for(i in Rvalues$tmat[,as.numeric(input$end_state)]){
            if(!is.na(i))(
              try({bs_fit <- bshazard(Surv(Tstart, Tstop, status)~1,
                                      subset(Rvalues$datoslong, trans == i &
                                               as.data.frame(Rvalues$datoslong)[,input$selec_covariate] == unique(na.omit(Rvalues$data[,input$selec_covariate]))[j]),
                                      degree = 3)
              
              dd <- rbind(dd,
                          data.frame(Transition=Rvalues$names_tmat[i], time=bs_fit$time, hazard=bs_fit$hazard, categ=j))},silent = TRUE))}}
        
        
        dd_y <- subset(dd, dd$time<Rvalues$follow_up_time)
        ymax <- max(dd_y$hazard,na.rm=TRUE)
        
        for(j in 1:length(unique(na.omit(Rvalues$data[,input$selec_covariate])))){
          
          b <- list(
            text = unique(na.omit(Rvalues$data[,input$selec_covariate]))[j],
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
          )
          
          gg <- ggplotly(ggplot(subset(dd, dd$categ==j), aes(x=time,y=hazard,color=Transition)) +
                           geom_line(size = 1.2) +
                           xlim(0,Rvalues$follow_up_time) +
                           labs(x = Rvalues$time_unit, y ="Instantaneous hazard") +
                           scale_color_manual("Legend", values = dd.col) +
                           ylim(0,ymax) +
                           common_theme) %>% 
            layout(annotations = b)
          
          #Guardamos el plot de cada categoría en esta lista
          myplots[[j]] <- gg
        }
        
        plot <- subplot(plotlist = myplots, titleY = TRUE, titleX = TRUE)
        
        f_name <- function(x) x$name
        sel_dup <- which(duplicated(sapply(plot$x$data,f_name)))
        for (i in sel_dup){
          plot$x$data[[i]]$showlegend <- FALSE
        }
      }
      plot
    } 
  }
})


