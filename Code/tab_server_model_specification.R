#----------------------------
#Define the transition matrix
#----------------------------

output$from_select <- renderUI({
  selectInput("from_select", "From:", Rvalues$states)
})

output$to_select <- renderUI({
  selectInput("to_select", "To:", Rvalues$states)
})

#Mediante esta función vamos actualizando la matriz de transiciones
#Cuando la matriz de transiciones es nula pone un 1 en la nueva transición 
#Cuando la matriz de transiciones no es nula pone el siguiente número
trans_mat <- function(state_from, state_to, tmat) {
  tmat_pre <- tmat
  tmat[state_from, state_to] <- ifelse(is.na(tmat[state_from, state_to]),
                                       ifelse(max(tmat, na.rm = T) == -Inf, 1, max(tmat, na.rm = T)+1),
                                       tmat[state_from, state_to])
  
  if(is.null(find_loops(tmat, max_num_loops = 2)$loop[[1]])==TRUE){
    return(tmat)
  }else{
    shinyalert("Oops!", HTML("Warning: there is a loop. Bidirectional transitions 
                             and loops are not allowed, so the last transition is
                             not added."), type = "warning",html=TRUE) 
    return(tmat_pre)
  }
  
}


observeEvent(input$add,{
  #Actualizamos la matriz tmat y el vector names_tmat (tiene los nombres de las
  #transiciones en format a -> b)
  #Para actualizar tmat usamos la función que hemos definido antes teniendo en 
  #cuenta los estados seleccionados por el usuario y tmat
  if(is.na(Rvalues$tmat[input$from_select, input$to_select])){
    Rvalues$tmat <- trans_mat(input$from_select, input$to_select, Rvalues$tmat)
    Rvalues$names_tmat <- c(Rvalues$names_tmat, paste0(input$from_select, " -> ", input$to_select))
    
    #Creamos un data frame indicando las transiciones de una forma adecuada para
    #hacer el diagrama
    Rvalues$transit <- as.data.frame(which(!is.na(Rvalues$tmat), arr.ind = T))
    names(Rvalues$transit) = c("from", "to")
    
    #Actualizamos los estados iniciales, transitorios y absorbentes del modelo
    #Esta actualización la tenemos que hacer cada vez que añadimos/eliminamos
    #una transición y clasificamos los estados en función de tmat
    for(i in 1:length(Rvalues$states)){
      #Si una fila es todo NA ese estado es absorbente
      if(all(is.na(Rvalues$tmat[i,]))){
        Rvalues$absorbing_states <- unique(c(Rvalues$absorbing_states, Rvalues$states[i]))
        Rvalues$out_states <- if(Rvalues$states[i] %in% Rvalues$out_states){
          Rvalues$out_states[-which(Rvalues$out_states == Rvalues$states[i])]
        }else{Rvalues$out_states}
        if(all(is.na(Rvalues$tmat[,i]))){
          Rvalues$out_states <- unique(c(Rvalues$out_states, Rvalues$states[i]))
        }
      }else{
        #Si la fila y la columna de un estado tienen algún valor diferente de NA
        #este estado será transitorio
        if(any(!is.na(Rvalues$tmat[,i]))){
          Rvalues$transient_states <- unique(c(Rvalues$transient_states, Rvalues$states[i]))}
        #Al clasificar un estado como transitorio miramos si de antes era absorbente o out
        #y si lo era lo eliminamos de la lista de estados absorbentes o outs
        Rvalues$absorbing_states <- if(Rvalues$states[i] %in% Rvalues$absorbing_states){
          Rvalues$absorbing_states[-which(Rvalues$absorbing_states == Rvalues$states[i])]
        }else{Rvalues$absorbing_states}
        Rvalues$out_states <- if(Rvalues$states[i] %in% Rvalues$out_states){
          Rvalues$out_states[-which(Rvalues$out_states == Rvalues$states[i])]
        }else{Rvalues$out_states}
      }
    }
    
    #Basándonos en la lista de estados iniciales y transitorios hacemos una lista 
    #de los estados iniciales que no son transitorios y otra con los estados
    #iniciales y transitorios
    Rvalues$initial_not_transient_states <- Rvalues$initial_states[!(Rvalues$initial_states %in% Rvalues$transient_states)]
    Rvalues$initial_or_transient_states <- unique(c(Rvalues$initial_states, Rvalues$transient_states))
    
    Rvalues$colores[Rvalues$states %in% Rvalues$transient_states] <- 2
    Rvalues$colores[Rvalues$states %in% Rvalues$initial_states]   <- 1
    Rvalues$colores[Rvalues$states %in% Rvalues$absorbing_states] <- 3
    Rvalues$colores[Rvalues$states %in% Rvalues$out_states]       <- 4
    
  }else{
    shinyalert("Oops!", HTML("This transition already exists."), type = "warning",html=TRUE)
  }
})


observeEvent(input$delete,{
  #Eliminamos de tmat la transición seleccionada (ponemos NA en su posición)
  #Para que el número de transiciones sea igual al valor máximo de tmat
  #cambiamos la numeración de las siguientes transiciones
  #Eliminamos el nombre de esta transición del vector names_tmat
  if(!is.na(Rvalues$tmat[input$from_select, input$to_select])){
    k <- Rvalues$tmat[input$from_select, input$to_select]
    for (i in k:max(Rvalues$tmat, na.rm = T)) {
      Rvalues$tmat[which(Rvalues$tmat == i+1)] <- i
    }
    Rvalues$tmat[input$from_select, input$to_select] <- NA
    Rvalues$names_tmat <- Rvalues$names_tmat[-which(Rvalues$names_tmat == paste0(input$from_select, " -> ", input$to_select))]
  }else{
    shinyalert("Oops!", HTML("Warning: that transition does not exist."),
               type = "warning",html=TRUE)
  }
  
  #Creamos un data frame indicando las transiciones de una forma adecuada para
  #hacer el diagrama
  Rvalues$transit <- as.data.frame(which(!is.na(Rvalues$tmat), arr.ind = T))
  names(Rvalues$transit) = c("from", "to")
  
  #Actualizamos los estados iniciales, transitorios y absorbentes del modelo
  #Esta actualización la tenemos que hacer cada vez que añadimos/eliminamos
  #una transición y clasificamos los estados en función de tmat
  for(i in 1:length(Rvalues$states)){
    #Si una fila es todo NA ese estado es absorbente
    #Si la fila y la columna es todo NA ese estado es out
    if(all(is.na(Rvalues$tmat[i,]))){
      if(all(is.na(Rvalues$tmat[,i]))){
        Rvalues$out_states <- unique(c(Rvalues$out_states, Rvalues$states[i]))
      }else{
        Rvalues$absorbing_states <- unique(c(Rvalues$absorbing_states, Rvalues$states[i]))
      }
      
      
      #Al clasificar un estado como absorbente miramos si de antes era transitorio
      #y si lo era lo eliminamos de la lista de estados transitorio
      Rvalues$transient_states <- if(Rvalues$states[i] %in% Rvalues$transient_states){
        Rvalues$transient_states[-which(Rvalues$transient_states == Rvalues$states[i])]
      }else{Rvalues$transient_states}
      
    }else{
      #Si la fila y la columna de un estado tienen algún valor diferente de NA
      #este estado será transitorio
      if(any(!is.na(Rvalues$tmat[,i]))){
        Rvalues$transient_states <- unique(c(Rvalues$transient_states, Rvalues$states[i]))
      }
    }
  }
  #Basándonos en la lista de estados iniciales y transitorios hacemos una lista 
  #de los estados iniciales que no son transitorios y otra con los estados
  #iniciales y transitorios
  Rvalues$initial_not_transient_states <- Rvalues$initial_states[!(Rvalues$initial_states %in% Rvalues$transient_states)]
  Rvalues$initial_or_transient_states <- unique(c(Rvalues$initial_states, Rvalues$transient_states))
  
  Rvalues$colores[Rvalues$states %in% Rvalues$transient_states] <- 2
  Rvalues$colores[Rvalues$states %in% Rvalues$initial_states]   <- 1
  Rvalues$colores[Rvalues$states %in% Rvalues$absorbing_states] <- 3
  Rvalues$colores[Rvalues$states %in% Rvalues$out_states]       <- 4
  

})


#----------------------------
#Create the diagram of the model
#----------------------------

output$diagram <- renderGrViz({
  only_nodes <- create_graph() %>% add_nodes_from_table(table = Rvalues$nodes,label_col = names)
  
  if(length(Rvalues$names_tmat)>0 & nrow(Rvalues$transit)>0){
    col <- only_nodes %>%
      add_edges_from_table(
        table = Rvalues$transit,
        from_col = from,
        to_col = to,
        from_to_map = id_external
      )  %>%
      set_node_attrs(
        node_attr = weight,
        values = Rvalues$colores
      )  %>%
      colorize_node_attrs(
        node_attr_from = weight,
        node_attr_to = fillcolor,
        cut_points = c(0,1.5,2.5,3.5,5),
        palette = c("#FF9900", "#4DB3E6" , "#AA4371", "#66A61E"))
    if(is.null(input$filein)){
      graph <- col %>%
        set_node_position(node = 1, x = 0.5, y = 2.5) %>%
        set_node_position(node = 2, x = 0.5, y = 1.5) %>%
        set_node_position(node = 3, x = 2.5, y = 2  ) %>%
        set_node_position(node = 4, x = 2  , y = 3  ) %>%
        set_node_position(node = 5, x = 2  , y = 1  ) %>%
        set_node_position(node = 6, x = 4  , y = 1  ) %>%
        set_node_position(node = 7, x = 4  , y = 3  )
    }else if(!all(is.na(Rvalues$tmat))){
      graph <- col
    }
  }else{
    graph <- only_nodes
  }
  
  render_graph(graph)
  
})



#-------------------------------------------
#Selection of the covariates per transitions
#-------------------------------------------
output$myTable = renderDataTable({
  
  if(all(is.na(Rvalues$tmat))){
    
    shinyalert("Oops!", HTML("Warning: you need to define transitions."),
               type = "warning",html=TRUE)
    
    checkboxes =  as.data.frame(matrix(rep(NA,length(Rvalues$all_covar)), ncol=length(Rvalues$all_covar)))
    colnames(checkboxes) <- Rvalues$all_covar
    rownames(checkboxes) <- c("all")
    
    checkboxes
  }else{
    #The proxy to update the DT
    
    proxy <- dataTableProxy('myTable')
    
    
    checkboxes =  as.data.frame(matrix(rep(NA,length(Rvalues$all_covar)*(max(Rvalues$tmat, na.rm=TRUE)+1)), ncol=length(Rvalues$all_covar)))
    colnames(checkboxes) <- Rvalues$all_covar
    rownames(checkboxes) <- c(Rvalues$names_tmat, "all")
    
    #The reactive version of the data
    tableData = reactiveValues(checkboxes = checkboxes)
    
    #Update the table when clicked
    observeEvent(req(input$myTable_cells_selected), {
      tableData$checkboxes[input$myTable_cells_selected] =
        ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
               c(as.character(icon("ok", lib = "glyphicon"))), NA)
      
      #Al clickar en all que se seleccione la covariable en todas las transiciones
      #Ponemos NA en all para que sea mas sencillo
      if(input$myTable_cells_selected[1]==max(Rvalues$tmat, na.rm=TRUE)+1){
        tableData$checkboxes[,input$myTable_cells_selected[2]] =
          c(as.character(icon("ok", lib = "glyphicon")))
        tableData$checkboxes[input$myTable_cells_selected] = NA
      }
      
      #Send proxy (no need to refresh whole table)
      replaceData(proxy, tableData$checkboxes)
      
      if(Rvalues$covar_type[input$myTable_cells_selected[2]]=="factor" && 
         length(levels(Rvalues$data[,Rvalues$all_covar[input$myTable_cells_selected[2]]]))>2){
        
        for(k in 1:(length(levels(Rvalues$data[,Rvalues$all_covar[input$myTable_cells_selected[2]]]))-1)){
          if(input$myTable_cells_selected[1]==max(Rvalues$tmat, na.rm=TRUE)+1){
            for(i in 1:max(Rvalues$tmat, na.rm=TRUE)){
              cov_trans <- paste0(Rvalues$all_covar[input$myTable_cells_selected[2]],k,".",i)
              if(cov_trans %in% Rvalues$form1){
                Rvalues$form1
                Rvalues$sel_cov
              }else{
                Rvalues$form1 <- c(Rvalues$form1, cov_trans)
                Rvalues$sel_cov <- c(Rvalues$sel_cov, Rvalues$all_covar[input$myTable_cells_selected[2]])}
            }
          }else{
            cov_trans <- paste0(Rvalues$all_covar[input$myTable_cells_selected[2]],k,".",input$myTable_cells_selected[1])
            if(cov_trans %in% Rvalues$form1){
              Rvalues$form1 <- Rvalues$form1[-which(Rvalues$form1 == cov_trans)]
              Rvalues$sel_cov <- Rvalues$sel_cov[-which(Rvalues$sel_cov==Rvalues$all_covar[input$myTable_cells_selected[2]])[1]]
            }else{
              Rvalues$form1 <- c(Rvalues$form1, cov_trans)
              Rvalues$sel_cov <- c(Rvalues$sel_cov, Rvalues$all_covar[input$myTable_cells_selected[2]])}
          }}
      }else{
        if(input$myTable_cells_selected[1]==max(Rvalues$tmat, na.rm=TRUE)+1){
          for(i in 1:max(Rvalues$tmat, na.rm=TRUE)){
            cov_trans <- paste0(Rvalues$all_covar[input$myTable_cells_selected[2]],".",i)
            if(cov_trans %in% Rvalues$form1){
              Rvalues$form1
              Rvalues$sel_cov
            }else{
              Rvalues$form1 <- c(Rvalues$form1, cov_trans)
              Rvalues$sel_cov <- c(Rvalues$sel_cov, Rvalues$all_covar[input$myTable_cells_selected[2]])}
          }
        }else{    
          cov_trans <- paste0(Rvalues$all_covar[input$myTable_cells_selected[2]],".",input$myTable_cells_selected[1])
          if(cov_trans %in% Rvalues$form1){
            Rvalues$form1 <- Rvalues$form1[-which(Rvalues$form1 == cov_trans)]
            Rvalues$sel_cov <- Rvalues$sel_cov[-which(Rvalues$sel_cov==Rvalues$all_covar[input$myTable_cells_selected[2]])[1]]
          }else{
            Rvalues$form1 <- c(Rvalues$form1, cov_trans)
            Rvalues$sel_cov <- c(Rvalues$sel_cov, Rvalues$all_covar[input$myTable_cells_selected[2]])}
        }}
      
    })
    
    
    #The "checkbox" table
    
    checkboxes
    
    
  }}
  ,
  #These are options to make the table look like checkboxes
  selection = list(mode = "single", target = 'cell'),
  extensions = 'FixedHeader',
  options = list(
    pageLength = 100,
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    ordering = F, dom = "t",
    fixedHeader = TRUE
  ),
  escape = F)
    
  




#-----------------------------------------------------
#Prepare data in long format to fit a multistate model
#-----------------------------------------------------
observeEvent(input$save_model, {
  if(length(Rvalues$form1)>0){
    Rvalues$list_covar_trans <-  vector(mode = "list", length = max(Rvalues$tmat, na.rm=T))
    for(i in 1:max(Rvalues$tmat, na.rm=T)){
      cov <- c()
      
      for(j in 1:length(Rvalues$form1)){
        if(str_detect(Rvalues$form1[j], paste0("\\.",i, "$"))){
          
          #Nos quedamos con lo que esta delante del punto
          new_cov <- substr(Rvalues$form1[j],1,gregexpr(pattern ='.',Rvalues$form1[j],fixed = TRUE)[[1]][1]-1)
          #Tenemos que hacer el str_detect ya que si no con las variables categoricas no dicotomicas tenemos probelmas
          #Se quedaria con wave1, wave2... no con wave y luego no funciona
          for(k in 1:length(Rvalues$all_covar)){if(str_detect(new_cov, Rvalues$all_covar[k])) new_cov <- Rvalues$all_covar[k]}
          cov <- c(cov, new_cov)
        
          # cov <- c(cov, substr(Rvalues$form1[j],1,gregexpr(pattern ='.',Rvalues$form1[j],fixed = TRUE)[[1]][1]-1))

        }
      }
      Rvalues$list_covar_trans[[i]] <- cov
     
    }
    
    Rvalues$sel_cov_nodup <- Rvalues$sel_cov[!duplicated(Rvalues$sel_cov)]
  }
  
  Rvalues$datoslong <- if(any(!is.na(Rvalues$tmat))){
      datoslong1 <- msprep(Rvalues$all_time, Rvalues$all_status, Rvalues$data, Rvalues$tmat,
                           start = list(state = sapply(Rvalues$data$inistat,
                                                       search_rowname,
                                                       tmat=Rvalues$tmat,
                                                       simplify = TRUE),
                                        time = rep(0, nrow(Rvalues$data))),
                           keep = Rvalues$all_covar)
      expand.covs(datoslong1, Rvalues$all_covar, longnames = FALSE)
  }else{data.frame()}
})



#----------------------------------
#Table number events per transition
#----------------------------------
output$num_events <- DT::renderDataTable({
  if(ncol(Rvalues$datoslong)==0){
    NULL
  }else{
    matrix(events(Rvalues$datoslong)$Frequencies[,1:Rvalues$num_states],
           ncol=Rvalues$num_states, dimnames = list(Rvalues$states, Rvalues$states))}
}, options = list(dom = 't',  ordering = FALSE))



#---------------------------------------
#Selection follow-up time for the graphs
#---------------------------------------
output$follow_up_time <- renderUI({numericInput("follow_up_time", "Follow-up time:", 30)})

observeEvent(input$follow_up_time, Rvalues$follow_up_time <- input$follow_up_time)

output$time_unit <- renderUI({awesomeRadio("time_unit", "Choose the time unit:",
                                           choices = c("Hours","Days", "Weeks", "Months", "Years"),
                                           selected =  c("Days"), inline = TRUE, status = "warning")
})

observeEvent(input$time_unit, Rvalues$time_unit <- input$time_unit)


