#------------
# Forest plot
#------------

#----------------------------------
# Select the transition of interest
#----------------------------------
output$trans <- renderUI({
  selectInput("trans", "Choose the transition of interest:",
              choices = setNames(c(1:max(Rvalues$tmat, na.rm = TRUE)),  Rvalues$names_tmat),
              selected =  c(1))
})


#----------------------------------------------
# Select the units for the numerical covariates
#----------------------------------------------
output$num_unit <- renderUI({
  
  unit <- list()
  for(i in 1:length(Rvalues$all_covar)){
    if(Rvalues$covar_type[i]=="numeric"){
      unit <- list(unit, numericInput(paste0("unit_",Rvalues$all_covar[i]), Rvalues$all_covar[i], 
                                      value = 1, min = 1, max = 100))  
    }}
  unit
})


#--------------
# Make the plot
#--------------
output$forest_plot <- renderPlotly({
  if(!is.null(input$trans)){
    if(input$select_model == "Cox" && length(Rvalues$list_covar_trans[[as.numeric(input$trans)]])>0){
      #Creo un data frame con los valores del HR y su CI para hacer el forest plot
      data_forest <- as.data.frame(summary(Rvalues$mod)$conf.int[,c(1,3,4)])
      names(data_forest) <- c("HR", "lower", "upper")
      
      #Nos quedamos solo con las filas relacionadas con la transición seleccionada
      eliminar <- c()
      trans_int <- paste0("[.]", input$trans, "$")
      
      for(i in 1:dim(data_forest)[1]){
        if(!str_detect(rownames(data_forest)[i], trans_int)){
          eliminar <- c(eliminar, i)
        }}
      
      data_forest2=data_forest[-eliminar,]
      
      #Modifico los HR y el CI de las covariables numéricas en función de las unidades seleccionadas
      for(i in 1:dim(data_forest2)[1]){
        for(j in 1:length(Rvalues$list_covar_trans[[as.numeric(input$trans)]])){
          #Miro si el nombre de la fila i contiene la covariable  que estamos analizando
          if(str_detect(rownames(data_forest2)[i], Rvalues$list_covar_trans[[as.numeric(input$trans)]][j])){
            if(Rvalues$covar_type[which(Rvalues$all_covar == Rvalues$list_covar_trans[[as.numeric(input$trans)]][j])]=="numeric"){
              #Si la contiene, modifico el HR y el CI elevandolo a las unidades seleccionadas
              unidades <- paste0("unit_",Rvalues$list_covar_trans[[as.numeric(input$trans)]][j])
              
              data_forest2$HR[i]    <- data_forest2$HR[i]^input[[unidades]]
              data_forest2$lower[i] <- data_forest2$lower[i]^input[[unidades]]
              data_forest2$upper[i] <- data_forest2$upper[i]^input[[unidades]]
              rownames(data_forest2)[i] <- paste0(Rvalues$list_covar_trans[[as.numeric(input$trans)]][j], "\n (for ", input[[unidades]], " unit increase)")
              
            }else{
              if(length(levels(Rvalues$data[,Rvalues$list_covar_trans[[as.numeric(input$trans)]][j]]))<3){
                rownames(data_forest2)[i] <- paste0(Rvalues$list_covar_trans[[as.numeric(input$trans)]][j], "\n (",
                                                    levels(Rvalues$data[,Rvalues$list_covar_trans[[as.numeric(input$trans)]][j]])[2],
                                                    " respect to ", levels(Rvalues$data[,Rvalues$list_covar_trans[[as.numeric(input$trans)]][j]])[1], ")")
              }else{
                for(k in 1:(length(levels(Rvalues$data[,Rvalues$list_covar_trans[[as.numeric(input$trans)]][j]]))-1)){
                  categ <- paste0(k,"[.]")
                  if(str_detect(rownames(data_forest2)[i], categ)){
                    rownames(data_forest2)[i] <- paste0(Rvalues$list_covar_trans[[as.numeric(input$trans)]][j], "\n (", 
                                                        levels(Rvalues$data[,Rvalues$list_covar_trans[[as.numeric(input$trans)]][j]])[k+1],
                                                        " respect to ", levels(Rvalues$data[,Rvalues$list_covar_trans[[as.numeric(input$trans)]][j]])[1], ")")
                  }
                }
              }
            }
          }
        }
      }
      
      #Hacemos el forest plot
      ggplotly(ggplot(data_forest2, aes(y = rownames(data_forest2), x = HR)) +
                 geom_point(shape = 18, size = 5) +  
                 geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25) +
                 geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
                 labs(x = "Hazard Ratio (95% CI)", y = "") + 
                 scale_x_log10() +
                 ggtitle(Rvalues$names_tmat[as.numeric(input$trans)]) +
                 common_theme)
    }else{
      shinyalert("Oops!", HTML("Warning: there are no selected covariates for this transition"), type = "warning",html=TRUE)
      plotly_empty()
    }
  }
})


