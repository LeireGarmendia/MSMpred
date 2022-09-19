#-------------------------------------------------------------------------------------
# Martingale residuals: Checking for the linear assumption of the continuos covariates
#-------------------------------------------------------------------------------------

#----------------------------------
# Select the transition of interest
#----------------------------------
output$trans_lin <- renderUI({
  selectInput("trans_lin", "Choose the transition of interest:",
              choices = setNames(c(1:max(Rvalues$tmat, na.rm = TRUE)),  Rvalues$names_tmat),
              selected =  c(1))
})

output$trans_lin_cov <- renderUI({
  if(input$select_model == "Cox" && length(Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]])>0){
    numeric_covar <- c()
    number_numeric_covar <- c()
    
    for(i in 1:length(Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]])){
      
      if(Rvalues$covar_type[which(Rvalues$all_covar == Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]][i])]=="numeric"){
        numeric_covar <- c(numeric_covar, Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]][i])
        number_numeric_covar <- c(number_numeric_covar, which(Rvalues$all_covar == Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]][i]))
      }
    }
    
    if(length(numeric_covar)>0){
      awesomeRadio("trans_lin_cov", "Choose the numerical covariate of interest:", numeric_covar, status = "warning")
    }
  }
})

output$martingalas_res <- renderPlotly({
  if(!is.null(input$trans_lin)){
    if(input$select_model == "Cox" && length(Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]])>0){
      numeric_covar <- number_numeric_covar <- c()
      for(i in 1:length(Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]])){
        if(Rvalues$covar_type[which(Rvalues$all_covar == Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]][i])]=="numeric"){
          numeric_covar        <- c(numeric_covar, Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]][i])
          number_numeric_covar <- c(number_numeric_covar, which(Rvalues$all_covar == Rvalues$list_covar_trans[[as.numeric(input$trans_lin)]][i]))
        }
      }
      if(length(numeric_covar)>0){
        #Nos quedamos con los pacientes que pueden hacer esa transicion y que tienen datos completos
        data_res <- subset(Rvalues$datoslong, trans == input$trans_lin)
        data_res <- data_res[complete.cases(data_res),]
       
        #Nos quedamos con la parte de la formula correspondiente a la transicion de interes
        eliminar <- c()
        trans_int <- paste0("[.]", input$trans_lin, "$")

        for(i in 1:length(Rvalues$form1)){
          if(!str_detect(Rvalues$form1[i], trans_int)){
            eliminar <- c(eliminar, i)}}

        form_int=Rvalues$form1[-eliminar]

        form <- as.formula(paste0('Surv(Tstart, Tstop, status) ~',
                                  paste(form_int, collapse = '+')))

        cox <- coxph(form, data=data_res)
        print(cox)
        covar_trans <- paste0(input$trans_lin_cov, ".", input$trans_lin)
        print(update(cox,  as.formula(paste0("~ . - ",covar_trans))))
        resids <- residuals(update(cox,  as.formula(paste0("~ . - ",covar_trans))))

        dd <- data.frame(res = resids, cov = data_res[rownames(data_res) %in% names(resids),covar_trans])

        ggplotly(ggplot(dd, aes(x = cov, y = res)) +
                   geom_point() +
                   geom_hline(yintercept = 0, linetype = "dashed") +
                   geom_smooth(method = "gam") +
                   labs(x = input$trans_lin_cov) +
                   ggtitle(Rvalues$names_tmat[as.numeric(input$trans_lin)]) +
                   common_theme)}
    }else{
      shinyalert("Oops!", HTML("Warning: there are no numercial covariates selected for this transition"), type = "warning",html=TRUE)
      plotly_empty()
    }
  }
})


#------------------------------------------------------------
# The dfbetas residuals: looking for influential observations
#------------------------------------------------------------

#----------------------------------
# Select the transition of interest
#----------------------------------
output$trans_influ <- renderUI({
  selectInput("trans_influ", "Choose the transition of interest:",
              choices = setNames(c(1:max(Rvalues$tmat, na.rm = TRUE)),  Rvalues$names_tmat),
              selected =  c(1))
})



output$score_res <- renderPlotly({
  if(!is.null(input$trans_influ)){
    if(input$select_model == "Cox" && length(Rvalues$list_covar_trans[[as.numeric(input$trans_influ)]])>0){
      data_res <- subset(Rvalues$datoslong, trans == input$trans_influ)
      data_res <- data_res[complete.cases(data_res),]
      # data_res <- Rvalues$datoslong[complete.cases(Rvalues$datoslong),]
      
      eliminar <- c()
      trans_int <- paste0("[.]", input$trans_influ, "$")

      for(i in 1:length(Rvalues$form1)){
        if(!str_detect(Rvalues$form1[i], trans_int)){
          eliminar <- c(eliminar, i)}}
      
      form_int=Rvalues$form1[-eliminar]
      
      form <- as.formula(paste0('Surv(Tstart, Tstop, status) ~',
                                paste(form_int, collapse = '+')))
      
      cox <- coxph(form, data=data_res)

      ggplotly(ggcoxdiagnostics(cox, type = "dfbetas",
                                title = Rvalues$names_tmat[as.numeric(input$trans_influ)],
                                ggtheme = common_theme))

    }else{
      shinyalert("Oops!", HTML("Warning: there are no selected covariates for this transition"), type = "warning",html=TRUE)
      plotly_empty()
    }
  }
})


#--------------------------------------------------------------------------
# Schoenfeld residuals: Checking for the assumption of proportional hazards
#--------------------------------------------------------------------------

#----------------------------------
# Select the transition of interest
#----------------------------------
output$trans_prop <- renderUI({
  selectInput("trans_prop", "Choose the transition of interest:",
              choices = setNames(c(1:max(Rvalues$tmat, na.rm = TRUE)),  Rvalues$names_tmat),
              selected =  c(1))
})


output$schoenfeld_res <- renderPlotly({
  if(!is.null(input$trans_prop)){
    num_trans <- paste0("trans_",input$trans_prop)
    if(input$select_model == "Cox" && length(Rvalues$list_covar_trans[[as.numeric(input$trans_prop)]])>0){
      data_res <- subset(Rvalues$datoslong, trans == input$trans_prop)
      data_res <- data_res[complete.cases(data_res),]
      # data_res <- Rvalues$datoslong[complete.cases(Rvalues$datoslong),]
      
      eliminar <- c()
      trans_int <- paste0("[.]", input$trans_prop, "$")

      
      for(i in 1:length(Rvalues$form1)){
        if(!str_detect(Rvalues$form1[i], trans_int)){
          eliminar <- c(eliminar, i)}}
      
      form_int=Rvalues$form1[-eliminar]
      
      form <- as.formula(paste0('Surv(Tstart, Tstop, status) ~',
                                paste(form_int, collapse = '+')))

      cox <- coxph(form, data=data_res)

      ggplotly(ggcoxdiagnostics(cox, type = "schoenfeld",
                                title = Rvalues$names_tmat[as.numeric(input$trans_prop)],
                                ggtheme = common_theme))
    }else{
      shinyalert("Oops!", HTML("Warning: there are no selected covariates for this transition"), type = "warning",html=TRUE)
      plotly_empty()
    }
  }
})


