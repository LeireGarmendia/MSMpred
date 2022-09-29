#--------------------------------------
#Select the profile of the new patients
#--------------------------------------

output$initial_state_1 <- renderUI({
  radioGroupButtons("initial_state_1", "Choose the initial state of the new patient:", 
                    choiceNames = Rvalues$initial_or_transient_states, 
                    choiceValues = 1:length(Rvalues$initial_or_transient_states), selected =  c(1),
                    status = "primary", justified=TRUE)
})

output$initial_state_2 <- renderUI({
  radioGroupButtons("initial_state_2", "Choose the initial state of the new patient:", 
                    choiceNames = Rvalues$initial_or_transient_states, 
                    choiceValues = 1:length(Rvalues$initial_or_transient_states), selected =  c(1),
                    status = "primary", justified=TRUE)
})

output$pred_1 <- renderUI({
  print(Rvalues$sel_cov_nodup)
  if(length(Rvalues$sel_cov_nodup)>0){
    pred_out <- list()
    for(i in 1:length(Rvalues$sel_cov_nodup)){
      if(Rvalues$covar_type[which(Rvalues$all_covar==Rvalues$sel_cov_nodup[i])]=="numeric"){
        
        pred_out <- list(pred_out, numericInput(paste0("pred_1_",Rvalues$sel_cov_nodup[i]), Rvalues$sel_cov_nodup[i], 
                                                quantile(Rvalues$data[,Rvalues$sel_cov_nodup[i]], 0.5, na.rm = T, type = 3),
                                                min = min(Rvalues$data[,Rvalues$sel_cov_nodup[i]], na.rm = T), 
                                                max = max(Rvalues$data[,Rvalues$sel_cov_nodup[i]], na.rm = T), width = "100%"))
      } else if(Rvalues$covar_type[which(Rvalues$all_covar==Rvalues$sel_cov_nodup[i])]=="factor"){
        pred_out <- list(pred_out, radioGroupButtons(inputId = paste0("pred_1_",Rvalues$sel_cov_nodup[i]), 
                                                     label = Rvalues$sel_cov_nodup[i], 
                                                     choices = levels(Rvalues$data[,Rvalues$sel_cov_nodup[i]]),
                                                     status = "primary", justified=TRUE))  
      }
    }
    pred_out
  }else{NULL}
})


output$pred_2 <- renderUI({
  if(length(Rvalues$sel_cov_nodup)>0){
    pred_out <- list()
    for(i in 1:length(Rvalues$sel_cov_nodup)){
      if(Rvalues$covar_type[which(Rvalues$all_covar==Rvalues$sel_cov_nodup[i])]=="numeric"){
        pred_out <- list(pred_out, numericInput(paste0("pred_2_",Rvalues$sel_cov_nodup[i]), Rvalues$sel_cov_nodup[i], 
                                                quantile(Rvalues$data[,Rvalues$sel_cov_nodup[i]], 0.5, na.rm = T, type = 3),
                                                min = min(Rvalues$data[,Rvalues$sel_cov_nodup[i]], na.rm = T), 
                                                max = max(Rvalues$data[,Rvalues$sel_cov_nodup[i]], na.rm = T), width = "100%"))  
        
      } else if(Rvalues$covar_type[which(Rvalues$all_covar==Rvalues$sel_cov_nodup[i])]=="factor"){
        pred_out <- list(pred_out, radioGroupButtons(inputId = paste0("pred_2_",Rvalues$sel_cov_nodup[i]), 
                                                     label = Rvalues$sel_cov_nodup[i], 
                                                     choices = levels(Rvalues$data[,Rvalues$sel_cov_nodup[i]]),
                                                     status = "primary", justified=TRUE))
      }
    }
    pred_out
  }else{NULL}
})



#------------------------------
#Select time for the prediction
#------------------------------

output$time_pred_1 <- renderUI({numericInput("time_pred_1", "Prediction time:", 30, width = "100%")})

output$time_pred_2 <- renderUI({numericInput("time_pred_2", "Prediction time:", 30, width = "100%")})


#----------------------------------
#Probability of being in each state
#----------------------------------
output$indiv_1 <- renderText({
  if(input$select_model == "Cox"){
    paste("Probability of being in each state after", input$time_pred_1, input$time_unit,
          "for a patient with initial state", Rvalues$states[as.numeric(input$initial_state_1)])
  }})



output$pred_box_1 <- renderUI({
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)==0){
      Rvalues$msf_1 <- msfit(Rvalues$mod, trans = Rvalues$tmat)
      
      Rvalues$pt_1 <- probtrans(Rvalues$msf_1, predt = 0, variance = FALSE)
      
      if(input$time_pred_1 %in%  Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time){
        Rvalues$pred_time_1 <- input$time_pred_1
      }else{
        ind <- which(Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time<input$time_pred_1)
        Rvalues$pred_time_1 <- Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_1 <- subset(Rvalues$pt_1[[as.numeric(input$initial_state_1)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_1))
    }else{
      Rvalues$predict <- "Yes"
      name_cov <- paste0("pred_1_",Rvalues$sel_cov_nodup[1])
      pat <- data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T)))
      for(i in 2:length(Rvalues$sel_cov_nodup)){
        name_cov <- paste0("pred_1_",Rvalues$sel_cov_nodup[i])
        pat <- cbind(pat,data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T))))
      }
      pat <- cbind(pat,1:max(Rvalues$tmat, na.rm = T))
      # }
      
      names(pat) <- c(Rvalues$sel_cov_nodup, "trans")
      
      for(i in 1:length(pat)){
        if(!is.numeric(pat[,i])) {
          pat[,i] <- factor(pat[,i], levels = levels(Rvalues$data[, names(pat)[i]]))
        }
      }
      
      attr(pat, "trans") <- Rvalues$tmat
      class(pat) <- c("msdata", "data.frame")
      
      pat <- expand.covs(pat, covs=Rvalues$sel_cov_nodup, longnames = FALSE)
      
      pat$strata <- pat$trans
      Rvalues$msf_1 <- msfit(Rvalues$mod, pat, trans = Rvalues$tmat)
      
      Rvalues$pt_1 <- probtrans(Rvalues$msf_1, predt = 0, variance = FALSE)
      
      #Si el tiempo seleccionado no esta en las probabilidades de transicion 
      #cogemos el ultimo tiempo anterior a este que tengamos
      if(input$time_pred_1 %in%  Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time){
        Rvalues$pred_time_1 <- input$time_pred_1
      }else{
        ind <- which(Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time<input$time_pred_1)
        Rvalues$pred_time_1 <- Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_1 <- subset(Rvalues$pt_1[[as.numeric(input$initial_state_1)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_1))
      for(i in 1:length(Rvalues$states)){
        if(Rvalues$pred_prob_1[1,i+1]>1){
          Rvalues$predict <- "No"
          shinyalert("Oops!", HTML("Warning: it is not possible to compute the probabilities 
                                     due to convergence problems."), type = "warning",html=TRUE)
          break}
      }
    }
    if(input$select_model == "Cox" & Rvalues$num_states>1 & Rvalues$predict == "Yes"){
      a <-lapply(1:Rvalues$num_states, function (statenum){
        valueBox(
          value = ifelse(100*Rvalues$pred_prob_1[1,statenum+1]<0, 
                         paste0(formatC(0.0, digits = 1, format = "f"),'%'), 
                         paste0(formatC(100*Rvalues$pred_prob_1[1,statenum+1], digits = 1, format = "f"),'%')),
          subtitle = Rvalues$states[statenum],
          color = ifelse(Rvalues$pred_prob_1[1,statenum+1] >= 0.5,'purple',ifelse(Rvalues$pred_prob_1[1,statenum+1] >= 0.3, "light-blue","aqua")))
      })
    }else{NULL}
    a
  }else{
    shinyalert("Oops!", HTML("You should fit a model previously in the Fitted model tab."), type = "warning",html=TRUE)}
})






output$indiv_2 <- renderText({
  if(input$select_model == "Cox"){
    paste("Probability of being in each state after", input$time_pred_2, input$time_unit,
          "for a patient with initial state", Rvalues$states[as.numeric(input$initial_state_2)])
  }})


output$pred_box_2 <- renderUI({
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)==0){
      Rvalues$msf_2 <- msfit(Rvalues$mod, trans = Rvalues$tmat)
      
      Rvalues$pt_2 <- probtrans(Rvalues$msf_2, predt = 0, variance = FALSE)
      
      if(input$time_pred_2 %in%  Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time){
        Rvalues$pred_time_2 <- input$time_pred_2
      }else{
        ind <- which(Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time<input$time_pred_2)
        Rvalues$pred_time_2 <- Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_2 <- subset(Rvalues$pt_2[[as.numeric(input$initial_state_2)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_2))
    }else{
        Rvalues$predict <- "Yes"
        name_cov <- paste0("pred_2_",Rvalues$sel_cov_nodup[1])
        pat <- data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T)))
        for(i in 2:length(Rvalues$sel_cov_nodup)){
          name_cov <- paste0("pred_2_",Rvalues$sel_cov_nodup[i])
          pat <- cbind(pat,data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T))))
        }
        pat <- cbind(pat,1:max(Rvalues$tmat, na.rm = T))
      
      names(pat) <- c(Rvalues$sel_cov_nodup, "trans")
      
      for(i in 1:length(pat)){
        if(!is.numeric(pat[,i])) {
          pat[,i] <- factor(pat[,i], levels = levels(Rvalues$data[, names(pat)[i]]))
        }
      }
      
      attr(pat, "trans") <- Rvalues$tmat
      class(pat) <- c("msdata", "data.frame")
      
      pat <- expand.covs(pat, covs=Rvalues$sel_cov_nodup, longnames = FALSE)
      
      pat$strata <- pat$trans
      
      Rvalues$msf_2 <- msfit(Rvalues$mod, pat, trans = Rvalues$tmat)
      
      Rvalues$pt_2 <- probtrans(Rvalues$msf_2, predt = 0, variance = FALSE)
      
      if(input$time_pred_2 %in%  Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time){
        Rvalues$pred_time_2 <- input$time_pred_2
      }else{
        ind <- which(Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time<input$time_pred_2)
        Rvalues$pred_time_2 <- Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_2 <- subset(Rvalues$pt_2[[as.numeric(input$initial_state_2)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_2))
      for(i in 1:length(Rvalues$states)){
        if(Rvalues$pred_prob_2[1,i+1]>1){
          Rvalues$predict <- "No"
          shinyalert("Oops!", HTML("Warning: it is not possible to compute the probabilities 
                                     due to convergence problems."), type = "warning",html=TRUE)
          break}
      }
    }
    
    
    
    if(input$select_model == "Cox" & Rvalues$num_states>1 & Rvalues$predict == "Yes"){
      a <-lapply(1:Rvalues$num_states, function (statenum){
        valueBox(
          value = ifelse(100*Rvalues$pred_prob_2[1,statenum+1]<0, 
                         paste0(formatC(0.0, digits = 1, format = "f"),'%'), 
                         paste0(formatC(100*Rvalues$pred_prob_2[1,statenum+1], digits = 1, format = "f"),'%')),
          subtitle = Rvalues$states[statenum],
          color = ifelse(Rvalues$pred_prob_2[1,statenum+1] >= 0.5,'purple',ifelse(Rvalues$pred_prob_2[1,statenum+1] >= 0.3, "light-blue","aqua")))
      })
    }else{NULL}
    a
    
  }else{
    shinyalert("Oops!", HTML("You should fit a model previously in the Fitted model tab."), type = "warning",html=TRUE)}
})




#---------------------------
#Transition probability plot
#---------------------------
output$stacked_pred_1 <- renderUI({
  if(input$select_model == "Cox"){
    radioButtons("stacked_pred_1", "Choose the plot type:",
                 choiceNames = c("Non-stacked plot", "Stacked plot"),
                 choiceValues = c("single", "filled"), selected =  c("filled"))
  }else{NULL}
})

output$stacked_pred_2 <- renderUI({
  if(input$select_model == "Cox"){
    radioButtons("stacked_pred_2", "Choose the plot type:",
                 choiceNames = c("Non-stacked plot", "Stacked plot"),
                 choiceValues = c("single", "filled"), selected =  c("filled"))
  }else{NULL}
})


output$plot_pred_1 <- renderPlotly({
  #Vuelvo a generar pred_prob para que se actualice el grafico teniendo la caja de las probabilidades cerrada
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)==0){
      Rvalues$msf_1 <- msfit(Rvalues$mod, trans = Rvalues$tmat)
      
      Rvalues$pt_1 <- probtrans(Rvalues$msf_1, predt = 0, variance = FALSE)
      
      if(input$time_pred_1 %in%  Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time){
        Rvalues$pred_time_1 <- input$time_pred_1
      }else{
        ind <- which(Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time<input$time_pred_1)
        Rvalues$pred_time_1 <- Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_1 <- subset(Rvalues$pt_1[[as.numeric(input$initial_state_1)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_1))
    }else{
        Rvalues$predict <- "Yes"
        name_cov <- paste0("pred_1_",Rvalues$sel_cov_nodup[1])
        pat <- data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T)))
        for(i in 2:length(Rvalues$sel_cov_nodup)){
          name_cov <- paste0("pred_1_",Rvalues$sel_cov_nodup[i])
          pat <- cbind(pat,data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T))))
        }
        pat <- cbind(pat,1:max(Rvalues$tmat, na.rm = T))
      # }
      
      names(pat) <- c(Rvalues$sel_cov_nodup, "trans")
      
      for(i in 1:length(pat)){
        if(!is.numeric(pat[,i])) {
          pat[,i] <- factor(pat[,i], levels = levels(Rvalues$data[, names(pat)[i]]))
        }
      }
      
      attr(pat, "trans") <- Rvalues$tmat
      class(pat) <- c("msdata", "data.frame")
      
      pat <- expand.covs(pat, covs=Rvalues$sel_cov_nodup, longnames = FALSE)
      
      pat$strata <- pat$trans
      Rvalues$msf_1 <- msfit(Rvalues$mod, pat, trans = Rvalues$tmat)
      
      Rvalues$pt_1 <- probtrans(Rvalues$msf_1, predt = 0, variance = FALSE)
      
      if(input$time_pred_1 %in%  Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time){
        Rvalues$pred_time_1 <- input$time_pred_1
      }else{
        ind <- which(Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time<input$time_pred_1)
        Rvalues$pred_time_1 <- Rvalues$pt_1[[as.numeric(input$initial_state_1)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_1 <- subset(Rvalues$pt_1[[as.numeric(input$initial_state_1)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_1))
      for(i in 1:length(Rvalues$states)){
        if(Rvalues$pred_prob_1[1,i+1]>1){
          Rvalues$predict <- "No"
          shinyalert("Oops!", HTML("Warning: it is not possible to compute the probabilities 
                                     due to convergence problems."), type = "warning",html=TRUE)
          break}
      }
    }
    if(Rvalues$predict == "Yes"){
      ggplotly(plot(Rvalues$pt_1, xlim = c(0, input$time_pred_1), type = input$stacked_pred_1,
           from = as.numeric(input$initial_state_1), lwd = 1, xlab = input$time_unit,
           use.ggplot = TRUE) +
        ggtitle("Transition probability plot for individual 1") +
        common_theme)
      
    }else{NULL}
  }else{
    shinyalert("Oops!", HTML("You should fit a model previously in the Fitted model tab."), type = "warning",html=TRUE)}
})



output$plot_pred_2 <- renderPlotly({
  #Vuelvo a generar pred_prob para que se actualice el grafico teniendo la caja de las probabilidades cerrada
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)==0){
      Rvalues$msf_2 <- msfit(Rvalues$mod, trans = Rvalues$tmat)
      
      Rvalues$pt_2 <- probtrans(Rvalues$msf_2, predt = 0, variance = FALSE)
      
      if(input$time_pred_2 %in%  Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time){
        Rvalues$pred_time_2 <- input$time_pred_2
      }else{
        ind <- which(Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time<input$time_pred_2)
        Rvalues$pred_time_2 <- Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_2 <- subset(Rvalues$pt_2[[as.numeric(input$initial_state_2)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_2))
    }else{
      # if(length(input$covariates)==1){
      #   name_cov <- paste0("pred_2_",input$covariates[1])
      #   pat <- data.frame(c(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T))),1:max(Rvalues$tmat, na.rm = T))
      # }else if(length(input$covariates)>1){
        Rvalues$predict <- "Yes"
        name_cov <- paste0("pred_2_",Rvalues$sel_cov_nodup[1])
        pat <- data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T)))
        for(i in 2:length(Rvalues$sel_cov_nodup)){
          name_cov <- paste0("pred_2_",Rvalues$sel_cov_nodup[i])
          pat <- cbind(pat,data.frame(rep(input[[name_cov]],max(Rvalues$tmat, na.rm = T))))
        }
        pat <- cbind(pat,1:max(Rvalues$tmat, na.rm = T))
      # }
      
      names(pat) <- c(Rvalues$sel_cov_nodup, "trans")
      
      for(i in 1:length(pat)){
        if(!is.numeric(pat[,i])) {
          pat[,i] <- factor(pat[,i], levels = levels(Rvalues$data[, names(pat)[i]]))
        }
      }
      
      attr(pat, "trans") <- Rvalues$tmat
      class(pat) <- c("msdata", "data.frame")
      
      pat <- expand.covs(pat, covs=Rvalues$sel_cov_nodup, longnames = FALSE)
      
      pat$strata <- pat$trans
      
      Rvalues$msf_2 <- msfit(Rvalues$mod, pat, trans = Rvalues$tmat)
      
      Rvalues$pt_2 <- probtrans(Rvalues$msf_2, predt = 0, variance = FALSE)
      
      if(input$time_pred_2 %in%  Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time){
        Rvalues$pred_time_2 <- input$time_pred_2
      }else{
        ind <- which(Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time<input$time_pred_2)
        Rvalues$pred_time_2 <- Rvalues$pt_2[[as.numeric(input$initial_state_2)]]$time[max(ind)]
      }
      
      Rvalues$pred_prob_2 <- subset(Rvalues$pt_2[[as.numeric(input$initial_state_2)]][,1:(length(Rvalues$states)+1)],
                                    time == as.numeric(Rvalues$pred_time_2))
      for(i in 1:length(Rvalues$states)){
        if(Rvalues$pred_prob_2[1,i+1]>1){
          Rvalues$predict <- "No"
          shinyalert("Oops!", HTML("Warning: it is not possible to compute the probabilities 
                                     due to convergence problems."), type = "warning",html=TRUE)
          break}
      }
    }
    if(Rvalues$predict == "Yes"){
      ggplotly(plot(Rvalues$pt_2, xlim = c(0, input$time_pred_2), type = input$stacked_pred_2,
           from = as.numeric(input$initial_state_2), lwd = 1, xlab = input$time_unit,
           use.ggplot = TRUE) +
        ggtitle("Transition probability plot for individual 2") +
          common_theme)
    }else{NULL}
  }else{
    shinyalert("Oops!", HTML("You should fit a model previously in the Fitted model tab."), type = "warning",html=TRUE)}
  
})
