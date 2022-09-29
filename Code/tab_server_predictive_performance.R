#-----------------------------------------
#Create the table of the logarithmic score
#-----------------------------------------
observeEvent(input$LS, {
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)>0){
      set.seed(123)
      p_train <- 0.7
      n <- nrow(Rvalues$data)
      
      train_sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob = c(1-p_train,p_train))
      datos_70 <- Rvalues$data[train_sel,]      # train
      datos_30 <- Rvalues$data[!train_sel,]     # test
      datos_30 <- datos_30[complete.cases(datos_30[,Rvalues$all_covar]),]
      
      datoslong_70 <- prepare_data(data=datos_70,tmat=Rvalues$tmat,time=Rvalues$all_time,
                                   status=Rvalues$all_status,covar=Rvalues$all_covar,var_inistat="inistat")
      datoslong_30 <- prepare_data(data=datos_30,tmat=Rvalues$tmat,time=Rvalues$all_time,
                                   status=Rvalues$all_status,covar=Rvalues$all_covar,var_inistat="inistat")
      
      LS <- 0 # Logarithmic Score
      k  <- 0
      
      ##-- Launch progress bar 
      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0, total = dim(datos_30)[1]) # reinitialize to 0 if you run the calculation several times
      session$sendCustomMessage(type = 'launch-modal', "my-modal") 
      for(i in 1:dim(datos_30)[1]){
        updateProgressBar(session = session, id = "pb", value = i, total = dim(datos_30)[1])  # Update progress bar 
        
        #Preparamos los datos para calcular las probabilidades de transición
        pat_1 <- datos_30[rep(i, max(Rvalues$tmat, na.rm = TRUE)), Rvalues$all_covar]
        pat_1$trans <- 1:max(Rvalues$tmat, na.rm = TRUE)
        attr(pat_1, "trans") <- Rvalues$tmat
        class(pat_1) <- c("msdata", "data.frame")
        pat_1 <- expand.covs(pat_1, Rvalues$all_covar, longnames = FALSE)
        pat_1$strata <- pat_1$trans
        
        msf_1 <- msfit(Rvalues$mod, pat_1, trans = Rvalues$tmat)
        
        pt_1 <- probtrans(msf_1, predt = 0, variance = FALSE)
        
        #Buscamos si el tiempo de interés está en las probabilidades de transición y si no está cogemos el anterior
        if(Rvalues$follow_up_time %in%  pt_1[[which(Rvalues$states==datos_30[i,"inistat"])]]$time){
          LS_time <- Rvalues$follow_up_time
        }else{
          ind <- which(pt_1[[which(Rvalues$states==datos_30[i,"inistat"])]]$time<Rvalues$follow_up_time)
          LS_time <- pt_1[[which(Rvalues$states==datos_30[i,"inistat"])]]$time[max(ind)]
        }
        
        #Nos quedamos con las probabilidades de transición de ese tiempo
        
        pred_prob_1 <- subset(pt_1[[which(Rvalues$states==datos_30[i,"inistat"])]][,1:(length(Rvalues$states)+1)], time == LS_time)
        
        #Buscamos el estado real
        ind <- which(datoslong_30$id==i & datoslong_30$Tstop < LS_time)
        ifelse(length(ind)==0, real_state <- which(Rvalues$states==datos_30[i,"inistat"]),{
          row <- which(datoslong_30$id==i & datoslong_30$Tstop == max(datoslong_30[ind,]$Tstop) & datoslong_30$status==1)
          real_state <- datoslong_30$to[row]})
        
        
        if(!(pred_prob_1[,1+real_state]>1 | pred_prob_1[,1+real_state]<0)){
          k  <- k+1
          LS <- LS+log(pred_prob_1[,1+real_state])
        }
        
        ifelse(i==1,
               tabla_conf <- data.frame("pred"=factor(Rvalues$states[which.max(pred_prob_1[,2:length(pred_prob_1)])],levels=Rvalues$states), 
                                        "real"=factor(Rvalues$states[real_state],levels=Rvalues$states)),
               tabla_conf[i,] <- c(Rvalues$states[which.max(pred_prob_1[,2:length(pred_prob_1)])], Rvalues$states[real_state]))
        
        
      }
      
      
      ##-- Close the progress bar ----------------------------------
      session$sendCustomMessage(type = 'remove-modal', "my-modal") 
      LS_total <- -LS/k
      
      output$logar_score <- DT::renderDataTable({
        tab <- rbind("Logarithmic score"=LS_total)
        colnames(tab) <- c("Fitted model")
        return(round(tab,3))
      }, options = list(dom = 't'))
      
      
      output$cont_tab <- DT::renderDataTable({
        tab <- rbind("pred"=table(tabla_conf))
        return(tab)
      }, options = list(dom = 't')) 
      
      #Barplot
      tab <- table(tabla_conf)
      prop_correct_guess <- data.frame(state=Rvalues$states,
                                       p=100*diag(tab)/apply(tab,2,sum))
      prop_correct_guess$p[prop_correct_guess$p==0] <- 0.01
      prop_correct_guess$p[is.na(prop_correct_guess$p)] <- 0
      
      
      gg <- ggplot(prop_correct_guess,aes(x=state,y=p)) +
        geom_bar(stat='identity', col='black', fill='darkorange2') +
        ylab('Prop. of correct guess') +
        common_theme
      
      output$conf_barplot <- renderPlotly({ggplotly(gg)})
      
    }else{
      return(NULL)
    }
  }
})