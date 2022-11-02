#----------------------------
#Adjust a Markovian Cox model
#----------------------------

observeEvent({input$save_model
  input$select_model},{
    
    if(input$select_model == "Cox"){
      
      #Creamos el modelo
      
      Rvalues$mod <- if(length(Rvalues$form1)>0){
        #With covariates
        form2 <- as.formula(paste0('Surv(Tstart, Tstop, status)','~', 
                                   paste(Rvalues$form1, collapse = "+"),'+','strata(trans)'))
        
        coxph(form2, data=Rvalues$datoslong)
        
      }else{
        #Without covariates
        
        #Ajustamos el modelo nulo, pero como no tenemos coeficientes a estimar
        #decimos que no devuelva nada en esta tabla
        coxph(Surv(Tstart, Tstop, status)~strata(trans), data=Rvalues$datoslong)
      }
      
      
      #Creamos la tabla de los coeficientes
      if(length(Rvalues$form1)>0){
        #With covariates
        
        #Creamos un data frame con la info que queremos incluir en la tabla
        #Del summary cogemos coef, exp(coef), Pr(>|z|), lower.95 y upper.95, pero
        #luego lo modificaremos un poco para que quede más bonito
        tab <- as.data.frame(cbind(summary(Rvalues$mod)$coefficients[,c(1,2,5)],
                                   summary(Rvalues$mod)$conf.int[,c(3,4)]))
        
        #Creamos una nueva columna juntando exp(coef), lower.95 y upper.95 para 
        #tener en una única columna la info HR (95%CI)
        tab$HR <- paste0(ifelse(tab[,2]>max_HR,rep(Inf,nrow(tab)),formatC(tab[,2],format='f', digits=2)),' (',
                         formatC(tab[,4],format='f', digits=2),', ',
                         ifelse(tab[,5]>max_HR,rep(Inf,nrow(tab)),formatC(tab[,5],format='f', digits=2)),')')
        
        #Nos quedamos con las columnas que queremos incluir en la tabla 
        #(coef, HR (95%CI) y p-value), les cambiamos el nombre a las columnas y 
        #les cambiamos el formato
        tab <- tab[,c(1,6,3)]
        colnames(tab) <- c("coef", "HR (95%CI)", "p-value")
        tab$coef <- formatC(tab$coef,format='f', digits=3)
        tab$"p-value" <- formatC(tab$"p-value",format='f', digits=3)
        
        for(i in 1:dim(tab)[1]){
          for(k in 1:max(Rvalues$tmat, na.rm=T)){
            
            rownames(tab)[i] <- str_replace(string = rownames(tab)[i], pattern = paste0("\\.",k,"$"), 
                                            replacement = paste0(" (", Rvalues$names_tmat[k], ")"))}
          
          for(j in 1:length(Rvalues$all_covar)){
            #Para cada covariable miramos si es categorica
            if(Rvalues$covar_type[j]=="factor"){
              #Buscamos sus filas
              if(str_detect(rownames(tab)[i], Rvalues$all_covar[j])){
                if(length(levels(Rvalues$data[,Rvalues$all_covar[j]]))>2){
                  #Dentro de sus filas buscamos las que corresponden a cada comparación y
                  #sustituimos el número por la categoría que se está comparando
                  
                  for(l in 1:(length(levels(Rvalues$data[,Rvalues$all_covar[j]]))-1)){
                    if(str_detect(rownames(tab)[i], as.character(l))){
                      rownames(tab)[i] <- str_replace(rownames(tab)[i], as.character(l), 
                                                      paste0(" (", levels(Rvalues$data[,Rvalues$all_covar[j]])[l+1], 
                                                             " respect to ", levels(Rvalues$data[,Rvalues$all_covar[j]])[1], ")"))
                    }
                  }
                }else{
                  rownames(tab)[i] <- str_replace(rownames(tab)[i], Rvalues$all_covar[j],
                                                  paste0(Rvalues$all_covar[j], " (", levels(Rvalues$data[,Rvalues$all_covar[j]])[2],
                                                         " respect to ", levels(Rvalues$data[,Rvalues$all_covar[j]])[1], ")"))
                  
                }
              }
            }
            
          }
        }
        
      }else{
        tab <- as.data.frame(rbind(rep(c("-"),3)))
        colnames(tab) <- c("coef", "HR (95%CI)", "p-value")
      }
      Rvalues$tab_coef <- tab
      
      
      #Creamos el data frame para comparar modelos
      Rvalues$comp <- rbind(isolate(Rvalues$comp), data.frame(
        "model"= isolate(Rvalues$model_num),
        "covariates" = ifelse(length(Rvalues$form1)>0, length(Rvalues$form1), c("-")),
        "transitions" = length(Rvalues$names_tmat),
        "coefficients" = c(" "),
        "log likelihood" = ifelse(length(Rvalues$form1)>0, round(summary(Rvalues$mod)$loglik[2],3),round(summary(Rvalues$mod)$loglik[1],3)),
        "AIC" = round(AIC(Rvalues$mod, k=2), 3),
        "Likelihood ratio" = ifelse(length(Rvalues$form1)>0, round(summary(Rvalues$mod)$logtest[1],3), c("-")),
        "Wald" = ifelse(length(Rvalues$form1)>0, round(summary(Rvalues$mod)$waldtest[1],3), c("-")),
        "Score" = ifelse(length(Rvalues$form1)>0, round(summary(Rvalues$mod)$sctest[1],3), c("-"))))
      
      Rvalues$model_num <- isolate(Rvalues$model_num)+1
      Rvalues$cov_table <- c(isolate(Rvalues$cov_table), list(data.frame("covariates_transition" = isolate(Rvalues$form1))))
      Rvalues$trans_names <- c(isolate(Rvalues$trans_names),list(data.frame("transition_names" = isolate(Rvalues$names_tmat))))
      Rvalues$info_coef <- c(isolate(Rvalues$info_coef), list(Rvalues$tab_coef))
      
    }
  })



#----------------------------------------------
#Create the table of the estimated coefficients
#----------------------------------------------
output$model <- DT::renderDataTable({
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)>0){
      #With covariates
      return(Rvalues$tab_coef)
      
    }else{
      #Without covariates
      return(NULL)
    }
  }
})


#-------------------------------------
#Create the table of the loglikelihood
#-------------------------------------
output$model_loglik <- DT::renderDataTable({
  if(input$select_model == "Cox"){
    tab <- rbind("log likelihood"=summary(Rvalues$mod)$loglik)
    if(length(Rvalues$form1)>0){colnames(tab) <- c("Null model", "Fitted model")
    }else{colnames(tab) <- "Null model"}
    
    return(round(tab,3))
  }
}, options = list(dom = 't'))


#---------------------------
#Create the table of the AIC
#---------------------------
output$model_AIC <- DT::renderDataTable({
  if(input$select_model == "Cox"){
    tab <- rbind(AIC(Rvalues$mod, k=2))
    colnames(tab) <- "Fitted model"
    rownames(tab) <- "AIC"
    return(round(tab,3))
  }
}, options = list(dom = 't'))


#-----------------------------
#Create the table of the tests
#-----------------------------
output$model_tests <- DT::renderDataTable({
  if(input$select_model == "Cox"){
    if(length(Rvalues$form1)>0){
      #With covariates

      #Seguimos el mismo proceso que antes pero esta vez nos interesan los valores
      #relacionados con los diferentes tests
      tab <- as.data.frame(rbind("Likelihood ratio test"=summary(Rvalues$mod)$logtest,
                                 "Wald test" =summary(Rvalues$mod)$waldtest,
                                 "Score (logrank) test"=summary(Rvalues$mod)$sctest))
      colnames(tab) <- c("test", "df", "p-value")
      tab$test      <- formatC(tab$test,      format='f', digits=3)
      tab$df        <- formatC(tab$df,        format='f', digits=0)
      tab$"p-value" <- formatC(tab$"p-value", format='f', digits=6)
      return(tab)

    }else{
      return(NULL)
    }
  }
}, options = list(dom = 't'))



#----------------
#Model comparison
#----------------

react_table2 <- reactive({
  reactable(Rvalues$comp,
            
            defaultColDef = colDef(
              align = "left",
              minWidth = 120,
              headerStyle = list(background = "#f7f7f8")
            ),
            
            columns = list(
              `model` = colDef(maxWidth = 250),
              # `covariates` = colDef(maxWidth = 250),
              `log likelihood` = colDef(maxWidth = 250),
              `AIC` = colDef(maxWidth = 250),
              `Likelihood ratio` = colDef(maxWidth = 250),
              `Wald` = colDef(maxWidth = 250),
              `Score` = colDef(maxWidth = 250),
              
              `covariates` = colDef(
                maxWidth = 300,
                
                #code for level 1 nested table ####
                details = function(index){
                  cov <- Rvalues$cov_table[[index]]
                  div(
                    reactable(cov,
                              
                              columns = list(
                                "covariates_transition" = colDef(maxWidth = 250)
                              )))}),
              
              `transitions` = colDef(
                maxWidth = 300,
                
                #code for level 1 nested table ####
                details = function(index){
                  names <- Rvalues$trans_names[[index]]
                  div(
                    reactable(names,
                              
                              columns = list(
                                "transition_names" = colDef(maxWidth = 250)
                              )))}),
              
              `coefficients` = colDef(
                minWidth = 300,
                
                #code for level 1 nested table ####
                details = function(index){
                  coef <- Rvalues$info_coef[[index]]
                  div(
                    reactable(coef,
                              
                              columns = list(
                                
                                `coef` = colDef(maxWidth = 250),
                                `HR (95%CI)` = colDef(maxWidth = 250),
                                `p-value` = colDef(maxWidth = 250)
                              )))})),
            
            bordered     = TRUE,
            highlight    = TRUE,
            sortable     = TRUE,
            showSortable = TRUE,
            filterable   = TRUE,
            theme        = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            rownames     = FALSE
            
  )})



output$model_comp <- renderReactable(react_table2())


#----------------------------------------
#Save information to use in other session
#----------------------------------------

observeEvent(input$save,{
  react_table <- list(Rvalues$comp, Rvalues$cov_table, Rvalues$trans_names, Rvalues$info_coef)
  save(react_table,file = paste0('data_',session$token,'.RData'))
  output$session_id <- renderText(session$token)
})


#-----------------------------------
#Upload information of other session
#-----------------------------------

observeEvent(input$load,{
  load(paste0('data_',input$session_user,'.RData'))
  Rvalues$comp        <- react_table[[1]] 
  Rvalues$cov_table   <- react_table[[2]]
  Rvalues$trans_names <- react_table[[3]]
  Rvalues$info_coef   <- react_table[[4]]
})
