library(DT)
library(ggplot2)
library(plotly)
library(mstate)
library(dplyr)
library(stringr)
library(lubridate)
library(bshazard)
library(cmprsk)
library(LoopDetectR)
library(reactable) # tabla reactiva (comparar modelos)
library(DiagrammeR)
library(survminer)
library(pals) # color palettes
library(summarytools) # tabla descriptiva

# Load functions & parameters
source('functions.R')
source('parameters.R')


divine_data <- read.table('DIVINE_app.csv', sep = ",", 
                          header = TRUE, stringsAsFactors = TRUE)

divine_data$safi <- round(divine_data$safi,3)
divine_data$lympho <- round(divine_data$lympho,5)


shinyServer(function(input, output, session) {
  Rvalues <- reactiveValues(data = divine_data,
                            
                            all_covar = c(wave = "wave", sex = "sex", age = "age",
                                          card_vasc = "card_vasc", immune = "immune",
                                          vacany = "vacany", vactype = "vactype",
                                          vacdose = "vacdose",  charlson = "charlson",
                                          safi = "safi", curb65 = "curb65", psi = "psi",
                                          crprot = "crprot", lympho = "lympho"),
                            labels_covar = c("wave", "sex", "age", "card_vasc", "immune",
                                             "vacany", "vactype", "vacdose",  "charlson", 
                                             "safi", "curb65", "psi", "crprot", "lympho"),
                            covar_type = c("factor", "factor", "numeric", "factor", 
                                           "factor", "factor", "factor", "factor", 
                                           "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"),
                            
                            form1 = c(),
                            sel_cov = c(),
                            
                            all_time = c("nopneum_time", "pneum_time", "reco_time", 
                                         "nimv_time", "imv_time", "dcharg_time", "death_time"),
                            
                            all_status = c("nopneum_status", "pneum_status", "reco_status", 
                                           "nimv_status", "imv_status", "dcharg_status", "death_status"),
                            
                            states = c("nopneum", "pneum", "reco", "nimv", "imv", "dcharg", "death"),
                            labels_states = c("nopneum", "pneum", "reco", "nimv", "imv", "dcharg", "death"),
                            num_states = 7,
                            initial_states = c("nopneum", "pneum"),
                            initial_not_transient_states = c("nopneum"),
                            initial_or_transient_states = c("nopneum", "pneum", "reco", "nimv", "imv"),
                            transient_states = c("pneum", "reco", "nimv", "imv"),
                            absorbing_states = c("dcharg", "death"),
                            out_states = character(0),
                            
                            nodes = data.frame(id =  c(1:7),
                                               names = c("nopneum", "pneum", "reco", "nimv", "imv", "dcharg", "death")),
                            colores = c(1,1,2,2,2,3,3),
                            
                            tmat = transMat(x = list(c(2, 6:7), c(3:5,7), c(6:7),  c(3,5,7), c(3,7), c(), c()), 
                                            names = c("nopneum", "pneum", "reco", "nimv", "imv", "dcharg", "death")),
                            names_tmat = c("nopneum -> pneum", "nopneum -> dcharg", "nopneum -> death",
                                           "pneum -> reco", "pneum -> nimv", "pneum -> imv", "pneum -> death",
                                           "reco -> dcharg", "reco -> death", "nimv -> reco", "nimv -> imv", 
                                           "nimv -> death", "imv -> reco", "imv -> death"),
                            
                            transit = data.frame(from = c(rep(1,3), rep(2,4), rep(3,2), rep(4,3), rep(5,2)),  
                                                 to = c(c(2, 6:7), c(3:5,7), c(6:7),  c(3,5,7), c(3,7))),
                            
                            datoslong = msprep(time = c("nopneum_time", "pneum_time", "reco_time", 
                                                      "nimv_time", "imv_time", "dcharg_time", "death_time"),
                                               status = c("nopneum_status", "pneum_status", "reco_status", 
                                                        "nimv_status", "imv_status", "dcharg_status", "death_status"),
                                               data = divine_data,
                                               trans = transMat(x = list(c(2, 6:7), c(3:5,7), c(6:7),  c(3,5,7), c(3,7), c(), c()), 
                                                              names = c("nopneum", "pneum", "reco", "nimv", "imv", "dcharg", "death")),
                                               start = list(state = sapply(divine_data$inistat,
                                                                           function(x,tmat) which(rownames(tmat) == x),
                                                                           tmat = transMat(x = list(c(2, 6:7), c(3:5,7), c(6:7),  c(3,5,7), c(3,7), c(), c()), 
                                                                                         names = c("nopneum", "pneum", "reco", "nimv", "imv", "dcharg", "death")),
                                                                           simplify = TRUE),
                                                            time = rep(0, nrow(divine_data)))),
                            follow_up_time = 30,
                            time_unit = "Days",
                            
                            predict = "Yes",
                            
                            comp = data.frame(),
                            names = data.frame(),
                            model_num = 1,
                            cov_table = list(),
                            trans_names = list(),
                            info_coef = list()
  )
  
  
  #Data
  source('tab_server_data.R',local = TRUE)

  #Model
  source('tab_server_model_specification.R',local = TRUE)

  #Knowing the data
  source('tab_server_eda.R',local = TRUE)

  #Fitted model
  source('tab_server_fittedmodel.R',local = TRUE)

  #Graphics
  source('tab_server_forestplot.R',local = TRUE)

  #Model validation
  source('tab_server_validation.R',local = TRUE)

  #Prediction
  source('tab_server_prediction.R',local = TRUE)
})


