text_aboutMSMpred <- '<ul><li><b>Objective</b>: fit and validate a multistate
                             model and make predictions using it easily.
                             </li><li><b>Data</b>: the user can upload data or use the 
                             example data.
                             <ul><li><b>Example data</b>: example data comes from the 
                             <a href = "https://issuu.com/enginyersdetelecomunicacio/docs/telecos72/33">DIVINE project</a>.</li></ul></li></ul>'

text_sectionsMSMpred <- '<ul><li><b>Data</b>: upload new data.
                             </li><li><b>Model specification</b>: define the transitions, choose the 
                             covariates and decide the follow-up time.
                             </li><li><b>Exploring the data</b>: descriptive graphs of the 
                             data.
                             </li><li><b>Fitted model</b>: choose the type of model and 
                             fit the model.
                             </li><li><b>Graphics</b>: receive a forest plot of the fitted
                             model.
                             </li><li><b>Model validation</b>: validate the model using
                             residuals.
                             </li><li><b>Predictions</b>: make predictions about new individuals.
                             </li></ul>'

text_security <- 'For security purposes in some institutions this app does not work. You can use the following link: 
                  <a href = "https://msmpred.shinyapps.io/MSMpred/">https://msmpred.shinyapps.io/MSMpred/</a><br><br>
                  If you have problems using it do not heasitate to contact us.'

text_DIVINE <- "The DynamIc eValuation of COVID-19 cliNical statEs and 
                                      their prognostic factors to improve the intra-hospital 
                                      patient management (DIVINE) project is funded
                                      by Generalitat de Catalunya (2020PANDE00148).
                                      <ul><li><b>Team</b>: clinicians from Instituto de 
                                      Investigación Biomédica de Bellvitge (IDIBELL) and
                                      biostatisticians from Universitat Politècnica de 
                                      Catalunya (UPC).
                                      </li><li><b>Data</b>: more than 4,000 hospitalized adult 
                                      COVID-19 patients from 8 Catalan hospitals during four waves
                                      of the pandemic.
                                      </li><li><b>Objectives</b>:
                                      <ol><li>Identify the most clinically relevant prognostic 
                                      factors for the events.
                                      </li><li><b>Develop a prediction tool to identify high-risk 
                                      individuals.</b>
                                      </li><li>Estimate the incubation time period of the 
                                      SARS-CoV-2.
                                      </li><li>Assess the patients’ profile over time.
                                      </li></ol></li></ul>"

text_states <- '<ol><li><b>No severe pneumonia (nopneum)</b>:
                                           patients that are hospitalized due to COVID-19 but do 
                                           not have severe pneumonia.
                                           </li><li><b>Severe pneumonia (pneum)</b>: 
                                           patients that are hospitalized due to COVID-19 and have 
                                           severe pneumonia.
                                           </li><li><b>Recovery (reco)</b>: 
                                           patients that had severe pneumonia while hospitalized due 
                                           to COVID-19, and follow at the hospital but they have 
                                           recovered.
                                           </li><li><b>Non-invasive mechanical ventilation (NIMV)</b>:
                                           patients that need non-invasive mechanical ventilation 
                                           while hospitalized due to COVID-19.
                                           </li><li><b>Invasive mechanical ventilation (IMV)</b>:
                                           patients that need invasive mechanical ventilation while 
                                           hospitalized due to COVID-19.
                                           </li><li><b>Discharge (dcharg)</b>: 
                                           patients that go home or to another hospital after
                                           recovering from COVID-19.
                                           </li><li><b>Death (death)</b>: 
                                           patients that die in the hospital due to COVID-19.
                                           </li></ol>'

text_covariates <- "<ul><li><b>Sex (sex)</b>: dicotomic covariate with categories
                                           Men and Women representing the sex of the patients.
                                           </li><li><b>Age (age)</b>: numeric covariate that represents the 
                                           age of the patients.
                                           </li><li><b>Pneumonia severity index (psi)</b>: numeric covariate 
                                           that represents the severity of the pneumonia.
                                           </li><li><b>Cardiovascular diseases (card_vasc)</b>: a dicotomic 
                                           covariate with categories No and Yes representing if the patients 
                                           have any cardiovascular disease or not.
                                           </li><li><b>blood oxygen saturation/oxigen supply (safi)</b>:
                                           numeric covariate that represents the respiratory limitations.
                                           </li><li><b>Charlson index (charlson.fact)</b>: index that predicts 10-year mortality.
                                           </li><li><b>C-reactive protein (crprot)</b>: a numeric covariate
                                           that represents the value of the C-reactive protein (CRP) of each 
                                           patient.
                                           </li><li><b>Lymphocytes (lympho)</b>: a numeric covariate that 
                                           represents the number of lymphocytes of each patient.</li></ul>"


text_format_file <- "<ul><li>Uploaded data needs to be a csv file where columns are
                                      separated by comas and decimals are represented by points.</li></ul>"

text_format_states <- "<ul><li>Time and status variables related to the different states of 
                                      the model need to be called as <b>x_time</b> and <b>x_status</b> respectively,
                                      where <b>x</b> corresponds to the name of each state (e.g. <b>death_time</b>
                                      and <b>death_status</b>).
                                      </li><li>The initial state(s) need to be included in the file following the
                                      previous naming and time equal 0 when the initial state is not a transient state.
                                      </li><li>Data must include one variable named <b>inistat</b> where the name of
                                      the initial state of each individual is specified.</li></ul>"

text_format_covariates <- "<ul><li>The variables that are not named as <b>x_time</b>, <b>x_status</b>,
                           <b>id</b> or <b>inistat</b> will be considered as covariates. 
                           </li><li>The names of the variables can not contain any number or dot.</li></ul>"

text_DIVINE_cohort <- "When the DIVINE cohort is used for confidentiality reasons only the information of 20 patients is shown,
                           although internally all the individuals are used."

text_log_score <- "The predictive performance is not computed automatically as it takes to much time. 
                                If wanted this performance can be computed clicking on the button."

text_predictions <- "You can predict the clinical evolution for new individuals, using the model previously fitted.
                              This prediction can be done for one or two individuals at the same time, making possible the
                              comparison of the evolution of individuals with different profiles."

text_GRBIO <- "The Grup de Recerca en Bioestadística i Bioinformàtica (GRBIO) research 
                                           group has expertise in Biostatistics and Bioinformatics, mainly Survival 
                                           Analysis, Clinical Trials and  Biostatistical Methods for Integrative 
                                           Analysis of Omics Data. Visit our web to see our activities, publications 
                                           and  statistical tools."

text_funding <- "This research was funded by the Ministerio de Ciencia e Innovación (Spain)
                                           [PID2019-104830RB-I00/ DOI (AEI): 10.13039/501100011033] and by
                                           Generalitat de Catalunya (2020PANDE00148)."