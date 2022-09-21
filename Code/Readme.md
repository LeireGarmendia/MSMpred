To run the shiny app locally, it is necessary to follow the 3 steps below (it is assumed that R is installed and an internet connection is available):

  1)  Open an R session
  2) Make sure all the needed packages are installed
  3) Run the ui or server code.

Note that the app is optimized for Google Chrome, and some features may be limited in other browsers. Additionally, the first time this is run on a new machine may take a minute to install the required packages.

In this code folder you can find several R files with different goals:
  - functions: in this file all the needed functions are defined.
  - parameters: different parameters are initialized.
  - server: the instructions that the computer needs to build the app.
  - tab_server_data: the operations related with the data tab are defined.
  - tab_server_eda: the operations related with the exploring the data tab are defined.
  - tab_server_fittedmodel: the operations related with the model fitting are defined.
  - tab_server_forestplot: the operations related with the forest plot are defined.
  - tab_server_model_specification: the operations related with the model specification tab are defined.
  - tab_server_prediction: the operations related with the prediction tab are defined.
  - tab_server_validation: the operations related with the model validation are defined.
  - ui: the interface of the app is designed.
  - ui_text: the text to show in the app is written.


WARNING! Due to confidentiality reasons the dataset available on github is not the same used to develop the web tool, so if you run locally the example dataset you won't reach the same conclusions as the ones from the web.
