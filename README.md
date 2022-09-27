# MSMpred
Shiny R code for multistate web-app MSMpred

[MSMpred](https://www.grbio.eu/pubs/MSMpred/) is a shiny app with two main goals: 
  1) To fit a MSM from specific data.
  2) To predict the clinical evolution for a given individual based on a previously fitted MSM. 
The user can upload a new dataset, provided that it has the required format explained in the help page of the app.

As MSMpred is mainly designed for clinicians or researchers with little knowledge about MSMs or statisticians that want to analyse data in a quick and visual way, we have tried to make it very easy to use, to implement all the statistical part in an intuitive way and to include interpretations for the different outputs. 

The main sections of MSMpred and their features follow:
  - Home: brief explanation of how the app works.
  - Data: the user uploads his/her own dataset.
  - Model specification: the user specifies the model to fit.
  - Exploring the data: the app displays descriptive and non-parametric plots.
  - Model output: the user decides the type of model to be fitted, validates the model and compares the fitted models.
  - Predictions: the app returns some predictions for new individuals based on the information provided by the user.


![app](https://user-images.githubusercontent.com/79899112/192572533-6c33f2aa-450b-467c-851c-195ee3a0168c.png)
