# PROJECT

Shiny App for interactive exploration of data investigating incidence, presentation, risk factors, life years lost and mortality of patients with liver disease and subsequent cardiovascular comorbidity.

## Shiny App Online
The published shiny app can be found here:
[https://lailab.shinyapps.io/cvd_in_liver_disease/
](https://lailab.shinyapps.io/cvd_in_liver_disease/
)
## Repository Structure
- [Data_Explorer](Data_Explorer): shiny app root folder
- [Data_Explorer/ui.R](Data_Explorer/ui.R): shiny app R code for user interface, frontend code
- [Data_Explorer/server.R](Data_Explorer/server.R): shiny app R code for handling user input and producing outputs, backend code
- [Data_Explorer/global.R](Data_Explorer/global.R): script with global options and requirements (e.g. loading libraries, setting color options, defining ggplot theme)
- [Data_Explorer/helper.R](Data_Explorer/helper.R): script with functions to load inputs and produce all plots, primary purpose is to keep server.R code clean and easy to read


## Shiny App Pages

1. About
2. Chronic Liver Disease Incidence
3. CVD Incidence
4. Comorbidities in Liver Disease
5. Initial CVD presentation
6. Risk Factors
7. Live Years Lost
8. Disclaimer
