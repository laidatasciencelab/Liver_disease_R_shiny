#
# Global options for Shiny App: "CVD in Liver Disease Data Explorer"
#

# --- Load libraries --------------------------------------------------------

library(shiny)
library(tidyverse)
library(DT)
library(flexdashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(pals)
library(fmsb)

# --- DT Table Options ---------------------------------------------------------
myDT = function(data){
  data %>% DT::datatable(
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      searching = TRUE,
      fixedColumns =  FALSE,
      autoWidth =  TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    ))
}

# --- Plotting Options ---------------------------------------------------------

## ggPlot theme freq plots -----------------------------------------------------

theme_freq = theme_bw()+
  theme(title = element_text(size=rel(1.3), face = "bold"),
        axis.title.y = element_text(vjust=1.5),
        axis.title.x = element_text(vjust=-1.5),
        axis.text = element_text(size=rel(1.25)),
        axis.text.x = element_text(vjust=0.5),
        legend.text = element_text(size=rel(1.3)),
        strip.text = element_text(size=rel(1.3)),
        plot.margin = unit(c(1,1,1,2), "cm"),
        panel.grid.major = element_line(colour="grey80"),
        legend.position = "bottom")

##  Plot colors ----------------------------------------------------------------
# lancet palette in library ggsci: ggsci::pal_lancet()(5)
cld_cols =c("ALD"="#00468b", #ALD
            "Autoimmune liver disease"="#ed0000", #autoimm
            "HBV"="#42b540", #hbv 
            "HCV"="#0099b4", #hcv
            "NAFLD"="#925e9f",
            "any CLD"="grey50") 

# # JAMA palette in library ggsci: ggsci::pal_jama()(5)
# cvd_cols =c("Arrhythmia"="#374E55FF",
#             "Cardiomyopathy"="#DF8F44FF",
#             "Coronary heart disease"="#00A1D5FF",
#             "Peripheral vascular disease"="#B24745FF",
#             "Stroke and TIA"="#79AF97FF",
#             "any CVD"="grey50")

# dark palette used in plots for comorbidties
cvd_cols_dark =c("Arrhythmia"="#1B9E77",
            "Cardiomyopathy"="#D95F02",
            "Coronary heart disease"="#7570B3",
            "Peripheral vascular disease"="#E7298A",
            "Stroke and TIA"="#66A61E",
            "any CVD"="grey50")

# cols25 palette for individual CVDs
cvd_indiv_cols = c(
  "Abdominal aortic aneurysm"="#1F78C8",
  "Atrial fibrillation"="#ff0000",
  "Cardiomyopathy other"="#33a02c",
  "Coronary heart disease unspecified"="#6A33C2",
  "Dilated cardiomyopathy"="#ff7f00",
  "Heart failure"="#565656",
  "Hypertrophic Cardiomyopathy"="#FFD700",
  "Ischaemic stroke"="#a6cee3",
  "Myocardial infarction"="#FB6496",
  "Peripheral arterial disease"="#b2df8a",
  "Pulmonary embolism"="#CAB2D6",
  "Sick sinus syndrome"="#FDBF6F",
  "Stable angina"="#999999",
  "Stroke unspecified"="#EEE685",
  "Transient ischaemic attack"="#C8308C",
  "Unstable angina"="#FF83FA",
  "Venous thrombolism"="#C814FA")

# cols25 palette for individual non-CVD comorbidites
non_cvd_comorb_cols = 
  c("Barrett's oesophagus"="#1F78C8",
    "Complications of diabetes"="#ff0000",
    "Crohn's disease"="#33a02c",
    "Diabetes mellitus"="#6A33C2",
    "Diverticular disease of intestine"="#ff7f00",
    "Dyslipidaemia"="#565656",
    "Gastro-oesophageal reflux disease"="#FFD700",
    "Hypertension"="#a6cee3",
    "Irritable bowel syndrome"="#FB6496",
    "Jaundice"="#b2df8a",
    "Oesophagitis and oesophageal ulcer"="#CAB2D6",
    "Proteinuria"="#FDBF6F",
    "Proteinuric kidney diseases"="#999999",
    "Renal disease"="#EEE685")

      