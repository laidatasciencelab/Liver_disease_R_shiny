#
# Web interface code Shiny App: "Incidence and risk of CVD in people with liver disease"
#

# --- Preface ---------------------------------------------------------------

# source Global Code
source("global.R")

source("helper.R")


# --- Build UI --------------------------------------------------------------

ui <- 
    # define general structure of web interface as page that contains top level 
    # navigation
    navbarPage(
        
    # shiny theme determines general optics like color and font 
    # of web interface elements
        
        # set app name
        title = "Incidence and risk of CVD in people with liver disease",
    
        theme = shinytheme("cerulean"),
        collapsible = T,
        selected = "About",
        inverse = F,
        fluid=T,

# --- 1. Page: About -----------------------------------------------------------
# includes project description, documentations, etc
        
        tabPanel(
            
            title = "About",
            absolutePanel(
                top = 100, left =50, bottom = 50, width = "80%",
                includeMarkdown("www/markdown/about.md")
            )
            
        ), # end: tabPanel About


# --- 2. Page: Chronic liver disease incidence ---------------------------------
        tabPanel(
            
            title = "Chronic liver disease Incidence",
            
            tabsetPanel(
                
                tabPanel("Incidence Frequency",
                         
                         h2("Incidence Plots with Confidence Interval for
                            Chronic Liver Disease (CLD)"),
                         
                         fluidRow(

                             column(5, offset=6,
                                    
                                    style = "background-color:#F5F5F5;",
                                    
                                    h4(tags$b("Data Groupings")),

## --- Input 2.1: groupings ----------------------------------------------------
                                    radioGroupButtons(
                                        inputId = "picker_2_1_groupings",
                                        label = "Select Data Group", 
                                        choices = c("No Groups", "Age Groups", "Regional Groups"),
                                        status = "primary",
                                        selected = "No Groups",
                                        direction = "horizontal",
                                        justified = TRUE
                                    ) # end picker 2.1.
                                    
                             ) # end column

                        ), # end fluidRow

                        hr(),
                         
## --- Plot 2.1: asir freq CLD -----------------------------------------------------
                        plotOutput("plot_2_1_asir_freq_cld") %>% 
                             withSpinner()
                ), # end tabPanel 2.1. Freq Plots

                tabPanel("Regional Plots",

                     h2("Comparison of Regional Differences for
                        Chronic Liver Disease (CLD) Incidence"),

                     fluidRow(

                        column(5,
                        
                            style = "background-color:#F5F5F5;",
                            
                            h4(tags$b("Left Map")),
                            
## --- Input 2.2.1: CLD event 1 ---------------------------------------------------
                            pickerInput(
                                inputId = "picker_2_2_1_event1",
                                label = "Select Chronic Liver Disease to Display",
                                choices = c("any CLD", names(cld_cols)[1:5]),
                                options = pickerOptions(
                                    actionsBox = T,
                                    liveSearch = T),
                                selected = "any CLD",
                                multiple = F) # end picker 2.2.1

                        ), # end column

                        column(5, offset = 1,
                               
                            style = "background-color:#F5F5F5;",
                            
                            h4(tags$b("Right Map")),
                            
                               
## --- Input 2.2.2: CLD event 2 ---------------------------------------------------
                           pickerInput(
                               inputId = "picker_2_2_2_event2",
                               label = "Select Chronic Liver Disease to Display",
                               choices = c("any CLD", names(cld_cols)[1:5]),
                               options = pickerOptions(
                                   actionsBox = T,
                                   liveSearch = T),
                               selected = "any CLD",
                               multiple = F) # end picker 2.2.1

                         ) # end column

                    ), # end fluidRow

                    hr(),

                    fluidRow(
                        
                        column(6,
## --- Plot 2.2.1: CLD asir map1 -----------------------------------------------------
                           plotOutput("plot_2_2_1_asir_cld_map") %>%
                               withSpinner()
                        ),

                        column(6,
## --- Plot 2.2.2: CLD asir map2 -----------------------------------------------------
                               plotOutput("plot_2_2_2_asir_cld_map") %>%
                                   withSpinner()
                        )

                    ), # end Fluidrow

                 ) # end tabPanel 2.2. Regional Plots
                
            ), # end tabsetPanel
            
        ), # end tabPanel 2. Chronic liver disease (CLD) incidence

# --- 3. Page: CVD incidence ---------------------------------------------------
tabPanel(
    
    title = "CVD Incidence",
    
    tabsetPanel(
        
        tabPanel("Incidence Frequency",
                 
                 h2("Incidence Plots with Confidence Interval for
                    Cardiovascular Disease (CVD)"),
                 
                 fluidRow(
                     
                     column(5,
                            
                            style = "background-color:#F5F5F5;",
                            
                            h4(tags$b("Base Cohort")),
                            
## --- Input 3.1: Base Cohort --------------------------------------------------
                             pickerInput(
                                 inputId = "picker_3_1_basecohort",
                                 label = "Select Base Cohort",
                                 choices = unique(data_asir$Baseline_Cohort)[c(5,3:4,6:8,2)],
                                 options = pickerOptions(
                                     actionsBox = T,
                                     liveSearch = T),
                                 selected = "prevalent CLD",
                                 multiple = F), # end picker 3.1.

                     ), # end column
                     
                     column(5, offset = 1,
                            
                            style = "background-color:#F5F5F5;",
                            
                            h4(tags$b("Data Groupings")),
                            
## --- Input 3.2: groupings ----------------------------------------------------
                            radioGroupButtons(
                                inputId = "picker_3_2_groupings",
                                label = "Select Data Group", 
                                choices = c("No Groups", "Age Groups", "Regional Groups"),
                                status = "primary",
                                selected = "No Groups",
                                direction = "horizontal",
                                justified = TRUE
                            ) # end picker 2.1.
                            
                     ) # end column
                     
                 ), # end fluidRow
                 
                 hr(),
                 
## --- Plot 3.1: asir freq CVD -----------------------------------------------------
                 plotOutput("plot_3_1_asir_freq_cvd") %>% 
                     withSpinner()
        ), # end tabPanel 3.1. Freq Plots
        
        tabPanel("Regional Plots",

                 h2("Comparison of Regional Differences for
                    Cardiovascular Disease (CVD) Incidence"),

                 fluidRow(

                     column(5,

                            style = "background-color:#F5F5F5;",
                            
                            h4(tags$b("Left Map")),
                            

## --- Input 3.2.1: CVD basecohort 1 ---------------------------------------------------
                            pickerInput(
                                inputId = "picker_3_2_1_basecohort",
                                label = "Select Base Cohort",
                                choices = unique(data_asir$Baseline_Cohort)[c(5,3:4,6:8,2)],
                                options = pickerOptions(
                                    actionsBox = T,
                                    liveSearch = T),
                                selected = "prevalent CLD",
                                multiple = F), # end picker 3.2.1                            

## --- Input 3.3.1: CVD event 1 ---------------------------------------------------
                            pickerInput(
                                inputId = "picker_3_3_1_event",
                                label = "Select CVD Group to Display",
                                choices = c("any CVD", names(cvd_cols_dark)[1:5]),
                                options = pickerOptions(
                                    actionsBox = T,
                                    liveSearch = T),
                                selected = "any CVD",
                                multiple = F) # end picker 3.3.1

                     ), # end column

                     column(5, offset = 1,

                            style = "background-color:#F5F5F5;",
                            
                            h4(tags$b("Right Map")),
                            

## --- Input 3.2.2: CVD basecohort 2 ---------------------------------------------------
                            pickerInput(
                                inputId = "picker_3_2_2_basecohort",
                                label = "Select Base Cohort",
                                choices = unique(data_asir$Baseline_Cohort)[c(5,3:4,6:8,2)],
                                options = pickerOptions(
                                    actionsBox = T,
                                    liveSearch = T),
                                selected = "prevalent CLD",
                                multiple = F), # end picker 3.2.2                          
                            
## --- Input 3.3.2: CVD event 2 ---------------------------------------------------
                            pickerInput(
                                inputId = "picker_3_3_2_event",
                                label = "Select CVD Group to Display",
                                choices = c("any CVD", names(cvd_cols_dark)[1:5]),
                                options = pickerOptions(
                                    actionsBox = T,
                                    liveSearch = T),
                                selected = "any CVD",
                                multiple = F) # end picker 3.3.3

                     ) # end column

                 ), # end fluidRow

                 hr(),

                 fluidRow(

                     column(6,
## --- Plot 3.2.1: CLD asir map1 -----------------------------------------------------
                            plotOutput("plot_3_2_1_asir_cvd_map") %>%
                                withSpinner()
                     ),

                     column(6,
## --- Plot 3.2.2: CLD asir map2 -----------------------------------------------------
                            plotOutput("plot_3_2_2_asir_cvd_map") %>%
                                withSpinner()
                     )

                 ), # end Fluidrow

        ) # end tabPanel 2.2. Regional Plots
        
    ), # end tabsetPanel
    
), # end tabPanel 2. Chronic liver disease (CLD) incidence


# --- 4. Page: Comorbidities in liver disease ----------------------------------
        tabPanel(
            title = "Comorbidities in Liver Disease",
            
            fluidRow(
                column(3,
                       
                       align="center",    
                       
                       style = "background-color:#F5F5F5;",
                       
                       br(),
                       
## --- Input 4.1: CLD Selection ------------------------------------------------
                       pickerInput(
                           inputId = "picker_4_1_cld",
                           label = "Select Liver Disease to Display",
                           choices = names(cld_cols)[1:5],
                           options = pickerOptions(
                               actionsBox = T,
                               liveSearch = T,
                               noneSelectedText = "Select at least one"),
                           selected = names(cld_cols)[1:5],
                           multiple = T), # end picker 3.5   
                ),                        
                
                column(3, offset = 1,
                       
                       align="center",    
                       
                       style = "background-color:#F5F5F5;",
                       
                       br(),
                       
## --- Input 4.2: Comorb Selection ------------------------------------------------
                       pickerInput(
                           inputId = "picker_4_2_non_CVD_comorb",
                           label = "Select Comorbidites to Display",
                           choices = names(non_cvd_comorb_cols),
                           options = pickerOptions(
                               actionsBox = T,
                               liveSearch = T,
                               noneSelectedText = "Select at least one"),
                           selected = names(non_cvd_comorb_cols),
                           multiple = T), # end picker 3.6   
                ),
                column(3, offset = 1,
                       
                       align="center",    
                       
                       style = "background-color:#F5F5F5;",
                       
                       br(),
                       
## --- Input 4.3: Comorb Selection ------------------------------------------------
                       radioGroupButtons(
                           inputId = "radio_4_3_gender",
                           label = "Select Gender to Display",
                           choices = c("Women","Men"),
                           status = "primary",
                           selected = "Women",
                           direction = "horizontal",
                           justified = TRUE) # end radio button 3.4 
                )
            ), # end FluidRow
            
            hr(),
            
## --- Plot 4.1: Non-CVD comorb per age ----------------------------------------
            fluidRow( # use fluidrow to create <div> object which can be centered
                
                align="center",    
                     
                uiOutput("ui_4_0_heading_gender"),
    
                plotOutput("plot_4_1_comorb_non_CVD") %>% 
                    withSpinner()                    
            )
        ), # end TabPanel Page 4: Comorbidities in liver disease

# --- 5. Page: Initial CVD presentation ----------------------------------------
        tabPanel(
            
            title  = "Initial CVD presentation",
            
            fluidRow(
                column(2,
                
                    align="center",   
                   style = "background-color:#F5F5F5;",
                   
                   br(),

## --- Input 5.1: CLD Selection ------------------------------------------------
                   pickerInput(
                       inputId = "picker_5_1_cld",
                       label = "Select Liver Disease",
                       choices = names(cld_cols)[1:5],
                       options = pickerOptions(
                           actionsBox = T,
                           liveSearch = T,
                           noneSelectedText = "Select at least one"),
                       selected = names(cld_cols)[1:5],
                       multiple = T), # end picker 5.1
                ),
                column(2, offset = 1,
                    
                    align="center",
                    style = "background-color:#F5F5F5;",
                       
                    br(),

## --- Input 5.2: CVD Selection Composite ------------------------------------------
                   pickerInput(
                       inputId = "picker_5_2_comp",
                       label = "Select CVD Groups",
                       choices = names(cvd_cols_dark)[1:5],
                       options = pickerOptions(
                           actionsBox = T,
                           liveSearch = T,
                           noneSelectedText = "Select at least one"),
                       selected = names(cvd_cols_dark)[1:5],
                       multiple = T), # end picker 5.2
                
                ), # end of column

                column(2, offset = 1,
                       
                       align="center",
                       style = "background-color:#F5F5F5;",
                       
                       br(),
                       
                       ## --- Input 5.4: Stratify per gender ------------------------------------------
                       
                       radioGroupButtons(
                           inputId = "radio_5_4_gender",
                           label = "Stratify by gender",
                           choices = c("No","Yes"),
                           status = "primary",
                           selected = "No",
                           direction = "horizontal",
                           size="normal",
                           justified = FALSE) # end radio button 5.4
                       
                ), # end column

                column(2, offset=1,
                   
                   align="center",    
                   style = "background-color:#F5F5F5;",
                   
                   br(),
                       
## --- Input 5.3: CVD Selection Indiv ------------------------------------------

                # picker is reactive to selection of input "picker_3_2_comp" and will only
                # contain individual CVD diseases of chosen CVD composite groups,
                # picker ID is: picker_3_3_CVD_indiv
                    uiOutput("conditional_picker_CVD_indiv")
                )
            ), # end fluidrow


               
            hr(),
            
            tabsetPanel(

                tabPanel("CVD Presentation overall",
                
## --- Plot 5.1: CVD comorb yes/no ---------------------------------------------

                    plotOutput("plot_5_1_comorb_yes_no") %>%
                        withSpinner()
                ), #end tabpanel

                tabPanel("Presentation of CVD Composites",

## --- Plot 5.2: CVD comorb composite ----------------------------------------------
                         
                     plotOutput("plot_5_2_comorb_comp") %>%
                         withSpinner()
                 ),
                
                tabPanel("Presentation of individual CVDs",
                    
                    plotOutput("plot_5_3_comorb_indiv") %>%
                        withSpinner()
                ),

                tabPanel("Per age group: CVD group",
                         
                     plotOutput("plot_5_4_age_group_comp") %>%
                         withSpinner()
                ),

                tabPanel("Per age group: CVD individual",
                         
                     plotOutput("plot_5_5_age_group_indiv") %>%
                         withSpinner()
                )

         ), # end tabsetpanel
        ), # end tab panel 5. 


# --- 6. Page: Risk Factors ----------------------------------------------------
# includes Cox analysis

        tabPanel(
            
            title = "Risk Factors",
            
            fluidRow(
                column(5,
                       style = "background-color:#F5F5F5;",
                       
## --- Input 6.1: PValue -------------------------------------------------------
                       sliderTextInput(
                           inputId = "slider_6_1_pvalue",
                           label = "Choose a P-value threshold:",
                           choices = c(1,0.05, 0.001),
                           grid = TRUE
                       )

                ), # end column 1/2

                column(5, offset=1,
                       
                       style = "background-color:#F5F5F5;",
                       

## --- Input 6.2: Effect Direction ---------------------------------------------
                       
                       radioGroupButtons(
                           inputId = "radio_6_2_effect_direction",
                           label = "Choose Effect Direction",
                           choices = c("All","Predisposing", "Protective"),
                           status = "primary",
                           selected = "All",
                           direction = "horizontal",
                           justified = TRUE) # end radio button 6.2
                       
                ) # end column 2/2
            ), # end fluidRow

            hr(),

            tabsetPanel(
                
                tabPanel("Age in Liver Disease Patients as Risk Factor for CVD",
                         
## --- Plot 6.1: Risk Factor Age -----------------------------------------------
                         
                plotOutput("plot_6_1_hazard_age") %>% 
                         withSpinner()
                         
                ),

                tabPanel("Any other Risk Factor in Liver Disease Patients for CVD",

## --- Plot 6.2: All other Risk Factors ----------------------------------------
                         
                     plotOutput("plot_6_2_hazard_other") %>% 
                         withSpinner(),    
                         
                )

            ), # end tabset panel

        ), # end: tabPanel Risk Factors 

# --- 7. Page: Life Years Lost -------------------------------------------------

        tabPanel(
            
            title = "Life Years Lost",
            
            fluidRow(
                column(5,
                       
                    style = "background-color:#F5F5F5;",
                       
## --- Input 7.1: CLD--- -------------------------------------------------------
                    pickerInput(
                        inputId = "picker_7_1_cld",
                        label = "Select Liver Disease",
                        choices = names(cld_cols)[1:5],
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T,
                            noneSelectedText = "Select at least one"),
                        selected = names(cld_cols)[1:5],
                        multiple = T), # end picker 5.1   
                       
                ), # end column 1/2
                
                column(5, offset = 1,
                    
                    style = "background-color:#F5F5F5;",
                       
## --- Input 7.2: CVD Group ---------------------------------------------
                       
                    pickerInput(
                        inputId = "picker_7_2_CVD_group",
                        label = "Select CVD Groups",
                        choices = names(cvd_cols_dark),
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T,
                            noneSelectedText = "Select at least one"),
                        selected = names(cvd_cols_dark),
                        multiple = T), # end picker 5.2 
                       
                ) # end column 2/2
            ), # end fluidRow

            hr(),

            tabsetPanel(
                tabPanel("Life Years Lost per Liver Disease and CVD",
            
## --- Plot 7.1: Forest Plot Life Years Lost -----------------------------------

                    plotOutput("plot_7_1_lly_forest") %>% 
                        withSpinner()
                ),

## --- Plot 7.2: Forest Plot Life Years Lost per Gender -----------------------------------
            tabPanel("Life Years Lost per Gender",
                     

                     plotOutput("plot_7_2_lly_gender") %>% 
                         withSpinner()
            ),

                tabPanel("Life Years Lost per Age Group",
            
## --- Plot 7.3: Radar Plot Life Years Lost-------------------------------------
            
                    fluidRow(
                        
                        column(4, 
                               
                               plotOutput("plot_7_3_1_lly_radar.30") %>% 
                                   withSpinner()
                               
                        ), # end column 1/3
                        
                        column(4, 
                               
                               plotOutput("plot_7_3_2_lly_radar.40") %>% 
                                   withSpinner()
                               
                        ), # end column 2/3
                        
                        column(4, 
                               
                               plotOutput("plot_7_3_3_lly_radar.50") %>% 
                                   withSpinner()
                               
                        ) # end column 3/3
                        
                    ), # end fluidRow
        
                    fluidRow(
                        
                        column(4, 
                               
                               plotOutput("plot_7_3_4_lly_radar.60") %>% 
                                   withSpinner()
                               
                        ), # end column 1/3
                        
                        column(4, 
                               
                               plotOutput("plot_7_3_5_lly_radar.70") %>% 
                                   withSpinner()
                               
                        ), # end column 2/3
                        
                        column(4, 
                               
                               plotOutput("plot_7_3_6_lly_radar.80") %>% 
                                   withSpinner()
                               
                        ) # end column 3/3
                        
                    ) # end fluidRow

                ) # end tabpanel

            ) # end tabsetpanel
            
        ), # end: tabPanel Life Years Lost

# --- 8. Page: Disclaimer ------------------------------------------------------
# includes disclaimers

        tabPanel(

            "Disclaimer",
            absolutePanel(
                top = 100, left =50, bottom = 50, width = "80%",
                includeMarkdown("www/markdown/disclaimer.md")
            )

        ) # end: tabPanel Disclaimer
    
) # end: ui