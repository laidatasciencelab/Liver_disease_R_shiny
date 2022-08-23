#
# R Server code for Shiny App: "Incidence and risk of CVD in people with liver disease"
#


# --- Preface ------------------------------------------------------------------

# source Global Code
source("global.R")

# source helper Code
source("helper.R")


# --- R Server Code ------------------------------------------------------------


shinyServer(function(input, output) {

# --- 2. Page: CLD ASIR --------------------------------------------------------

# 2.1 Plot Asir freq CLD -------------------------------------------------------
  output$plot_2_1_asir_freq_cld = renderPlot(
    res=80,
    height = shiny::reactive(
      ifelse(input$picker_2_1_groupings=="Regional Groups", 800,400)),
    {
      
      if (input$picker_2_1_groupings=="No Groups"){
        
        plot_data = data_asir %>% 
          filter(Baseline_Cohort == "all") %>% 
          filter(Age_Group == "all") %>% 
          filter(Prac_Region == "England")
        
        p2.1 = plot_data %>% 
          plot_asir_freq(data = .,
              title = "Chronic Liver Disease Incidence Rates")
        
        return(p2.1)
        
      } else if (input$picker_2_1_groupings=="Age Groups"){
        
        plot_data = data_asir %>% 
          filter(Baseline_Cohort == "all") %>% 
          filter(Age_Group != "all") %>% 
          filter(Prac_Region == "England")
        
        p2.1.age = plot_data %>% 
          plot_asir_freq(data = .,
                         title = "Chronic Liver Disease Incidence Rates") +
          facet_grid(.~Age_Group)
        
        return(p2.1.age)
        
      } else if (input$picker_2_1_groupings=="Regional Groups"){
        
        plot_data = data_asir %>% 
          filter(Baseline_Cohort == "all") %>% 
          filter(Age_Group == "all") %>% 
          filter(Prac_Region != "England") %>%
          mutate(Prac_Region_Label = paste0(Prac_Region))

        p2.1.region = plot_data %>% 
          plot_asir_freq(data = .,
                         title = "Chronic Liver Disease Incidence Rates") +
          facet_wrap(.~Prac_Region_Label)
        
        return(p2.1.region)
        
      }
      
    })
  
# 2.2.1 Plot CLD Asir Map 1 --------------------------------------------------------
  output$plot_2_2_1_asir_cld_map = renderPlot(
    res = 120, height = 600,
    {
      
      plot_data = data_asir %>% 
        filter(Baseline_Cohort == "all") %>% 
        filter(Age_Group == "all") %>% 
        filter(Event==input$picker_2_2_1_event1) %>% 
        filter(Prac_Region != "England") %>%
        mutate(prac_region = as.numeric(Prac_Region_ID)) %>% 
        select("prac_region",
               "value" = ASIR_per_100k)
      
      p2.2.1 = plot_asir_map(data=plot_data,
                             map = prac_region_map) +
        scale_fill_viridis_c(
          option = "viridis",
          na.value = "grey90") +
        labs(title=paste0("Base Cohort: No Prior Chronic Liver Disease",
                          "\nEvent: ",input$picker_2_2_1_event1),
             fill="Age-standardized\nIncidence Rate")
      
      
      return(p2.2.1)
    })
  
# 2.2.2 Plot CLD Asir Map 2 --------------------------------------------------------
  output$plot_2_2_2_asir_cld_map = renderPlot(
    res = 120, height = 600,
    {
      
      plot_data = data_asir %>% 
        filter(Baseline_Cohort == "all") %>% 
        filter(Age_Group == "all") %>% 
        filter(Event==input$picker_2_2_2_event2) %>% 
        filter(Prac_Region != "England") %>%
        mutate(prac_region = as.numeric(Prac_Region_ID)) %>% 
        select("prac_region",
               "value" = ASIR_per_100k)
      
      p2.2.2 = plot_asir_map(data=plot_data,
                             map = prac_region_map) +
        scale_fill_viridis_c(
          option = "viridis",
          na.value = "grey90") +
        labs(title=paste0("Base Cohort: No Prior Chronic Liver Disease",
                          "\nEvent: ",input$picker_2_2_2_event2),
             fill="Age-standardized\nIncidence Rate")
      
      
      return(p2.2.2)
    })
  
# # 2.1 Table all ASIR Numbers ---------------------------------------------------
#   output$table_2_1_asir = DT::renderDataTable({
#     data_asir %>% myDT()
#   })
  
  
# --- 3. Page: CVD ASIR --------------------------------------------------------
  
  
# 3.1 Plot Asir freq CVD -------------------------------------------------------
  output$plot_3_1_asir_freq_cvd = renderPlot(
    res=80,
    height = shiny::reactive(
      ifelse(input$picker_3_2_groupings=="Regional Groups", 800,400)),
    {
      
      if (input$picker_3_2_groupings=="No Groups"){
        
        plot_data = data_asir %>% 
          filter(Baseline_Cohort == input$picker_3_1_basecohort) %>% 
          filter(Age_Group == "all") %>% 
          filter(Prac_Region == "England")
        
        p3.2 = plot_data %>% 
          plot_asir_freq(data = .,
                         title = paste0("CVD Incidence Rates in ",
                                        input$picker_3_1_basecohort))
        
        return(p3.2)
        
      } else if (input$picker_3_2_groupings=="Age Groups"){
        
        plot_data = data_asir %>% 
          filter(Baseline_Cohort == input$picker_3_1_basecohort) %>% 
          filter(Age_Group != "all") %>% 
          filter(Prac_Region == "England")
        
        p3.2.age = plot_data %>% 
          plot_asir_freq(data = .,
                         title = paste0("CVD Incidence Rates in ",
                                        input$picker_3_1_basecohort)) +
          facet_grid(.~Age_Group)
        
        return(p3.2.age)
        
      } else if (input$picker_3_2_groupings=="Regional Groups"){
        
        plot_data = data_asir %>% 
          filter(Baseline_Cohort == input$picker_3_1_basecohort) %>% 
          filter(Age_Group == "all") %>% 
          filter(Prac_Region != "England") %>%
          mutate(Prac_Region_Label = paste0(Prac_Region))
        
        p3.2.region = plot_data %>% 
          plot_asir_freq(data = .,
                         title = paste0("CVD Incidence Rates in ",
                                        input$picker_3_1_basecohort)) +
          facet_wrap(.~Prac_Region_Label)
        
        return(p3.2.region)
        
      }
      
    })

# 3.2.1 Plot CVD Asir Map 1 --------------------------------------------------------
  output$plot_3_2_1_asir_cvd_map = renderPlot(
    res = 120, height = 600,
    {
      
      plot_data = data_asir %>% 
        filter(Baseline_Cohort == input$picker_3_2_1_basecohort) %>% 
        filter(Age_Group == "all") %>% 
        filter(Event==input$picker_3_3_1_event) %>% 
        filter(Prac_Region != "England") %>%
        mutate(prac_region = as.numeric(Prac_Region_ID)) %>% 
        select("prac_region",
               "value" = ASIR_per_100k)
      
      p3.2.1 = plot_asir_map(data=plot_data,
                             map = prac_region_map) +
        scale_fill_viridis_c(
          option = "magma",
          na.value = "grey90") +
        labs(title=paste0("Base Cohort: ",input$picker_3_2_1_basecohort,
                          "\nEvent: ",input$picker_3_3_1_event),
             fill="Age-standardized\nIncidence Rate")
      
      
      return(p3.2.1)
    })
  
  
# 3.2.2 Plot CVD Asir Map 2 --------------------------------------------------------
  output$plot_3_2_2_asir_cvd_map = renderPlot(
    res = 120, height = 600,
    {
      
      plot_data = data_asir %>% 
        filter(Baseline_Cohort == input$picker_3_2_2_basecohort) %>% 
        filter(Age_Group == "all") %>% 
        filter(Event==input$picker_3_3_2_event) %>% 
        filter(Prac_Region != "England") %>%
        mutate(prac_region = as.numeric(Prac_Region_ID)) %>% 
        select("prac_region",
               "value" = ASIR_per_100k)
      
      p3.2.2 = plot_asir_map(data=plot_data,
                             map = prac_region_map) +
        scale_fill_viridis_c(
          option = "magma",
          na.value = "grey90") +
        labs(title=paste0("Base Cohort: ",input$picker_3_2_2_basecohort,
                          "\nEvent: ",input$picker_3_3_2_event),
             fill="Age-standardized\nIncidence Rate")
      
      
      return(p3.2.2)
    })  
  

# --- 4. Page: Comorbidities in liver disease ----------------------------------

# 4.0 Heading specifying which gender is displayed------------------------------
  output$ui_4_0_heading_gender = renderUI({
        h2(paste0("Displaying Comorbidites in Liver Disease for: ",
                  input$radio_4_3_gender))
    })  
  
# 4.1 plot non-CVD Comorbidity ---------------------------------------------------------
  output$plot_4_1_comorb_non_CVD = renderPlot(
    width = 1000, res=80,
    height = reactive(300+length(input$picker_4_2_non_CVD_comorb)*120),
    {
      
      p4.1 = plot_comorb_non_CVD(data=data_non_CVD_comorb, 
                                 CLD=input$picker_4_1_cld,
                                 Comorb=input$picker_4_2_non_CVD_comorb,
                                 plotGender=input$radio_4_3_gender)
      
      return(p4.1)
    })  
  
# --- 5. Page: Initial CVD presentation ----------------------------------------
# 5.0 conditional picker individual CVD diseases based on composite
  
  output$conditional_picker_CVD_indiv <- renderUI({
    
    CVD_indiv = data_CVD_comorb %>% 
      filter(cvd_composite_group %in% input$picker_5_2_comp) %>%
      select(`condition_cvd_clean_name`) %>% 
      unique() %>% 
      unlist() %>% 
      as.character() %>% 
      sort()
    
    pickerInput(inputId = "picker_5_3_CVD_indiv", 
                label = "Select Individual CVD",
                choices = CVD_indiv,
                options = pickerOptions(
                  actionsBox = T,
                  liveSearch = T,
                  noneSelectedText = "Select at least one"),
                selected = names(cvd_indiv_cols),
                multiple = T)
  })  
  
  
# 5.1 plot comorb has/has-not CVD ----------------------------------------------
  output$plot_5_1_comorb_yes_no = renderPlot(
    res=80,
    height = reactive(ifelse(input$radio_5_4_gender=="No", 400, 700)),
    {
    
    if(input$radio_5_4_gender=="No"){
      
      p5.1 = plot_comorb_CVD_yes_no.all_gender(data = data_CVD_comorb,
                                               CLD=input$picker_5_1_cld)
      
      return(p5.1)
      
    } else {
      
      p5.1 = plot_comorb_CVD_yes_no.per_gender(data = data_CVD_comorb,
                                               CLD=input$picker_5_1_cld)
      
      return(p5.1)
      
    }
    
  })
  
# 5.2 plot comorb composites----------------------------------------------------
    output$plot_5_2_comorb_comp = renderPlot(
      res=80,
      height = reactive(ifelse(input$radio_5_4_gender=="No", 400, 700)),
      {

      if(input$radio_5_4_gender=="No"){

        p5.2 = plot_comorb_CVD_comb.all_gender(data = data_CVD_comorb,
                                                CLD=input$picker_5_1_cld,
                                                CVD=input$picker_5_2_comp)

        return(p5.2)

      } else {

        p5.2 = plot_comorb_CVD_comb.per_gender(data = data_CVD_comorb,
                                                CLD=input$picker_5_1_cld,
                                                CVD=input$picker_5_2_comp)

        return(p5.2)

      }
    })

# 5.3 plot comorb indiv --------------------------------------------------------
    output$plot_5_3_comorb_indiv = renderPlot(
      res=80,
      height = reactive(ifelse(input$radio_5_4_gender=="No", 500, 800)),
      {

      if(input$radio_5_4_gender=="No"){

          p5.3 = plot_comorb_CVD_indiv.all_gender(data = data_CVD_comorb,
                                        CLD=input$picker_5_1_cld,
                                        CVD=input$picker_5_3_CVD_indiv)

          return(p5.3)

      } else {

        p5.3 = plot_comorb_CVD_indiv.per_gender(data = data_CVD_comorb,
                                     CLD=input$picker_5_1_cld,
                                     CVD=input$picker_5_3_CVD_indiv)

        return(p5.3)

      }
    })

# 5.4 plot comorb per age group composites--------------------------------------
    output$plot_5_4_age_group_comp = renderPlot(
      res=80,
      height =500,
      {

      p5.4 = plot_comorb_CVD_comb.per_age(data = data_CVD_comorb,
                                          CLD=input$picker_5_1_cld,
                                          CVD_comp=input$picker_5_2_comp)

      return(p5.4)
    })

# 5.5 plot comorb per age group individual CVD ---------------------------------
    output$plot_5_5_age_group_indiv = renderPlot(
      res=80,
      height =600,
      {

      p5.5 = plot_comorb_CVD_indiv.per_age(data = data_CVD_comorb,
                                          CLD=input$picker_5_1_cld,
                                          CVD=input$picker_5_3_CVD_indiv)

      return(p5.5)
    })

# 6. Page: Risk Factors --------------------------------------------------------

# 6.1. plot age hazard ratios --------------------------------------------------
  output$plot_6_1_hazard_age = renderPlot(
    res=80, height = 400,
    {
    
  
  p6_1 = plot_harzard(data = data_hazard,
                          age= "yes",
                          Pvalue_Threshold=input$slider_6_1_pvalue,
                          Effect_Direction = input$radio_6_2_effect_direction)
  
  return(p6_1)
    
  })
  
# 6.2. plot age hazard ratios --------------------------------------------------
  output$plot_6_2_hazard_other = renderPlot(
    res=80, width = 1200, height =1600,
    {
      
      
      p6_2 = plot_harzard(data = data_hazard,
                          age= "no",
                          Pvalue_Threshold=input$slider_6_1_pvalue,
                          Effect_Direction = input$radio_6_2_effect_direction)
      
      return(p6_2)
      
    })
  
# 7. Page: Life Years Lost------------------------------------------------------

# 7.1 plot life years lost forest plot -----------------------------------------
  
    output$plot_7_1_lly_forest = renderPlot(
      res=80,
      {
    
    p7_1 = plot_lly_forest(data=data_lly,
                           CLD=input$picker_7_1_cld,
                           CVD_group=input$picker_7_2_CVD_group)
    
    return(p7_1)
  })
    
# 7.2 plot life years lost per gender ------------------------------------------
  
  output$plot_7_2_lly_gender = renderPlot(
    res=80,
    {
      
      p7_2 = plot_lly_gender(data=data_lly,
                             CLD=input$picker_7_1_cld)
      
      return(p7_2)
    })

# 7.3 plot life years lost radar -----------------------------------------------
    
    output$plot_7_3_1_lly_radar.30 = renderPlot(
      res=80,
      {
      
      p7_3_1 = plot_lly_radar(data=data_lly,
                             CLD=input$picker_7_1_cld,
                            Age_Group=30)
      
      return(p7_3_1)
    })
  
    output$plot_7_3_2_lly_radar.40 = renderPlot(
      res=80,
      {
      
      p7_3_2 = plot_lly_radar(data=data_lly,
                              CLD=input$picker_7_1_cld,
                              Age_Group=40)
      
      return(p7_3_2)
    })
    
    output$plot_7_3_3_lly_radar.50 = renderPlot(
      res=80,
      {
      
      p7_3_3 = plot_lly_radar(data=data_lly,
                              CLD=input$picker_7_1_cld,
                              Age_Group=50)
      
      return(p7_3_3)
    })
    
    output$plot_7_3_4_lly_radar.60 = renderPlot(
      res=80,
      {
      
      p7_3_4 = plot_lly_radar(data=data_lly,
                              CLD=input$picker_7_1_cld,
                              Age_Group=60)
      
      return(p7_3_4)
    })
    
    output$plot_7_3_5_lly_radar.70 = renderPlot(
      res=80,
      {
      
      p7_3_5 = plot_lly_radar(data=data_lly,
                              CLD=input$picker_7_1_cld,
                              Age_Group=70)
      
      return(p7_3_5)
    })
    
    output$plot_7_3_6_lly_radar.80 = renderPlot(
      res=80,
      {
      
      p7_3_6 = plot_lly_radar(data=data_lly,
                              CLD=input$picker_7_1_cld,
                              Age_Group=80)
      
      return(p7_3_6)
    })
    
# --- END: server function -----------------------------------------------------  
}) 
