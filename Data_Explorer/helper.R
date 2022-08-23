#
# Data wrangling scripts for Shiny App: "CVD in Liver Disease Data Explorer"
#

# --- Preface ------------------------------------------------------------------
source("global.R")


# --- 2. Incidence analysis ----------------------------------------------------

# load asir data
load_data_ASIR = function(){
  
  data_asir  = read_csv("data/data_ASIR.csv")
  return(data_asir)
}

# load UK map data
load_region_map_data = function(){
  
  prac_region_map = read_csv("data/S02.prac_region_map_data.sparse_60_small.2021-05-04.csv.gz")
  return(prac_region_map)
}

# plot asir freqs with error bars
plot_asir_freq = function(data, title=""){
   
  out = data %>% 
    ggplot(aes(x = reorder(Event,ASIR_per_100k),
                y = ASIR_per_100k,
                ymin = ASIR_ci_lower,
                ymax = ASIR_ci_upper,
                color = Event)) +
    geom_point(size=rel(4)) +
    geom_line() +
    geom_errorbar(width=0.1,
                  size=rel(2))+
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = c(cld_cols, cvd_cols_dark))+
    theme_freq +
    labs(y="Age-standardised incidence rate per\n100,000 person years",
         x="",
         title=title)
  
  return(out)

}

# plot asir map
plot_asir_map = function(data, map = prac_region_map){

  # first combine shape data with input values
  plot_data = map %>% 
    left_join(data, by="prac_region")
  
  # plot UK map
  out = plot_data %>% 
    ggplot(aes(x= long,
               y= lat,
               group = group,
               fill= value),
           color=NA) +
    geom_polygon() +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #set aspect ratio for plot
    coord_fixed(1)
  
  return(out)
}

# --- 4. Comorbidities in liver disease ----------------------------------------

# load non-CVD comorb data
load_data_non_CVD_comorb= function(){
  out = read_csv("data/data_non_CVD_comorb.csv")
  return(out)
}

# plot percentage non CVD comorbidites per age group
plot_comorb_non_CVD = function(data, 
                               CLD=c("ALD",
                                     "Autoimmune liver disease",
                                     "HBV",
                                     "HCV",
                                     "NAFLD"),
                               Comorb=names(non_cvd_comorb_cols),
                               plotGender=c("Men", "Women")){
  
  plot = data %>% 
    filter(age_group != "all") %>%
    filter(gender == plotGender) %>% 
    mutate(percent = counts/total_number*100) %>% 
    filter(cld_type_clean %in% CLD) %>% 
    filter(comorb_clean_name %in% Comorb) %>% 
    mutate(CLD_label = ifelse(cld_type_clean=="Autoimmune liver disease",
                              "Autoimmune\nliver\ndisease", cld_type_clean)) %>%
    mutate(Comorb_label = str_replace_all(string = comorb_clean_name,
                                          pattern = " ",
                                          replacement = "\n") %>% 
             str_replace(., pattern = "-", replacement = "-\n")) %>% 
    ggplot(aes(x = age_group,
               y = percent,
               fill = comorb_clean_name)) +
    geom_bar(stat="identity",width=0.7) + 
    facet_grid(Comorb_label~ CLD_label, scales = "free") +
    scale_fill_manual(values=non_cvd_comorb_cols) +
    labs(x = "Age (years)",
         y = "Proportion of individuals with prevalent comorbidities (%)",
         fill="",
         title="") +
    theme_freq +
    theme(axis.text.x = element_text(angle=90)) +
    guides(fill=guide_legend(ncol=3,byrow=FALSE))
  
  return(plot)
  
}

# --- 5. Page: Initial CVD presentation ----------------------------------------

# load CVD comorb data
load_data_CVD_comorb = function(){
  out = read_csv("data/data_CVD_comorb.csv")
  return(out)
}


# plot binary has/has-not CVD, gender combined
plot_comorb_CVD_yes_no.all_gender = function(data,
                                  CLD=c("ALD",
                                        "Autoimmune liver disease",
                                        "HBV",
                                        "HCV",
                                        "NAFLD")){
  
  plot = data %>% 
    filter(age_group=="all") %>%
    filter(gender =="all") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    mutate(has_CVD = ifelse(condition_cvd_clean_name=="None", "No", "Yes")) %>% 
    group_by(cld_type_clean, has_CVD) %>% 
    summarise(counts = sum(counts),
              total_number = max(total_number)) %>% 
    mutate(percent = counts/total_number*100) %>% 
    mutate(ylabel = ifelse(has_CVD=="Yes", 10, 80)) %>% 
    ggplot(aes(x=cld_type_clean,
               y=percent,
               fill=has_CVD))+
    geom_bar(stat="identity",width=0.7, color="black") +
    geom_text(aes(color = has_CVD,
                  y = ylabel,
                  label = sprintf("%0.0f", round(percent, digits = 0),
                                  group = has_CVD)),
              fontface = "bold") +
    scale_fill_manual(values=c("Yes"="black","No"="white")) +
    scale_color_manual(values=c("Yes"="white", "No"="black"), guide=F) +
    coord_flip() +
    theme_freq +
    labs(fill="has CVD", y="Precentage of Patients",x="")
  
  return(plot)
  
}

# plot binary has/has-not CVD, per gender
plot_comorb_CVD_yes_no.per_gender = function(data,
                                             CLD=c("ALD",
                                                   "Autoimmune liver disease",
                                                   "HBV",
                                                   "HCV",
                                                   "NAFLD")){
  
  plot = data %>% 
    filter(age_group=="all") %>%
    filter(gender !="all") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    mutate(has_CVD = ifelse(condition_cvd_clean_name=="None", "No", "Yes")) %>% 
    group_by(cld_type_clean, has_CVD,gender) %>% 
    summarise(counts = sum(counts),
              total_number = max(total_number)) %>% 
    mutate(percent = counts/total_number*100) %>% 
    mutate(ylabel = ifelse(has_CVD=="Yes", 10, 80)) %>% 
    ggplot(aes(x=cld_type_clean,
               y=percent,
               fill=has_CVD))+
    geom_bar(stat="identity",width=0.7, color="black") +
    geom_text(aes(color = has_CVD,
                  y = ylabel,
                  label = sprintf("%0.0f", round(percent, digits = 0),
                                  group = has_CVD)),
              fontface = "bold") +
    scale_fill_manual(values=c("Yes"="black","No"="white")) +
    scale_color_manual(values=c("Yes"="white", "No"="black"), guide=F) +
    coord_flip() +
    theme_freq +
    labs(fill="has CVD", y="Precentage of Patients",x="") +
    facet_grid(gender~.) 
  
  return(plot)
  
}

# plot percentage intitial CVD COMBINED presentation percentage all gender
plot_comorb_CVD_comb.all_gender = function(data, 
                                            CLD=c("ALD",
                                                  "Autoimmune liver disease",
                                                  "HBV",
                                                  "HCV",
                                                  "NAFLD"),
                                            CVD_comp=names(cvd_cols_dark[1:5])){
  
  data %>% 
    filter(age_group == "all") %>%
    filter(gender =="all") %>% 
    filter(cvd_composite_group!="None") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    group_by(cld_type_clean,cvd_composite_group) %>% 
    summarise(counts = sum(counts),
              total_number = max(total_number)) %>% 
    ungroup() %>% 
    group_by(cld_type_clean) %>% 
    summarise(cvd_composite_group =factor(cvd_composite_group),
              counts = counts,
              total_number = total_number,
              total_number_with_CVD = sum(counts)) %>% 
    mutate(percent = counts/total_number_with_CVD*100) %>% 
    filter(cvd_composite_group %in% CVD_comp) %>% 
    arrange(desc(cvd_composite_group)) %>% 
    mutate(ylabel.tmp = cumsum(percent)) %>% 
    mutate(ylabel = cumsum(percent) - 0.5 * percent) %>% 
    ggplot(aes(x=cld_type_clean,
               y=percent,
               fill=cvd_composite_group)) + 
    geom_bar(stat="identity",width=0.7) +
    geom_text(aes(y = ylabel,
                  label = sprintf("%0.0f", round(percent, digits = 0),
                                  group = cld_type_clean)),
              fontface = "bold",
              color = "white") +
    scale_fill_manual(values=cvd_cols_dark) +
    guides(fill=guide_legend(byrow = FALSE,ncol=3)) +
    coord_flip() +
    theme_freq +
    ylim(0,101) +
    labs(y="Initial presentation of CVD in patients with liver disease",
         x="",
         fill="")
}

# plot percentage intitial CVD COMBINED presentation percentage all gender
plot_comorb_CVD_comb.per_gender = function(data, 
                                           CLD=c("ALD",
                                                 "Autoimmune liver disease",
                                                 "HBV",
                                                 "HCV",
                                                 "NAFLD"),
                                           CVD_comp=names(cvd_cols_dark[1:5])){
  
  data %>% 
    filter(age_group == "all") %>%
    filter(gender !="all") %>% 
    filter(cvd_composite_group!="None") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    group_by(cld_type_clean,cvd_composite_group,gender) %>% 
    summarise(counts = sum(counts),
              total_number = max(total_number)) %>% 
    ungroup() %>% 
    group_by(cld_type_clean,gender) %>% 
    summarise(cvd_composite_group =factor(cvd_composite_group),
              counts = counts,
              total_number = total_number,
              total_number_with_CVD = sum(counts)) %>% 
    mutate(percent = counts/total_number_with_CVD*100) %>% 
    filter(cvd_composite_group %in% CVD_comp) %>% 
    arrange(desc(cvd_composite_group)) %>% 
    mutate(ylabel.tmp = cumsum(percent)) %>% 
    mutate(ylabel = cumsum(percent) - 0.5 * percent) %>% 
    ggplot(aes(x=cld_type_clean,
               y=percent,
               fill=cvd_composite_group)) + 
    geom_bar(stat="identity",width=0.7) +
    geom_text(aes(y = ylabel,
                  label = sprintf("%0.0f", round(percent, digits = 0),
                                  group = cld_type_clean)),
              fontface = "bold",
              color = "white") +
    scale_fill_manual(values=cvd_cols_dark) +
    guides(fill=guide_legend(byrow = FALSE,ncol=3)) +
    coord_flip() +
    theme_freq +
    ylim(0,101) +
    facet_grid(gender~.)+
    labs(y="Initial presentation of CVD in patients with liver disease",
         x="",
         fill="")
  
}


# plot percentage intitial CVD INDIVIDUAL presentation percentage all gender
plot_comorb_CVD_indiv.all_gender = function(data, 
                      CLD=c("ALD",
                                "Autoimmune liver disease",
                                "HBV",
                                "HCV",
                                "NAFLD"),
                      CVD=names(cvd_indiv_cols)){
  
  data %>% 
    filter(age_group == "all") %>%
    filter(gender =="all") %>% 
    filter(condition_cvd_clean_name!="None") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    group_by(cld_type_clean) %>% 
    summarise(condition_cvd_clean_name=factor(condition_cvd_clean_name),
              counts = counts,
              total_number = total_number,
              total_number_with_CVD = sum(counts)) %>% 
    mutate(percent = counts/total_number_with_CVD*100) %>% 
    filter(condition_cvd_clean_name %in% CVD) %>% 
    arrange(desc(condition_cvd_clean_name)) %>% 
    mutate(ylabel.tmp = cumsum(percent)) %>% 
    mutate(ylabel = cumsum(percent) - 0.5 * percent) %>% 
    ggplot(aes(x=cld_type_clean,
               y=percent,
               fill=condition_cvd_clean_name)) + 
    geom_bar(stat="identity",width=0.7) +
    geom_text(aes(y = ylabel,
                  label = sprintf("%0.0f", round(percent, digits = 0),
                                  group = cld_type_clean)),
              fontface = "bold",
              color = "white") +
    scale_fill_manual(values=cvd_indiv_cols) +
    guides(fill=guide_legend(byrow = FALSE,ncol=3)) +
    coord_flip() +
    theme_freq +
    ylim(0,101) +
    labs(y="Initial presentation of CVD in patients with liver disease",
         x="",
         fill="")
}

# plot percentage intitial CVD INDIVIDUAL presentation percentage per gender
plot_comorb_CVD_indiv.per_gender = function(data, 
                                            CLD=c("ALD",
                                                  "Autoimmune liver disease",
                                                  "HBV",
                                                  "HCV",
                                                  "NAFLD"),
                                            CVD=names(cvd_indiv_cols)){
  
  data %>% 
    filter(age_group == "all") %>%
    filter(gender !="all") %>% 
    filter(condition_cvd_clean_name!="None") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    group_by(cld_type_clean, gender) %>% 
    summarise(condition_cvd_clean_name=factor(condition_cvd_clean_name),
              counts = counts,
              total_number = total_number,
              total_number_with_CVD = sum(counts)) %>% 
    mutate(percent = counts/total_number_with_CVD*100) %>% 
    filter(condition_cvd_clean_name %in% CVD) %>% 
    arrange(desc(condition_cvd_clean_name)) %>% 
    mutate(ylabel.tmp = cumsum(percent)) %>% 
    mutate(ylabel = cumsum(percent) - 0.5 * percent) %>% 
    ggplot(aes(x=cld_type_clean,
               y=percent,
               fill=condition_cvd_clean_name)) + 
    geom_bar(stat="identity",width=0.7) +
    geom_text(aes(y = ylabel,
                  label = sprintf("%0.0f", round(percent, digits = 0),
                                  group = cld_type_clean)),
              fontface = "bold",
              color = "white") +
    scale_fill_manual(values=cvd_indiv_cols) +
    guides(fill=guide_legend(byrow = FALSE,ncol=3)) +
    coord_flip() +
    theme_freq +
    ylim(0,101) +
    facet_grid(gender~.)+
    labs(y="Initial presentation of CVD in patients with liver disease",
         x="",
         fill="")
}

# plot percentage intitial group CVD per age group
plot_comorb_CVD_comb.per_age = function(data, 
                                            CLD=c("ALD",
                                                  "Autoimmune liver disease",
                                                  "HBV",
                                                  "HCV",
                                                  "NAFLD"),
                                        CVD_comp=names(cvd_cols_dark[1:5])){
  
  plot.data = data_CVD_comorb %>% 
    filter(gender!="all") %>% 
    filter(age_group!="all") %>% 
    group_by(cld_type_clean, cvd_composite_group, age_group, gender) %>% 
    summarise(count = sum(counts),
              total_number = max(total_number)) %>% 
    mutate(percentage_strata = count/total_number*100) %>% 
    ungroup() %>% 
    filter(cvd_composite_group!="None") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    filter(cvd_composite_group %in% CVD_comp)
  
  gender_label = data.frame(
    label=c("Women", "Men"),
    age_group=c("30 - 39","30 - 39"),
    y = c(-63,35)
  )
  
  plot = plot.data %>%    
    ggplot(aes(x=age_group)) +
    geom_bar(data=plot.data[plot.data$gender=="Men",],
             aes(y=percentage_strata,
                 fill=cvd_composite_group), stat="identity") +
    geom_bar(data=plot.data[plot.data$gender=="Women",],
             aes(y=-percentage_strata,
                 fill=cvd_composite_group), stat="identity") +
    geom_hline(yintercept=0, colour="white", lwd=1) +
    geom_label(data=gender_label, aes(label=label, 
                                      y=y),
               hjust="left", fill="grey90") + 
    coord_flip(ylim=c(-65,65)) + 
    scale_y_continuous(breaks=seq(-60,60,20), labels=c(60,40,20,0,20,40,60)) +
    labs(y="Proportion of individuals with incident CVD (%)",
         x="Age (years)",
         fill="") +
    facet_grid(.~cld_type_clean) +
    scale_fill_manual(values = cvd_cols_dark) +
    guides(fill=guide_legend(byrow = FALSE,ncol=3)) +
    theme_freq
  
  return(plot)
  
}

# plot percentage intitial individual CVD per age group
plot_comorb_CVD_indiv.per_age = function(data, 
                                        CLD=c("ALD",
                                              "Autoimmune liver disease",
                                              "HBV",
                                              "HCV",
                                              "NAFLD"),
                                        CVD=names(cvd_indiv_cols)){
  
  plot.data = data_CVD_comorb %>% 
    filter(gender!="all") %>% 
    filter(age_group!="all") %>% 
    group_by(cld_type_clean, condition_cvd_clean_name, age_group, gender) %>% 
    summarise(count = sum(counts),
              total_number = max(total_number)) %>% 
    mutate(percentage_strata = count/total_number*100) %>% 
    ungroup() %>% 
    filter(condition_cvd_clean_name!="None") %>% 
    filter(cld_type_clean %in% CLD) %>% 
    filter(condition_cvd_clean_name %in% CVD)
  
  gender_label = data.frame(
    label=c("Women", "Men"),
    age_group=c("30 - 39","30 - 39"),
    y = c(-63,35)
  )
  
  plot = plot.data %>%    
    ggplot(aes(x=age_group)) +
    geom_bar(data=plot.data[plot.data$gender=="Men",],
             aes(y=percentage_strata,
                 fill=condition_cvd_clean_name), stat="identity") +
    geom_bar(data=plot.data[plot.data$gender=="Women",],
             aes(y=-percentage_strata,
                 fill=condition_cvd_clean_name), stat="identity") +
    geom_hline(yintercept=0, colour="white", lwd=1) +
    geom_label(data=gender_label, aes(label=label, 
                                      y=y),
               hjust="left", fill="grey90") + 
    coord_flip(ylim=c(-65,65)) + 
    scale_y_continuous(breaks=seq(-60,60,20), labels=c(60,40,20,0,20,40,60)) +
    labs(y="Proportion of individuals with incident CVD (%)",
         x="Age (years)",
         fill="") +
    facet_grid(.~cld_type_clean) +
    scale_fill_manual(values = cvd_indiv_cols) +
    guides(fill=guide_legend(byrow = FALSE,ncol=3)) +
  theme_freq
  
  return(plot)
  
}



# --- 6. Hazard Ratios ---------------------------------------------------------

# load hazard data
load_hazard_data = function(){
  
  out = read_csv("data/data_cox_hr.csv")
  return(out)
}
  
# plot hazard ratios
plot_harzard = function(data,
                        age= c("yes","no"),
                        Pvalue_Threshold=1,
                        Effect_Direction = c("All","Predisposing", "Protective")){
  
  # are we plotting age groups or all other risk factors?
  if(age == "yes"){
    plot.data = data %>% 
      filter(Covariate_is_Age)
    
    plot.ratio=1
    
  } else {
    plot.data = data %>% 
      filter(!Covariate_is_Age)
    
    plot.ratio=5
  }
  
  # create new variabel for grouping of alpha selection according to 
  # input selection for PValue threshold and effect directions
  if (Effect_Direction =="All"){
    
    plot.data.alpha = plot.data %>% 
      mutate(Alpha_highlight = ifelse(P_numeric<=Pvalue_Threshold, "YES", "NO"))
    
  } else if (Effect_Direction == "Predisposing"){
    
    plot.data.alpha = plot.data %>% 
      mutate(Alpha_highlight = ifelse(P_numeric<=Pvalue_Threshold & is_predisposing, "YES", "NO"))
    
  } else if (Effect_Direction == "Protective"){
    
    plot.data.alpha = plot.data %>% 
      mutate(Alpha_highlight = ifelse(P_numeric<=Pvalue_Threshold & is_protective, "YES", "NO"))
    
  }
  
  # plot 
  plot = plot.data.alpha %>% 
    mutate(cld_label = str_replace_all(string = cld_type, pattern = " ",
                                       replacement = "\n")) %>% 
    ggplot(aes(x=factor(clean_names, levels=unique(clean_names)),
               y=HR, ymin=lower.ci, ymax=upper.ci,
               color=cld_type,
               alpha= Alpha_highlight)) +
    geom_linerange(size=1) +
    facet_grid(~cld_label, scales="free") +
    geom_hline(aes(x=0, yintercept=1), lty=1, size=0.2) + 
    geom_point(size=2, shape=15, stroke=0.5) +
    coord_flip() + 
    scale_color_manual(values=cld_cols) + 
    scale_alpha_manual(values=c("YES"=1,"NO"=0.2), guide=F) +
    scale_y_log10() +
    labs(y="Hazard ratio",
         x="",
         color="",
         alpha="") + 
    theme_freq

  return(plot)
}  
  

# --- 7. Life Years Lost--------------------------------------------------------

# load life years lost data
load_lyl_data = function(){
  
  out = read_csv("data/data_lly.csv")
  return(out)
}

# plot forest lly plots
plot_lly_forest = function(data,
                          CLD=c("ALD",
                                "Autoimmune liver disease",
                                  "HBV",
                                  "HCV",
                                  "NAFLD"),
                          CVD_group=names(cvd_cols_dark)){
  
  plot = data %>% 
    filter(gender=="all") %>% 
    filter(outcome %in% CVD_group) %>% 
    filter(reference_population_clean %in% CLD) %>% 
    ggplot(aes(y=lyl_estimate.TotalLYL,
               x=age_disease_onset,
               color=reference_population_clean)) + 
    geom_line(size =1) +
    geom_point(size = 3.5) + 
    geom_errorbar(aes(ymin=lyl_ci_left.TotalLYL,
                      ymax=lyl_ci_right.TotalLYL),
                  width=3, size =1,
                  position=position_dodge(0.05)) + 
    scale_color_manual(values = cld_cols) + 
    labs(x="Age of cardiovascular disease onset (years)",
         y="Excess life years lost (years)",
         color="") +
    facet_grid(~outcome)+
    theme_freq
  
  return(plot)
}

# plot life years lost per gender
plot_lly_gender = function(data,
                             CLD=c("ALD",
                                   "Autoimmune liver disease",
                                   "HBV",
                                   "HCV",
                                   "NAFLD")){
  
  plot = data %>% 
    filter(gender!="all") %>% 
    filter(reference_population_clean %in% CLD) %>% 
    ggplot(aes(y=lyl_estimate.TotalLYL,
               x=age_disease_onset,
               color=gender)) + 
    geom_line(size =1) +
    geom_point(size = 3.5) + 
    geom_errorbar(aes(ymin=lyl_ci_left.TotalLYL,
                      ymax=lyl_ci_right.TotalLYL),
                  width=3, size =1,
                  position=position_dodge(0.05)) + 
    scale_color_manual(values = c("Women" ="#DF8F44FF", "Men"="#374E55")) + 
    labs(x="Age of cardiovascular disease onset (years)",
         y="Excess life years lost (years)",
         color="") +
    facet_grid(~reference_population_clean)+
    theme_freq
  
  return(plot)
}

# plot lly radar plots
plot_lly_radar = function(data,
                           CLD=c("ALD",
                                 "Autoimmune liver disease",
                                 "HBV",
                                 "HCV",
                                 "NAFLD"),
                          Age_Group){
  
  
  # create min max data for radar plot
  max_min <- tibble(
    `Arrhythmia` = c(40, -10), `Cardiomyopathy` = c(40, -10), `Coronary heart disease` = c(40, -10),
    `Peripheral vascular disease` = c(40, -10), `Stroke and TIA` = c(40, -10)
  )
  
  # create plot data
  plot_data.tibble = data %>% 
    filter(age_disease_onset==Age_Group) %>% 
    filter(outcome != "any CVD") %>% 
    filter(reference_population_clean %in% CLD) %>% 
    select(reference_population_clean,
           outcome,
           lyl_estimate.TotalLYL) %>%
    pivot_wider(values_from = lyl_estimate.TotalLYL,
                names_from = outcome) %>% 
    column_to_rownames("reference_population_clean") %>% 
    rbind(max_min,.)
  
  plot_data = data.frame(plot_data.tibble)

  colnames(plot_data) = str_replace_all(string =  colnames(plot_data),
                                        pattern = "\\.",
                                        replacement = " ") %>% 
                        str_replace_all(string = .,
                                        pattern = " disease",
                                        replacement = "\ndisease")
  
  # general radar plot function from Alvina, requires package 'fmsb'
  create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                          vlabels = colnames(data), vlcex = 0.9,
                                          caxislabels = NULL, title = NULL, ...){
    radarchart(
      data, axistype = 1,
      # Customize the polygon
      pcol = color,
      pfcol = scales::alpha(color, 0.1),
      #pfcol=NA,
      plwd = 2, plty = 1, seg = 5,
      # Customize the grid
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      # Customize the axis
      axislabcol = "grey", 
      # Variable labels
      vlcex = vlcex, vlabels = vlabels,
      caxislabels = caxislabels, title = title,
      palcex = 5
    )
  }
  
  # create radar plot
  plot = create_beautiful_radarchart(
    data = plot_data, 
    caxislabels = c(-10,0,10,20,30,40),
    color = cld_cols[which(names(cld_cols) %in% CLD)],
    title = paste0("Age ", Age_Group)
  )
  
  return(plot)
}



# --- Load input data ----------------------------------------------------------

# asir
data_asir = load_data_ASIR()
prac_region_map = load_region_map_data()

# comorb
data_CVD_comorb = load_data_CVD_comorb()
data_non_CVD_comorb = load_data_non_CVD_comorb()

# hazard ratios
data_hazard = load_hazard_data()

# life years lost
data_lly = load_lyl_data()


