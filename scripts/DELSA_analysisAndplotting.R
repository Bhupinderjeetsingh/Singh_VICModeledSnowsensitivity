########################################################################
### Author: Bhupinderjeet Singh
### Contact: bhupinderjeet.singh@wsu.edu
### Creation date: 10/15/2024
### Purpose: This code is for analyzing the DELSA sensitivity values
### Dependencies: Flow direction file ; set path
### This expecting the file format used with routing model for VIC flows where the first 6 lines have metadata
###


# set the file path to directory containing data and codes 
file_path_results <- "C:/Users/bhupinderjeet.singh/OneDrive - Washington State University (email.wsu.edu)/WSU/Research Proposal/Paper/Sensitivity_analysis/JournalOfHydrology/Proof_read/Codes"

setwd(file_path_results)


## list the directories containing the results for cumulative frequency distribution (cfd) 
## for both peak SWE and snow duration
cfd_results_folder <- list.dirs("./data/CFD_DELSA_firstOrderSensitvity",full.names = T,recursive = F)


#######################################################
##
##  get the cummulative frequency distribution plot for 
##  the snow metrics
##
#######################################################

for(cfd_metric in cfd_results_folder){
  
  metric_name <- str_split(cfd_metric,pattern = "\\/") %>% .[[1]] %>% .[4]
  
  ## list of the files with CFD results for each base run simulation
  ## each file has DELSA sensitvity for individual parameter
  cfd_results_all_parameters <- list.files(cfd_metric,full.names = T,recursive = F)
  
  
  ## concatenate all the 100 base simulations into one
  df_cfd_all_parameters <- do.call(rbind,lapply(cfd_results_all_parameters,read.csv)) %>% 
    mutate(Parameter_abb = case_when(parameter_name == "LAI_ADJUSTMENT" ~ "LAI",
                                     parameter_name == "MIN_RAIN_TEMP" ~ "Trmin",
                                     parameter_name == "MAX_SNOW_TEMP" ~ "Tsmax",
                                     parameter_name == "NEW_SNOW_ALB" ~ "NSA",
                                     parameter_name == "SNOW_ALB_ACCUM_A" ~ "SACC_A",
                                     parameter_name == "SNOW_ALB_ACCUM_B" ~ "SACC_B",
                                     parameter_name == "SNOW_ALB_THAW_A" ~ "STHA_A",
                                     parameter_name == "SNOW_ALB_THAW_B" ~ "STHA_B",
                                     parameter_name == "LIQUID_WATER_CAPACITY" ~ "LWC",
                                     parameter_name == "Z0SNOW" ~ "Z0SNOW",
                                     parameter_name == "NEW_SNOW_DENSITY" ~ "NSD"))
  
  
  # Make zeros print as "0" always as 0 was printed as 0.00
  prettyZero <- function(l){
    max.decimals = max(nchar(str_extract(l, "\\.[0-9]+")), na.rm = T)-1
    lnew = formatC(l, replace.zero = T, zero.print = "0",
                   digits = max.decimals, format = "f", preserve.width=T)
    return(lnew)
  }
  
  # Get unique parameter names
  unique_parameters <- unique(df_cfd_all_parameters$parameter_name)
  
  
  # Loop through each parameter and create individual plots
  for (param in unique_parameters) {
    # Filter data for the current parameter
    data_filtered <- subset(df_cfd_all_parameters, parameter_name == param)
    
    # Create ggplot for the current parameter
    plot_cfd_individual_param <- ggplot(data_filtered, aes(x = Value, y = Cummulative_frequency, group = simulation_number)) +
      geom_line(color = "red", alpha = 0.3) +
      scale_x_continuous(labels = prettyZero) +
      labs(x = "First order DELSA sensitvity index", y = "Cumulative Frequency") +
      theme_bw() +
      ylim(c(0,1))+
      theme(
        axis.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black"),
        axis.title.y.left = element_text(size = 14, colour = "black"),
        legend.position = "none",
        strip.text = element_text(size = 16, colour = "black", margin = margin(0.2, 0, 0.2, 0, unit = "cm"))
      )
    
    # Save individual plots as JPEG files
    jpeg(filename = paste("./plots/CFD_plots/",metric_name,"/",param, ".jpeg", sep = ""), bg = "white", width = 1100, height = 800, pointsize = 12, res = 300, quality = 100)
    print(plot_cfd_individual_param)
    dev.off()
  }
  
  
  # Plot cumulative frequency distribution
  cfd_plot_all_parameters_combined <- ggplot(df_cfd_all_parameters, aes(x = Value, y = Cummulative_frequency,
                                                               group = simulation_number)) +
    geom_line(color = "red",alpha = 0.3)+
    scale_x_continuous(labels = prettyZero)+
    labs(x = "First order DELSA sensitvity index", y = "Cumulative Frequency") +
    theme_bw()+
    theme(axis.text.x = element_text(size = 14,colour = "black"),
          axis.text.y = element_text(size = 14,colour = "black"),
          axis.title.x = element_text(size = 16,colour = "black"),
          axis.title.y.left = element_text(size = 16,colour = "black"),
          legend.position = "none",
          strip.text =element_text(size = 16,colour = "black",margin = margin(0.2,0,0.2,0,unit = "cm")) )+
    facet_wrap(. ~ parameter_name, ncol  = 4) +
    jpeg(filename=paste("./plots/CFD_plots/",metric_name,"/",metric_name,".jpeg", sep=""), bg="white", width=4000, height=2300, pointsize=12, res=300, quality=100)
  plot(cfd_plot_all_parameters_combined)
  dev.off()
  
  ## compute the area above curve (aac) for each cfd curve 
  
  df_AreaAboveCurve <- df_cfd_all_parameters %>% 
    mutate(Target_metric = metric_name) %>% 
    drop_na() %>% 
    as.data.frame() %>% 
    group_by(simulation_number,Parameter_abb,Target_metric) %>%
    summarise(AUC_param = DescTools::AUC(Value,Cummulative_frequency),
              AAC_param = (max(Value,na.rm = T)-AUC_param)) 
  
  boxplot_aac <- ggplot(df_AreaAboveCurve, aes(x = factor(Parameter_abb), y = abs(AAC_param), fill = Target_metric)) +
    geom_boxplot(alpha = .5)+
    stat_boxplot(geom='errorbar')+
    scale_fill_manual(values = c("goldenrod2", "forestgreen"))+
    theme_bw()+
    scale_y_continuous()+
    labs(y = "Area above curve")+
    theme(axis.text.x = element_text(size = 14,colour = "black",angle = 90,vjust = 0.5,hjust = 1),
          axis.text.y = element_text(size = 14,colour = "black"),
          axis.title.y.left=element_text(size = 14,colour = "black"),
          axis.title.x = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text =element_text(size = 10,face = "bold",colour = "black"),
          legend.background = element_blank(),
          strip.text = element_text(size = 12,colour = "black"))+
    jpeg(filename=paste("./plots/AAC_",metric_name , ".jpeg", sep=""), bg="white", width=3000, height=1000, pointsize=12, res=300, quality=100)
  plot(boxplot_aac)
  dev.off()
  
  
  
  
}

