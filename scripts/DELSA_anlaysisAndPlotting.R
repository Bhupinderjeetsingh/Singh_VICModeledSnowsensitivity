########################################################################
### Author: Bhupinderjeet Singh
### Contact: bhupinderjeet.singh@wsu.edu
### Creation date: 10/15/2024
### Purpose: Analyze DELSA sensitivity values and generate plots
########################################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)



# Set the working directory to the folder containing the results and code
file_path_results <- "C:/Users/bhupinderjeet.singh/OneDrive - Washington State University (email.wsu.edu)/WSU/Research Proposal/Paper/Sensitivity_analysis/JournalOfHydrology/Proof_read/Codes"
setwd(file_path_results)

# List the directories containing cumulative frequency distribution (CFD) results
# for peak SWE and snow duration metrics
cfd_results_folder <- list.dirs("./data/CFD_DELSA_firstOrderSensitvity", full.names = TRUE, recursive = FALSE)

#######################################################
# Loop through CFD results to analyze snow metrics
#######################################################

for (cfd_metric in cfd_results_folder) {
  
  # Extract the metric name from the folder path
  metric_name <- str_split(cfd_metric, pattern = "\\/") %>% .[[1]] %>% .[4]
  
  # List the files containing CFD results for each base simulation
  # Each file holds DELSA sensitivity results for individual parameters
  cfd_results_all_parameters <- list.files(cfd_metric, full.names = TRUE, recursive = FALSE)
  
  # Concatenate data from all base simulations into one data frame
  df_cfd_all_parameters <- do.call(rbind, lapply(cfd_results_all_parameters, read.csv)) %>%
    # Create a short abbreviation for parameter names
    mutate(Parameter_abb = case_when(
      parameter_name == "LAI_ADJUSTMENT" ~ "LAI",
      parameter_name == "MIN_RAIN_TEMP" ~ "Trmin",
      parameter_name == "MAX_SNOW_TEMP" ~ "Tsmax",
      parameter_name == "NEW_SNOW_ALB" ~ "NSA",
      parameter_name == "SNOW_ALB_ACCUM_A" ~ "SACC_A",
      parameter_name == "SNOW_ALB_ACCUM_B" ~ "SACC_B",
      parameter_name == "SNOW_ALB_THAW_A" ~ "STHA_A",
      parameter_name == "SNOW_ALB_THAW_B" ~ "STHA_B",
      parameter_name == "LIQUID_WATER_CAPACITY" ~ "LWC",
      parameter_name == "Z0SNOW" ~ "Z0SNOW",
      parameter_name == "NEW_SNOW_DENSITY" ~ "NSD"
    ))
  
  # Function to pretty-print zero values in a plot
  prettyZero <- function(l) {
    max.decimals = max(nchar(str_extract(l, "\\.[0-9]+")), na.rm = TRUE) - 1
    lnew = formatC(l, replace.zero = TRUE, zero.print = "0", digits = max.decimals, format = "f", preserve.width = TRUE)
    return(lnew)
  }
  
  # Get a list of unique parameters from the data
  unique_parameters <- unique(df_cfd_all_parameters$parameter_name)
  
  # Loop through each parameter to create individual CFD plots
  for (param in unique_parameters) {
    # Filter the data for the current parameter
    data_filtered <- subset(df_cfd_all_parameters, parameter_name == param)
    
    # Create the CFD plot for the current parameter using ggplot2
    plot_cfd_individual_param <- ggplot(data_filtered, aes(x = Value, y = Cummulative_frequency, group = simulation_number)) +
      geom_line(color = "red", alpha = 0.3) +
      scale_x_continuous(labels = prettyZero) +
      labs(x = "First order DELSA sensitivity index", y = "Cumulative Frequency") +
      theme_bw() +
      ylim(c(0, 1)) +
      theme(
        axis.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black"),
        axis.title.y.left = element_text(size = 14, colour = "black"),
        legend.position = "none",
        strip.text = element_text(size = 16, colour = "black", margin = margin(0.2, 0, 0.2, 0, unit = "cm"))
      )
    
    # Save the individual plot as a JPEG file
    jpeg(filename = paste("./plots/CFD_plots/", metric_name, "/", param, ".jpeg", sep = ""), bg = "white", width = 1100, height = 800, pointsize = 12, res = 300, quality = 100)
    print(plot_cfd_individual_param)
    dev.off()
  }
  
  # Plot combined CFD for all parameters
  cfd_plot_all_parameters_combined <- ggplot(df_cfd_all_parameters, aes(x = Value, y = Cummulative_frequency, group = simulation_number)) +
    geom_line(color = "red", alpha = 0.3) +
    scale_x_continuous(labels = prettyZero) +
    labs(x = "First order DELSA sensitivity index", y = "Cumulative Frequency") +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 14, colour = "black"),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.x = element_text(size = 16, colour = "black"),
      axis.title.y.left = element_text(size = 16, colour = "black"),
      legend.position = "none",
      strip.text = element_text(size = 16, colour = "black", margin = margin(0.2, 0, 0.2, 0, unit = "cm"))
    ) +
    facet_wrap(. ~ parameter_name, ncol = 4)
  
  # Save combined plot as JPEG
  jpeg(filename = paste("./plots/CFD_plots/", metric_name, "/", metric_name, ".jpeg", sep = ""), bg = "white", width = 4000, height = 2300, pointsize = 12, res = 300, quality = 100)
  plot(cfd_plot_all_parameters_combined)
  dev.off()
  
  # Compute area above the curve (AAC) for each CFD curve
  df_AreaAboveCurve <- df_cfd_all_parameters %>%
    mutate(Target_metric = metric_name) %>%
    drop_na() %>%
    as.data.frame() %>%
    group_by(simulation_number, Parameter_abb, Target_metric) %>%
    summarise(AUC_param = DescTools::AUC(Value, Cummulative_frequency),
              AAC_param = (max(Value, na.rm = TRUE) - AUC_param))
  
  # Create a boxplot for AAC values
  boxplot_aac <- ggplot(df_AreaAboveCurve, aes(x = factor(Parameter_abb), y = abs(AAC_param), fill = Target_metric)) +
    geom_boxplot(alpha = .5) +
    stat_boxplot(geom = 'errorbar') +
    scale_fill_manual(values = c("goldenrod2", "forestgreen")) +
    theme_bw() +
    scale_y_continuous() +
    labs(y = "Area above curve") +
    theme(
      axis.text.x = element_text(size = 14, colour = "black", angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.y.left = element_text(size = 14, colour = "black"),
      axis.title.x = element_blank(),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 10, face = "bold", colour = "black"),
      legend.background = element_blank(),
      strip.text = element_text(size = 12, colour = "black")
    )
  
  # Save AAC boxplot as JPEG
  jpeg(filename = paste("./plots/AAC_", metric_name, ".jpeg", sep = ""), bg = "white", width = 3000, height = 1000, pointsize = 12, res = 300, quality = 100)
  plot(boxplot_aac)
  dev.off()
}
