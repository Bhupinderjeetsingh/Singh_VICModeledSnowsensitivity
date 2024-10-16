# Singh_VICModeledSnowSensitivity
 This repository contains the processed data and scripts used in the study titled:   **"Sensitivity of Snow Magnitude and Duration to Hydrology Model Parameters."**

# Manuscript: Sensitivity of Snow Magnitude and Duration to Hydrology Model Parameters  
### Authors:  
- Bhupinderjeet Singh (bhupinderjeet.singh@wsu.edu)  
- Kirti Rajagopalan (kirtir@wsu.edu)  
- **Date**: October 15, 2024  
---

## Overview

This repository contains the processed data and scripts used in the study titled:  
**"Sensitivity of Snow Magnitude and Duration to Hydrology Model Parameters."**

The repository is organized into three main folders: `data`, `scripts`, and `plots`. Each folder contains specific files related to the analysis, results, and visualizations used in the manuscript.

---

## Folder Structure

### 1. **data**

This folder contains all the necessary processed data files for the study, including parameter files, sensitivity analyses, and processed data for model simulations. Due to the large size of the raw data files, they have not been uploaded here. For access to the raw data, please contact the authors.

- **parameterfiles**  
  Contains the complete parameter sample used for model simulations.

- **CFD_DELSA_firstOrderSensitivity**  
  This folder includes the cumulative frequency distribution (CFD) of both snow metrics: **peak SWE** (Snow Water Equivalent) and **snow duration**.  
  - Each file contains two columns:
    - `value`: The DELSA First Order Sensitivity (FOS) values.
	- `CumulativeFrequency`: grids that exhibit the given FOS values.
    - `Cumulative_frequency`: Percentage of total grids that exhibit the given FOS values.
	- `simulation_number`: base run simulation number.
	- `parameter_name`: name of the parameter.

- **processed_data**  
  Contains multiple files with data derived from the simulations:
  
  1. **DELSA_kmeans_clusters**  
     - Includes the latitude and longitude of each grid in the study area and the corresponding cluster assignment.  
     - Clusters are based on the sensitivity of peak SWE to model parameters.
  
  2. **EDA_features**  
     - Contains the values of the features used in the **XGBoost classifier** to explain the sensitivity response.  
     - More details are available in Supplementary Table S1 of the manuscript.
  
  3. **FOS_max_SWE**  
     - Provides the DELSA First Order Sensitivity values for each grid-parameter combination and the baseline simulation.
     - Since the file is large, please unzip it to extract the CSV file.
  
  4. **PCA_clustering_input**  
     - Contains the top 200 principal components explaining 85% of the total variance.  
     - These components were used as input for the **k-means clustering** algorithm.
     - Since the file is large, please unzip it to extract the CSV file.
  
  5. **PrecipRunoffTemp_median**  
     - Median values of monthly precipitation ratio, runoff ratio, and average temperature for each class (cluster).  
     - The median is computed across all grids within each class.

---

### 2. **scripts**

This folder contains all the Python scripts used to process the data, perform analyses, and generate visualizations.

- **DELSA_analysisAndPlotting.R**  
  - Analyzes the cumulative frequency distribution (CFD) of DELSA FOS values and calculates the **area above the curve** for each simulation and parameter.
  
- **clusteringPlots_and_firstOrderSensitivityVariations.R**  
  - Generates spatial maps of k-means clusters and visualizes the variation in DELSA FOS for each parameter across identified clusters.
  
- **kmeansClustering_and_SHAPanalysis.py**  
  - Python script for performing **k-means clustering**, followed by **XGBoost classification** and **SHAP analysis** to interpret model outputs.

---

### 3. **plots**

This folder contains all the plots generated for the manuscript, including visualizations related to the sensitivity analysis and clustering results.

- **CFD_plots**  
  - A sub-folder containing the individual **cumulative frequency distribution** plots for each parameter, used to illustrate DELSA FOS for the study.

---

### How to Use

1. **Data Files**:  
   Navigate to the `data` folder to access the processed files used in the analysis.
   
2. **Scripts**:  
   The `scripts` folder contains scripts that can be run to reproduce the analysis and generate visualizations. Ensure you have the required R and Python packages installed.

3. **Plots**:  
   All visualizations used in the manuscript are located in the `plots` folder. You can find specific cumulative frequency distribution plots under `CFD_plots`.

---
