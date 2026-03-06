# Analytical-Data-Science-Programmer-Coding-Assessment
Project: ADS Coding Assessment

Author: Maitri Rana
Date: March 2026

--------------------------------------------------
**Question 1 – SDTM Dataset Creation**

**Folder:** question_1_sdtm_01_create_ds_domain

**Programs:**
question_1_sdtm01_create_ds_domain.R – Creates SDTM Disposition dataset

**Input Data:**
pharmaverseraw::ds_raw 

**Required Packages:**
sdtm.oak, tidyverse, pharmaverseraw 

**Log:**
sdtm_ds_domain_logfile.txt – Execution log file

**Output:**
sdtm_ds.csv – Final SDTM dataset in .csv format

--------------------------------------------------
**Question 2 – ADaM Dataset Creation**

**Folder:** question_2_adam_create_adsl

**Programs:**
question_2_adamcreate_adsl.R – Creates ADaM Subject Level Analysis dataset

**Input Data:**
 pharmaversesdtm::dm, pharmaversesdtm::vs, pharmaversesdtm::ex, pharmaversesdtm::ds, pharmaversesdtm::ae  
 
**Required Packages:**
admiral, tidyverse, pharmaverseraw 

**Log:**
adam_adsl_logfile.txt – Execution log file

**Output:**
ADaM_ADSL.csv – Final ADaM dataset in .csv format

--------------------------------------------------
**Question 3 – TLG Generation**

**Folder:** question_3_tlg_creation

**Programs:**
question_3_tlg01_create_ae_summary_table.R – Generates AE summary table of Treatment Emergent AEs
question_3_tlg02_create_visualizations.R   – Generates two AE plots # Plot1 - AE Severity Distribution by treatment (Bar chart)
                                                                    # Plot2 - Top 10 most frequent AEs (Forest Plot)
 **Input Data for ae_summary table:**
 pharmaverseadam::adae, pharmaverseadam::adsl  
 
**Required Packages for ae_summary table:**
gtsummary, tidyverse, pharmaverseraw, tern   

 **Input Data for visualization AE plots:**
 pharmaverseadam::adae 
 
**Required Packages for ae_summary table:**
ggplot2, tidyverse

**Logs:**
tlg01_ae_summary_table_logfile.txt
tlg02_visualizations_logfile.txt

**Outputs:**
tlg01_ae_summary table.pdf – Summary table in pdf format
tlg01_ae_summary_table.html – Summary table in html format
tlg02_AE_Severity_Distribution - Visualization Plot 1
tlg02_Top10_AE_ForestPlot - Visualization Plot 2**
