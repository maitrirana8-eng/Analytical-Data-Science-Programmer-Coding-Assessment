########################################################
# Question  : question_1_sdtm/01_create_ds_domain.R
# Dataset   : DM domain
#Input 
# datasets  : pharmaverseraw::ds_raw
#########################################################

#Load required Libraries
library(sdtm.oak)
library(pharmaverseraw)
library(tidyverse)

#Read the raw datasets

ds_raw <- pharmaverseraw::ds_raw


#Create oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var= "PATNUM",
    raw_src= "ds_raw"
  )

#Read the CT
study_ct <-  
  data.frame( 
    stringsAsFactors = FALSE, 
    codelist_code = c("C66727","C66727", 
                      "C66727","C66727","C66727","C66727","C66727","C66727", 
                      "C66727","C66727"), 
    term_code = c("C41331","C25250", 
                  "C28554","C48226","C48227","C48250","C142185","C49628", 
                  "C49632","C49634"), 
    term_value = c("ADVERSE EVENT", 
                   "COMPLETED","DEATH","LACK OF EFFICACY","LOST TO FOLLOW-UP", 
                   "PHYSICIAN DECISION","PROTOCOL VIOLATION", 
                   "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR", 
                   "WITHDRAWAL BY SUBJECT"), 
    collected_value = c("Adverse Event", 
                        "Complete","Dead","Lack of Efficacy","Lost To Follow-Up", 
                        "Physician Decision","Protocol Violation", 
                        "Trial Screen Failure","Study Terminated By Sponsor", 
                        "Withdrawal by Subject"), 
    term_preferred_term = c("AE","Completed","Died", 
                            NA,NA,NA,"Violation", 
                            "Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"), 
    term_synonyms = c("ADVERSE EVENT", 
                      "COMPLETE","Death",NA,NA,NA,NA,NA,NA, 
                      "Discontinued Participation")
  )


#Map Topic variable requiring no CT
ds <- assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "IT.DSTERM",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
)

#Map variables requiring CT
#Create a list of DSDECOD values present in CT codelist
ct_list <- ds_raw %>%
  filter(!is.na("IT.DSDECOD"), "IT.DSDECOD" != "")%>%
  mutate(DSDECOD1 = str_to_upper(str_squish(IT.DSDECOD)))%>%
  distinct(DSDECOD1) %>%
  filter(
    DSDECOD1 %in% (
      study_ct %>%
        filter(codelist_code == "C66727") %>%
        pull (term_value) %>%
        str_to_upper() %>%
        str_squish()
    )
  ) %>%
  pull(DSDECOD1)

#Create dataframe with coded DSDECOD values
ds_coded <- ds_raw %>%
  mutate(DSDECOD1 = str_to_upper(str_squish(`IT.DSDECOD`))) %>%
  filter(!is.na(DSDECOD1), DSDECOD1 != "", DSDECOD1 %in% ct_list) %>%
  assign_ct(
    raw_dat = .,
    raw_var = "DSDECOD1",
    tgt_var = "IT.DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) 
#Rename IT.DSDECOD to populate DSDECOD with CT-compliant coded values
# in subsequent steps
ds_coded <- ds_coded %>%
  select(all_of(oak_id_vars()), "IT.DSDECOD") %>%
  rename(DSDECOD_CODED = "IT.DSDECOD")


#Map other required timing variables
ds <- ds %>%
  # Map DSDTC using assign_datetime, raw_var=DSDTCOL
  
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL","DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    id_vars = oak_id_vars()
  )%>%
  
  # Map DSSTDTC using assign_datetime, raw_var=IT.DSSTDAT
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars()
  ) 

# Create SDTM derived vars and map the rest of variables
# 1) Join coded DSDECOD values derived using CT mapping
# 2) Assign standard SDTM identifiers and variables from raw dataset
# 3) Derive DSTERM: use OTHERSP if DSTERM is missing/blank; convert to uppercase
# 4) Derive DSDECOD using coded value first; fallback to raw IT.DSDECOD 
#     if coded value missing.
# 5) If DSDECOD still missing, populate from OTHERSP
# 6) Derive DSCAT based on event type


ds <-ds %>%
  
  #Join coded DSDECOD values derived using CT mapping
  left_join(ds_coded, by = oak_id_vars()) %>%
    mutate(

    STUDYID = ds_raw$STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    VISIT   = ds_raw$INSTANCE,
    OTHERSP = ds_raw$OTHERSP,
    
    DSTERM = case_when(
      (is.na(DSTERM) | DSTERM == "" | DSTERM == "NA") &
        !is.na(OTHERSP) & OTHERSP != "" ~ str_to_upper(OTHERSP),
      TRUE ~ str_to_upper(DSTERM)
    ),
    
    DSDECOD = coalesce(
      na_if(DSDECOD_CODED, ""),
      na_if(str_to_upper(str_squish(ds_raw$`IT.DSDECOD`)), "")
    ),
    
    DSDECOD = case_when(
      (is.na(DSDECOD) | DSDECOD == "" | DSDECOD == "NA") &
        !is.na(OTHERSP) & OTHERSP != "" ~ str_to_upper(OTHERSP),
      TRUE ~ str_to_upper(DSDECOD)
    ),
    
    DSCAT = case_when(
      OTHERSP !="NA" & !is.na(OTHERSP) ~ "OTHER EVENT",
      DSDECOD =="RANDOMIZED"          ~ "PROTOCOL MILESTONE",
      TRUE                            ~ "DISPOSITION EVENT",
      )) %>%
#Create a seq number
  arrange(USUBJID, DSSTDTC) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ =row_number()) %>%
  ungroup() %>%
  select(-DSDECOD_CODED)

#Derive study day creating DM like reference table using min of DSSTDTC

 dm_like <- ds %>%
  filter(DSDECOD == "RANDOMIZED") %>%
  mutate(DSSTDTC_d = as.Date(as.character(DSSTDTC))) %>%
  group_by(USUBJID) %>%
  summarise(RFSTDTC = min(DSSTDTC_d, na.rm = TRUE), .groups = "drop")
 
 # Derive Study day
  ds <- ds %>%
    derive_study_day(
    sdtm_in = .,
    dm_domain = dm_like,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
    ) %>%
# Get Visit and VISITNUM
   mutate(
     VISIT1 = str_to_upper(str_squish(VISIT)),
     week = if_else(
       str_detect(VISIT1, "^WEEK\\s*\\d+"), 
       as.numeric(str_extract(VISIT1, "\\d+")),
       NA_real_)
     ) %>%
    group_by(USUBJID) %>%
    mutate(
      #Gives maximum scheduled Visitnum
      othvis_num = max(c(1,1 + dense_rank(week)), na.rm = TRUE),
      
  # Seq for visits that are not (screening/baseline/week/unscheduled)
      other_n = cumsum(
        ! str_detect(VISIT1, "^SCREENING") |
         VISIT1 ==  "BASELINE" |
        !is.na(week) |        
        str_detect(VISIT1, "^UNSCHEDULED")),
      
       VISITNUM = case_when(
        str_detect(VISIT1, "^SCREENING")   ~ 0,
        VISIT1 ==         "BASELINE"       ~ 1,
        !is.na(week)                       ~ 1 + dense_rank(week),
        str_detect(VISIT1, "^UNSCHEDULED") ~99,
        TRUE                               ~ othvis_num + other_n #
        )
    ) %>%
    ungroup() %>%
    select (-othvis_num, -other_n, -VISIT1, -week)
  
  # Final SDTM.DS
  ds_final <- ds %>%
    select (STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, 
            DSSTDTC, DSSTDY) %>%
    arrange (USUBJID,DSSEQ)
  
  # Final output in csv format
  
  write_csv(ds_final,"SDTM_DS.csv")
  