########################################################
# Question  : question_2_adam/create_adsl_domain.R
# Dataset   : ADSL - Subject Level Analysis Dataset
#Input 
# datasets  :  pharmaversesdtm::dm, pharmaversesdtm::vs, pharmaversesdtm::ex, 
#              pharmaversesdtm::ds, pharmaversesdtm::ae 
#########################################################

#Loading required Libraries
library(admiral)
library(pharmaversesdtm)
library(tidyverse)

#Reading Input Datasets

dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

#Converting Blanks/missing values to NA

dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

#Create ADSL

adsl <-dm %>%
  select(-DOMAIN)

#Derive AGEGR9 categorical age group
#Derive AGEGR9N numeric version of age group

adsl <- adsl %>%
  mutate (
    AGEGR9N = case_when(
                     AGE < 18      ~ 1,
                between(AGE,18,50) ~ 2,
                        AGE >50    ~ 3,
              ),
    AGEGR9 = recode(AGEGR9N,
                    '1' = "<18",
                    '2' = "18-50",
                    '3' = ">50",
                    )
  )

#Derive TRTSDTM/ TRTSTMF using the exposure data
#Logic - Filter EXTRT contains Placebo and EXDOSE is >= 0,
#        Datetime of 1st exposure observation (from EX.EXSTDTC)
#        Sort by EXSTDTM & EXSEQ
# Date Imputation rules - Requires complete date
# Time Imputation rules:       
#        - Time missing impute with 00:00:00 
#        - Partially missing time - 00 for hrs, 00 for mins, 00 for secs.
#        - No flag if only seconds is missing

#Process data EX data to get the datetime and its flag
ex_ext <- ex%>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix ="EXST",
    time_imputation = "first",
    ignore_seconds_flag = TRUE
    )%>%
  derive_vars_dtm(
    dtc= EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last",
  ) %>%
 mutate(EXSTDTM = format(EXSTDTM, "%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EXENDTM = format(EXENDTM, "%Y-%m-%dT%H:%M:%S"))

#Get first dose
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = (EXDOSE > 0 |
        (EXDOSE == 0 &
        str_detect(EXTRT,"PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF, ),
    order = exprs(EXSTDTM, EXSEQ),
    mode  = "first",
    by_vars = exprs(STUDYID, USUBJID)
    ) %>%
# Derive ITTFL 
# Logic - DM.ARM populated then 'Y' else 'N'

  mutate(
    ITTFL = if_else(!is.na(ARM) & ARM != "SCREEN FAILURE", "Y", "N"))

# Derivation of LSTAVLDT
# 1 -->  # Derive TRTEDTM getting the last dose

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = (EXDOSE > 0 |
                     (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(
      TRTEDTM = EXENDTM,
      TRTETMF = EXSTTMF
    ),
    order = exprs(EXENDTM, EXSEQ),
    mode  = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

#Helps to chek if date is complete

is_complete_dtc <- function(dtc) {
  !is.na(dtc) & grepl("^\\d{4}-\\d{2}-\\d{2}$", dtc)
}

#Derivation to get LSTAVLDT
adsl <- adsl %>%
  distinct(STUDYID, USUBJID, .keep_all = TRUE) %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
       # Get Last complete date of VS
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(VSSTRESC) & !is.na(VSSTRESN) & is_complete_dtc(VSDTC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(VSDTC),
          seq = VSSEQ
        )
      ),
      # Get Last complete date of AE
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = is_complete_dtc(AESTDTC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(AESTDTC),
          seq = AESEQ
        )
      ),
      # Get Last complete date of DS
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = is_complete_dtc(DSSTDTC),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(DSSTDTC),
          seq = DSSEQ
        )
      ),
      # Get Last complete date of TRTEDTM
      event(
        dataset_name = "adsl",
        condition = is_complete_dtc(TRTEDTM),
        set_values_to = exprs(
          LSTAVLDT = as.Date(TRTEDTM),
          seq = 0
        )
      )
    ),
 # dataset to be used
 source_datasets = list(vs=vs, ae=ae, ds=ds, adsl=adsl),
 
 #Create temporary event var
 tmp_event_nr_var = event_nr,
 order = exprs(LSTAVLDT, seq, event_nr),
 mode = "last",
 new_vars = exprs(LSTAVLDT, seq)
  )
 
 #Final dataset
 
 adsl_final <-adsl %>%
   select(
     STUDYID, USUBJID, AGEGR9, AGEGR9N, TRTSDTM, 
     TRTSTMF, ITTFL, TRTEDTM, LSTAVLDT
     )
 

 # Final output in csv format
 
 write_csv(adsl_final,"ADaM_ADSL.csv")
