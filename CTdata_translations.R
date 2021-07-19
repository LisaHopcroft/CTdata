# NIHR inclusion framework
# ========================
# ethinicity_translation = c(
#   "White: English",
#   "White: Scottish",
#   "White: Northern Irish",
#   "White: British",
#   "White: Irish",
#   "White: other",
#   "Mixed/Multiple: White and Black Caribbean",
#   "Mixed/Multiple: White and Black African",
#   "Mixed/Multiple: White and Asian",
#   "Mixed/Multiple: other",
#   "Asian/Asian British: Indian",
#   "Asian/Asian British: Pakistani",
#   "Asian/Asian British: Bangladeshi",
#   "Asian/Asian British: Chinese",
#   "Asian/Asian British: other",
#   "Black/African/Caribbean/Black	British: African",
#   "Black/African/Caribbean/Black	British: Caribbean",
#   "Black/African/Caribbean/Black	British: other",
#   "Other: Arab"
# )


#####################################################################
### ETHNICITY data taken from 2011 census
### https://www.scotlandscensus.gov.uk/variables-classification/ethnic-group
### https://en.wikipedia.org/wiki/Demography_of_Scotland#Ethnicity
#####################################################################

ethnicity_translation_table = tribble(
  ~category, ~proportion,
  ### White
  "White: Scottish", 0.8395,
  "White: Other British", 0.0788,
  "White: Irish", 0.0102,
  "White: Gypsy/Traveller", 0.0008,
  "White: Polish", 0.0116,
  "White: Other", 0.0193,
  ### Mixed or multiple ethnic group
  "Mixed or multiple ethnic groups", 0.0037,
  
  ### Asian, Asian Scottish or Asian British
  "Pakistani, Pakistani Scottish or Pakistani British", 0.0093,
  "India, Indian Scottish or Indian British", 0.0062, 
  "Bangladeshi, Bangladeshi Scottish or Bangladeshi British", 0.0007,
  "Chinese, Chinese Scottish or Chinese British", 0.0064,
  "Other Asian, Asian Scottish or Asian British", 0.0040,
  
  ### African
  "African, African Scottish or African British", 0.0055,
  "Other African", 0.0001,

  ### Carribean or Black
  "Caribbean, Caribbean Scottish or Caribbean British", 0.0006,
  "Black, Black Scottish or Black British", 0.0004,
  "Other Caribbean or Black", 0.0001,
  
  ### Other ethnic group
  "Arab, Arab Scottish or Arab British", 0.0018,
  "Other Arab, Arab Scottish or Arab British", 0.0009,
  
  "Other", 0.0027
) %>% 
  mutate( code = 1:n() ) %>% 
  select( code, category, proportion )

ethnicity_translation = ethnicity_translation_table$category

#####################################################################
### SURGERY outcome data
#####################################################################

surgery_translation_table = tribble(
~category, ~proportion,
"Thrombus - Milked back into renal vein and side clamped", 0.0,
"Infra-hepatic (IVC clamping with no liver mobilisation)", 0.1,
"Retro-hepatic (liver mobilisation and clamping below hepatic veins)",0.3,
"Retro-hepatic (liver mobilisation and clamping above hepatic veins)",0.5,
"Supra-hepatic (infradiaphragmatic)", 0.1
) %>% 
  mutate( code = 1:n() ) %>% 
  select( code, category, proportion )

surgery_translation = surgery_translation_table$category

#####################################################################
### RECIST data
#####################################################################

RECIST_translation_table = tribble(
  ~category, ~proportion,
  "Complete response", 0,
  "Partial response", 0.1,
  "Stable disease", 0.7,
  "Progressive disease", 0.2
) %>% 
  mutate( code = 1:n() ) %>% 
  select( code, category, proportion )

RECIST_translation = RECIST_translation_table$category


#####################################################################
### Yes/No translations
#####################################################################

binary_translation = list( `1` = "Yes",
                           `2` = "No" )
