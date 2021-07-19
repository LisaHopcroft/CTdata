library( simstudy )
library( magrittr )
library( dplyr )
library( tibble )

source("CTdata_functions.R")
source("CTdata_translations.R")

set.seed(5482)

number_of_participants = 20

timepoint.list = sprintf( "T%d", 0:4 ) 

#####################################################################
### Initialise the data definition object                         ###
#####################################################################

trial_data.definition = 
  ### Defining the gender variable
  defData( varname = "Gender",
           dist = "categorical",
           formula = genCatFormula(0.50,0.50),
           id = "idnum" ) %>% 
  defData( varname = "Arm",
           dist = "binary",
           formula = 0.5 ) %>%
  defData( varname = "Ethnicity",
           dist = "categorical",
           formula = paste(sprintf("%.4f",ethnicity_translation_table$proportion),
                           collapse=";") ) %>% 
  defData ( varname = "Height",
            dist = "normal",
            formula = 171, 
            variance = 20 ) %>% 
  defData ( varname = "Weight",
            dist = "normal",
            formula = 76.5, 
            variance = 15 ) %>% 
  defData( varname = "surgery_planned",
           dist = "categorical",
           formula = paste(sprintf("%.4f",surgery_translation_table$proportion),
                           collapse=";") ) %>% 
  defData( varname = "RECIST_Wk6",
           dist = "categorical",
           formula = paste(sprintf("%.4f",RECIST_translation_table$proportion),
                           collapse=";") ) %>% 
  defData( varname = "Screening_PMH_Throm",
           dist = "binary",
           formula = 0.5 ) %>% 
  defData( varname = "Screening_PMH_Cereb",
           dist = "binary",
           formula = 0.5 )

example_trial_data.0 = genData( number_of_participants,
                                trial_data.definition )

example_trial_data.1 = example_trial_data.0 %>% 
  ### Fix label
  mutate( Label = sprintf( "N%0*d", nchar(number_of_participants), idnum ) ) %>%
  ### Fix age
  mutate( Age = 80-abs(rnorm(number_of_participants, mean=10, sd=10)) %>% round ) %>% 
  ### Have some random number of patients improve surgical outcome
  mutate( surgery_performed = surgery_planned-rnorm(number_of_participants, sd=2) %>% round ) %>% 
  mutate( surgery_performed = ifelse( surgery_performed > surgery_planned,
                                      surgery_planned,
                                      surgery_performed) ) %>% 
  mutate( surgery_performed = ifelse( surgery_performed <= 0,
                                      1,
                                      surgery_performed ) ) %>% 
  mutate( surgery_performed = ifelse( surgery_performed > max(surgery_translation_table$code),
                                      max(surgery_translation_table$code),
                                      surgery_performed ) ) %>% 
  
  ### RECIST at a later timepoint
  mutate( RECIST_Wk12 = RECIST_Wk6-rnorm(number_of_participants, sd=1) %>% round ) %>% 
  mutate( RECIST_Wk12 = ifelse( RECIST_Wk12 > RECIST_Wk6,
                                RECIST_Wk12,
                                RECIST_Wk12) ) %>% 
  mutate( RECIST_Wk12 = ifelse( RECIST_Wk12 <= 0,
                                1,
                                RECIST_Wk12 ) ) %>% 
  mutate( RECIST_Wk12 = ifelse( RECIST_Wk12 > max(RECIST_translation_table$code),
                                max(RECIST_translation_table$code),
                                RECIST_Wk12 ) ) %>% 
  
  ### PMH Yes/No variables
  mutate( Screening_PMH_Throm = Screening_PMH_Throm + 1 ) %>% 
  mutate( Screening_PMH_Cereb = Screening_PMH_Cereb + 1 )


###
### Translation
###

example_trial_data.2 = example_trial_data.1 %>% 
  mutate( Arm = recode(Arm,`0`="A", `1`="B")) %>% 
  mutate( Gender = recode( Gender, `1`="Female", `2`="Male")) %>% 
  mutate( Ethnicity = ethnicity_translation[ Ethnicity ]) %>% 
  mutate( surgery_planned = surgery_translation[ surgery_planned ] ) %>%  
  mutate( surgery_performed = surgery_translation[ surgery_performed ] ) %>% 
  mutate( RECIST_Wk6 = RECIST_translation[ RECIST_Wk6 ] ) %>% 
  mutate( RECIST_Wk12 = RECIST_translation[ RECIST_Wk12 ] ) %>% 
  mutate( Screening_PMH_Throm = binary_translation[ Screening_PMH_Throm ] ) %>% 
  mutate( Screening_PMH_Cereb = binary_translation[ Screening_PMH_Cereb ] )
  
example_trial.data = example_trial_data.2 %>% 
  select( Label, Arm,
          Age, Height, Weight,
          Gender, Ethnicity,
          starts_with( "Screening"),
          starts_with( "surgery" ),
          starts_with( "RECIST" ) )

example_trial.glossary = list(
  Label = "Patient ID",
  Arm = "Treatment Arm",
  Age = "Age at randomisation",
  Gender = "Gender",
  Ethnicity = "Ethnicity",
  Height = "Height (cm)",
  Weight = "Weight (kg)",
  Screening_PMH_Throm = "Existing or previous thromboembolic conditions/events?",
  Screening_PMH_Cereb = "Existing or previous cerebrovascular conditions/events?",
  surgery_planned = "Planned control of IVC or renal vein",
  surgery_performed = "Control of IVC or renal vein",
  RECIST_Wk6 = "RECIST assessment @ Week 6",
  RECIST_Wk12 = "RECIST assessment @ Week 12"
)

example_trial.vocabulary = list(
  Arm       = LETTERS[1:2],
  Gender    = c("Female","Male"),
  Ethnicity = ethnicity_translation_table$category,
  surgery_planned = surgery_translation_table$category,
  surgery_performed = surgery_translation_table$category,
  Screening_PMH_Throm = unlist( binary_translation ),
  Screening_PMH_Cereb = unlist( binary_translation ),
  RECIST_Wk6  = RECIST_translation_table$category,
  RECIST_Wk12 = RECIST_translation_table$category
)


save(example_trial.data,
     example_trial.glossary,
     example_trial.vocabulary,
     file="dat/example_trial_data.RData")
