library( simstudy )
library( magrittr )
library( dplyr )
library( tibble )


###==================================================================
### Function to add missing data to dataset
### =================================================================
#' 
#' #' Add dropouts to data
#' #'
#' #' @param d The data frame containing the column of interest
#' #' @param trajectory 
#' #' @param target_perc 
#' #' @param verbose 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' add_dropouts = function( d,
#'                          trajectory,
#'                          target_perc = 0.10,
#'                          verbose = FALSE ) {
#'   n_updates_made = 0
#'   
#'   this.focus = d %>% select( idnum, {{trajectory}} )
#'   
#'   total_n = this.focus %>% nrow  
#'   dropout_n = ( total_n * target_perc ) %>% plyr::round_any(1)
#'   
#'   these.dropouts.sample = sample( this.focus %>% pull( idnum ),
#'                                   dropout_n,
#'                                   replace=FALSE )
#'   
#'   mutate_holder = rep.int( NA, nrow(d) )
#'   
#'   for( i in these.dropouts.sample ) {
#'     this.idnum = these.dropouts.sample[i]
#'     this.dropout_from_here = sample(trajectory,1)
#'     
#'     if ( verbose ) {
#'       cat( this.dropout_from_here, "\n" )
#'       cat( this.idnum, "\n" )
#'     }
#'     
#'     these.tochange = trajectory %>% purrr::keep( ~ .x >= this.dropout_from_here )
#'     
#'     row.i = which( d$idnum==this.idnum )
#'     
#'     mutate_holder[ row.i ] = this.dropout_from_here
#'     
#'     for ( col.name in these.tochange ) {
#'       if ( verbose ) cat( sprintf( " +" ) ) 
#'       # cat( sprintf( "[%s] pre  = %d\n", col.name, d[[col.name]][row.i]  ))
#'       d[[col.name]][row.i] = 0
#'       # cat( sprintf( "[%s] post = %d\n", col.name, d[[col.name]][row.i]  ))
#'       n_updates_made = n_updates_made + 1
#'     }
#'     
#'     if ( verbose ) cat( sprintf("\n") )
#'     
#'   }
#'   
#'   cat( sprintf( "In adding dropouts, %d updates made.\n",
#'                 n_updates_made ) )
#'   d$DROPOUT = mutate_holder
#'   
#'   return( d )
#'   
#' }


generate_example_trial_data = function() {
  
  #####################################################################
  #####################################################################
  ### TRANSLATION TABLES ##############################################
  #####################################################################
  #####################################################################
  
  
  ###==================================================================
  ### ETHNICITY data taken from 2011 census
  ### https://www.scotlandscensus.gov.uk/variables-classification/ethnic-group
  ### https://en.wikipedia.org/wiki/Demography_of_Scotland#Ethnicity
  ###==================================================================
  
  ethnicity_translation_table = tibble::tribble(
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
    dplyr::mutate( code = 1:n() ) %>% 
    dplyr::select( code, category, proportion )
  
  
  ethnicity_translation = ethnicity_translation_table$category
  
  ###==================================================================
  ### SURGERY outcome data
  ###==================================================================
  
  surgery_translation_table = tibble::tribble(
    ~category, ~proportion,
    "Thrombus - Milked back into renal vein and side clamped", 0.0,
    "Infra-hepatic (IVC clamping with no liver mobilisation)", 0.1,
    "Retro-hepatic (liver mobilisation and clamping below hepatic veins)",0.3,
    "Retro-hepatic (liver mobilisation and clamping above hepatic veins)",0.5,
    "Supra-hepatic (infradiaphragmatic)", 0.1
  ) %>% 
    dplyr::mutate( code = 1:n() ) %>% 
    dplyr::select( code, category, proportion )
  
  surgery_translation = surgery_translation_table$category
  
  ###==================================================================
  ### RECIST data
  ###==================================================================
  
  RECIST_translation_table = tibble::tribble(
    ~category, ~proportion,
    "Complete response", 0,
    "Partial response", 0.1,
    "Stable disease", 0.7,
    "Progressive disease", 0.2
  ) %>% 
    dplyr::mutate( code = 1:n() ) %>% 
    dplyr::select( code, category, proportion )
  
  RECIST_translation = RECIST_translation_table$category
  
  
  ###==================================================================
  ### Yes/No translations
  ###==================================================================
  
  binary_translation = list( `1` = "Yes",
                             `2` = "No" )
  
 
  
  #####################################################################
  #####################################################################
  ### DOING THE THING (i.e., generating the example dataset) ##########
  #####################################################################
  #####################################################################
  
  set.seed(1)
  
  number_of_participants = 50
  
  # timepoint.list = sprintf( "T%d", 0:4 ) 
  
  #####################################################################
  ### Initialise the data definition object                         ###
  #####################################################################
  
  trial_data.definition = 
    ### Defining the gender variable
    simstudy::defData( varname = "Gender",
                       dist = "binary",
                       formula = 0.50,
                       id = "idnum" ) %>% 
    simstudy::defData( varname = "Arm",
                       dist = "binary",
                       formula = 0.5 ) %>%
    simstudy::defData( varname = "Ethnicity",
                       dist = "categorical",
                       formula = paste(sprintf("%.4f",ethnicity_translation_table$proportion),
                                       collapse=";") ) %>% 
    simstudy::defData( varname = "Week1_Surgery_NonInvasive_Y_N",
                       dist = "binary",
                       formula = 0.5 ) %>% 
    simstudy::defData( varname = "Week1_Surgery_Planned",
                       dist = "categorical",
                       formula = paste(sprintf("%.4f",surgery_translation_table$proportion),
                                       collapse=";") ) %>% 
    simstudy::defData( varname = "RECIST_Wk6",
                       dist = "categorical",
                       formula = paste(sprintf("%.4f",RECIST_translation_table$proportion),
                                       collapse=";") ) %>% 
    simstudy::defData( varname = "Screening_PMH_Throm_Y_N",
                       dist = "binary",
                       formula = 0.3 ) %>% 
    simstudy::defData( varname = "Screening_PMH_Cereb_Y_N",
                       dist = "binary",
                       formula = 0.2 )
  
  example_trial_data.0 = simstudy::genData( number_of_participants,
                                            trial_data.definition )
  
  example_trial_data.1 = example_trial_data.0 %>% 
    ### Fix label
    mutate( Label = sprintf( "N%0*d", nchar(number_of_participants), idnum ) ) %>%
    ### Fix age
    mutate( age = 80-abs(rnorm(number_of_participants, mean=10, sd=10)) %>% round ) %>% 
    ### Have some random number of patients improve surgical outcome
    mutate( Surgery_Surgery_Details = Week1_Surgery_Planned-rnorm(number_of_participants, sd=2) %>% round ) %>% 
    mutate( Surgery_Surgery_Details = ifelse( Surgery_Surgery_Details > Week1_Surgery_Planned,
                                              Week1_Surgery_Planned,
                                              Surgery_Surgery_Details) ) %>% 
    mutate( Surgery_Surgery_Details = ifelse( Surgery_Surgery_Details <= 0,
                                              1,
                                              Surgery_Surgery_Details ) ) %>% 
    mutate( Surgery_Surgery_Details = ifelse( Surgery_Surgery_Details > max(surgery_translation_table$code),
                                              max(surgery_translation_table$code),
                                              Surgery_Surgery_Details ) ) %>% 
    
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
    #mutate( Screening_PMH_Throm_Y_N = Screening_PMH_Throm_Y_N + 1 ) %>% 
    #mutate( Screening_PMH_Cereb_Y_N = Screening_PMH_Cereb_Y_N + 1 ) %>% 
    
    ### Invasive/Open surgery
    mutate( Week1_Surgery_Open_Y_N = as.integer(Week1_Surgery_NonInvasive_Y_N==0) ) %>% 
    mutate( Surgery_Surgery_NonInvasive_Y_N = Week1_Surgery_NonInvasive_Y_N - sample(c(0,1),number_of_participants,replace=TRUE)  ) %>% 
    mutate( Surgery_Surgery_NonInvasive_Y_N = ifelse( Surgery_Surgery_NonInvasive_Y_N < 0,
                                                      0,
                                                      Surgery_Surgery_NonInvasive_Y_N ) ) %>% 
    mutate( Surgery_Surgery_Open_Y_N = as.integer(Surgery_Surgery_NonInvasive_Y_N==0) ) %>% 
    mutate_at( vars( ends_with( "Y_N" ) ), function (x) {x+1} )
  
  ###
  ### Translation
  ###
  
  example_trial_data.2 = example_trial_data.1 %>% 
    dplyr::mutate( Arm = dplyr::recode(Arm,`0`="A", `1`="B")) %>% 
    dplyr::mutate( Gender = dplyr::recode( Gender, `0`="Female", `1`="Male")) %>% 
    dplyr::mutate( Ethnicity = ethnicity_translation[ Ethnicity ]) %>% 
    # mutate_at( vars(ends_with( "_Surgery_[Planned|Details]$")), function (x) surgery_translation[x] ) %>% 
    dplyr::mutate( Week1_Surgery_Planned = surgery_translation[ Week1_Surgery_Planned ] ) %>%  
    dplyr::mutate( Surgery_Surgery_Details = surgery_translation[ Surgery_Surgery_Details ] ) %>% 
    dplyr::mutate_at( dplyr::vars(tidyselect::starts_with("RECIST")), function (x) RECIST_translation[x]) %>% 
    # mutate( RECIST_Wk6 = RECIST_translation[ RECIST_Wk6 ] ) %>% 
    # mutate( RECIST_Wk12 = RECIST_translation[ RECIST_Wk12 ] ) %>% 
    dplyr::mutate_at( dplyr::vars(tidyselect::ends_with("Y_N")), function (x) binary_translation[x] )
  # mutate( Screening_PMH_Throm_Y_N = binary_translation[ Screening_PMH_Throm_Y_N ] ) %>% 
  # mutate( Screening_PMH_Cereb_Y_N = binary_translation[ Screening_PMH_Cereb_Y_N ] ) %>% 
  # mutate( Week1_Surgery_NonInvasive_Y_N = binary_translation[Week1_Surgery_NonInvasive_Y_N]) %>% 
  # mutate( Week1_Surgery_Open_Y_N = binary_translation[Week1_Surgery_Open_Y_N]) %>% 
  # mutate( Surgery_Surgery_NonInvasive_Y_N = binary_translation[Surgery_Surgery_NonInvasive_Y_N]) %>% 
  # mutate( Surgery_Surgery_Open_Y_N = binary_translation[Surgery_Surgery_Open_Y_N])
  
  
  example_trial.data = example_trial_data.2 %>% 
    dplyr::select( Label, Arm,
            Gender, Ethnicity, age,
            tidyselect::everything() ) %>% 
    dplyr::select( -idnum )
  
  example_trial.glossary = list(
    Label = "Patient ID",
    Arm = "Treatment Arm",
    Gender = "Gender",
    Ethnicity = "Ethnicity",
    Screening_PMH_Throm_Y_N = "Existing or previous thromboembolic conditions/events?",
    Screening_PMH_Cereb_Y_N = "Existing or previous cerebrovascular conditions/events?",
    Week1_Surgery_Planned = "Planned control of IVC or renal vein",
    Surgery_Surgery_Details = "Control of IVC or renal vein",
    RECIST_Wk6 = "RECIST assessment @ Week 6",
    RECIST_Wk12 = "RECIST assessment @ Week 12",
    Week1_Surgery_NonInvasive_Y_N = "Minimally invasive surgery",
    Week1_Surgery_Open_Y_N = "Open surgery",
    Surgery_Surgery_NonInvasive_Y_N = "Minimally invasive surgery",
    Surgery_Surgery_Open_Y_N = "Open surgery"
  )
  
  example_trial.vocabulary = list(
    Arm       = LETTERS[1:2],
    Gender    = c("Female","Male"),
    Ethnicity = ethnicity_translation_table$category,
    Week1_Surgery_Planned = surgery_translation_table$category,
    Surgery_Surgery_Details = surgery_translation_table$category,
    Screening_PMH_Throm_Y_N = unlist( binary_translation ),
    Screening_PMH_Cereb_Y_N = unlist( binary_translation ),
    RECIST_Wk6  = RECIST_translation_table$category,
    RECIST_Wk12 = RECIST_translation_table$category,
    Week1_Surgery_NonInvasive_Y_N = unlist( binary_translation ),
    Week1_Surgery_Open_Y_N = unlist( binary_translation ),
    Surgery_Surgery_NonInvasive_Y_N = unlist( binary_translation ),
    Surgery_Surgery_Open_Y_N = unlist( binary_translation )
  )
  
  save(example_trial.data,
       example_trial.glossary,
       example_trial.vocabulary,
       file="dat/example_trial_data.RData")
}

generate_example_trial_data()
