library(CTutils)
library(rlang)

# data(example_trial_data)

glossary.key = list( surgery_planned = "Surgery planned" )
parameter.vocab = list( surgery_planned = c(
  "Thrombus - Milked back into renal vein and side clamped",
  "Infra-hepatic (IVC clamping with no liver mobilisation)",
  "Retro-hepatic (liver mobilisation and clamping below hepatic veins)",
  "Retro-hepatic (liver mobilisation and clamping above hepatic veins)",
  "Supra-hepatic (infradiaphragmatic)",
  "Supra-hepatic (supradiaphragmatic)") )


  
  do_list_extraction( this_data = example_trial_data,
                      this_var  = quo(surgery_planned),
                      vocab_for_variables = parameter.vocab
                      # these_levels = c( "Thrombus - Milked back into renal vein and side clamped",
                      #                   "Infra-hepatic (IVC clamping with no liver mobilisation)",
                      #                   "Retro-hepatic (liver mobilisation and clamping below hepatic veins)",
                      #                   "Retro-hepatic (liver mobilisation and clamping above hepatic veins)",
                      #                   "Supra-hepatic (infradiaphragmatic)",
                      #                   "Missing"),
                      # key_for_variables = glossary.key )
  )
