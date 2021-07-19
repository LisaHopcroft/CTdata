
add_dropouts = function( d,
                         trajectory,
                         target_perc = 0.10,
                         verbose = FALSE ) {
  # d = trial_data.synthetic.MODERATE
  # trajectory = colnames( trial_data.synthetic.MODERATE %>% select( ends_with( "public_time")) %>% select( starts_with("ATT")))  
  
  n_updates_made = 0
  
  this.focus = d %>% select( idnum, {{trajectory}} )
  
  total_n = this.focus %>% nrow  
  dropout_n = ( total_n * target_perc ) %>% plyr::round_any(1)
  
  these.dropouts.sample = sample( this.focus %>% pull( idnum ),
                                  dropout_n,
                                  replace=FALSE )
  
  mutate_holder = rep.int( NA, nrow(d) )
  
  for( i in these.dropouts.sample ) {
    this.idnum = these.dropouts.sample[i]
    this.dropout_from_here = sample(trajectory,1)
    
    if ( verbose ) {
      cat( this.dropout_from_here, "\n" )
      cat( this.idnum, "\n" )
    }
    
    these.tochange = trajectory %>% purrr::keep( ~ .x >= this.dropout_from_here )
    
    row.i = which( d$idnum==this.idnum )
    
    mutate_holder[ row.i ] = this.dropout_from_here
    
    for ( col.name in these.tochange ) {
      if ( verbose ) cat( sprintf( " +" ) ) 
      # cat( sprintf( "[%s] pre  = %d\n", col.name, d[[col.name]][row.i]  ))
      d[[col.name]][row.i] = 0
      # cat( sprintf( "[%s] post = %d\n", col.name, d[[col.name]][row.i]  ))
      n_updates_made = n_updates_made + 1
    }
    
    if ( verbose ) cat( sprintf("\n") )
    
  }
  
  cat( sprintf( "In adding dropouts, %d updates made.\n",
                n_updates_made ) )
  d$DROPOUT = mutate_holder
  
  return( d )
  
}