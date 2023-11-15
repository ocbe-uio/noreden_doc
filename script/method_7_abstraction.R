# require 
# nutri_pu (per unit contribution) or envir_pu
# lwrupr_energy (computed as constraints)


demo_input <- readRDS('./data_processed/demo_9foods_input.rda')
demo_constraints <- readRDS('./data_processed/demo_9foods_constraints.rda')


info_energy <- list(pu_energy = nutri_pu$energy * std_coef_energy, 
                    lwrc_energy = lwrupr_energy$min, 
                    uprc_energy = lwrupr_energy$max)





f_factory <- function(namelist, datainfo){
  
  # namelist <- c('ene1')
  
  f_ineq <- function (x) {
    
    # energy
    ene1 <- - sum(x * datainfo$pu_energy) + datainfo$lwrc_energy
    ene2 <- sum(x * datainfo$pu_energy) - datainfo$uprc_energy
    
    # give name
    constr_full <- c(
      ene1 = ene1, 
      ene2 = ene2
    )
    # select the ones that matter:
    constr <- constr_full[namelist]
    
    return (constr)
  }
  return(f_ineq)
}

# in this way, the datainfo can also be scaled/unscaled.
f_inequalc <- f_factory(namelist = 'ene1', datainfo = info_energy)
f_inequalc <- f_factory(namelist = c('ene1', 'ene2'), datainfo = info_energy)


