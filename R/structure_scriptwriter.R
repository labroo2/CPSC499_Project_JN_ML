structure_scriptwriter <- function(task_manager, module, mainparams, extraparams, stratparams, populations,
                              numloci, numinds, input_file, output_file_base, seed, reps_per_k, script_basename){
  #create a matrix to hold the file name for each combination of population (k) and rep
  scriptname <- matrix(nrow = populations, ncol = reps_per_k, NA)
  #for each population i and replication j, concatenate the specified parameters
  for(i in 1:populations){
    for(j in 1:reps_per_k){
 scriptname[i,j] <- paste(script_basename, "_", "k", i, "r", j, ".sh", sep = "")
      cat(if(is.na(task_manager) == FALSE) paste(task_manager, collapse = "\n"),
          if(is.na(task_manager) == FALSE) paste("\n"),
          if(is.na(module) == FALSE) paste("module load", module, sep = " "),
          if(is.na(module) == FALSE) paste("\n"),
          paste("structure"),
          if(is.na(mainparams) == FALSE) paste("-m", mainparams, sep = " "),
          if(is.na(extraparams) == FALSE) paste("-e", extraparams, sep = " "),
          if(is.na(stratparams) == FALSE) paste("-s", stratparams, sep = " "),
          if(is.na(populations) == FALSE) paste("-K", i, sep = " "),
          if(is.na(numloci) == FALSE) paste("-L", numloci, sep = " "),
          if(is.na(numinds) == FALSE) paste("-N", numinds, sep = " "),
          if(is.na(input_file) == FALSE) paste("-i", input_file, sep = " "),
          #creates a unique output file name that specifies k and r
          if(is.na(output_file_base) == FALSE){
            outname <- paste(output_file_base, "k", i, "r", j, sep = "")
            paste("-o", outname, sep = " ")
      },
        if(is.na(seed) == FALSE) paste("-D", seed, sep = " "),
        sep = " ", file = scriptname[i,j])
    }
  }
}
