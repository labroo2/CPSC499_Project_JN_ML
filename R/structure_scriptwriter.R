#this function generates scripts to run STRUCTURE compatible with Linux-like environments, including high-performance computing clusters
structure_scriptwriter <- function(task_manager, module, mainparams, extraparams, stratparams, populations,
                              numloci, numinds, input_file, output_file_base, seed, reps_per_k, script_basename){
  #create a matrix to hold the file name for each combination of population (k) and rep
  scriptname <- matrix(nrow = populations, ncol = reps_per_k, NA)
  #for each population i and replication j, concatenate the specified parameters
  for(i in 1:populations){
    for(j in 1:reps_per_k){
 scriptname[i,j] <- paste(script_basename, "_", "k", i, "r", j, ".sh", sep = "")
      cat(if(is.null(task_manager) == FALSE) paste(task_manager, collapse = "\n"),
          if(is.null(task_manager) == FALSE) paste("\n"),
          if(is.null(module) == FALSE) paste("module load", module, sep = " "),
          if(is.null(module) == FALSE) paste("\n"),
          paste("structure"),
          if(is.null(mainparams) == FALSE) paste("-m", mainparams, sep = " "),
          if(is.null(extraparams) == FALSE) paste("-e", extraparams, sep = " "),
          if(is.null(stratparams) == FALSE) paste("-s", stratparams, sep = " "),
          if(is.null(populations) == FALSE) paste("-K", i, sep = " "),
          if(is.null(numloci) == FALSE) paste("-L", numloci, sep = " "),
          if(is.null(numinds) == FALSE) paste("-N", numinds, sep = " "),
          if(is.null(input_file) == FALSE) paste("-i", input_file, sep = " "),
          #creates a unique output file name that specifies k and r
          if(is.null(output_file_base) == FALSE){
            outname <- paste(output_file_base, "k", i, "r", j, sep = "")
            paste("-o", outname, sep = " ")
      },
        if(is.null(seed) == FALSE) paste("-D", seed, sep = " "),
        sep = " ", file = scriptname[i,j])
    }
  }
}