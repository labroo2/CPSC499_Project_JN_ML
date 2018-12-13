#this function generates scripts to run STRUCTURE compatible with Linux-like environments,
#including high-performance computing clusters
structure_scriptwriter <- function(task_manager, module, mainparams = NULL,
                                   extraparams = NULL, stratparams = NULL,
                                   populations, numloci = NULL, numinds = NULL,
                                   input_file, output_file_base, seed = NULL,
                                   reps_per_k, script_basename){
  #create a matrix to hold the file name for each combination of population (k) and rep
  scriptname <- matrix(nrow = populations, ncol = reps_per_k, NA)
  
  #paste the arguments together
  task_text <- if(is.null(task_manager) == FALSE) paste(task_manager, collapse = "\n")
  new_line <- if(is.null(task_manager) == FALSE) paste("\n")
  module <- if(is.null(module) == FALSE) paste("module load", module, sep = " ")
  new_line2 <- if(is.null(module) == FALSE) paste("\n")
  main <- if(is.null(mainparams) == FALSE) paste("-m", mainparams, sep = " ")
  extra <- if(is.null(extraparams) == FALSE) paste("-e", extraparams, sep = " ")
  strat <- if(is.null(stratparams) == FALSE) paste("-s", stratparams, sep = " ")
  loc <- if(is.null(numloci) == FALSE) paste("-L", numloci, sep = " ")
  inds <- if(is.null(numinds) == FALSE) paste("-N", numinds, sep = " ")
  infile <- if(is.null(input_file) == FALSE) paste("-i", input_file, sep = " ")
  seed <- if(is.null(seed) == FALSE) paste("-D", seed, sep = " ")
  
  #for each population i and replication j, concatenate the specified parameters
  for(i in 1:populations){
    for(j in 1:reps_per_k){
 scriptname[i,j] <- paste(script_basename, "_", "k", i, "r", j, ".sh", sep = "")
      cat(task_text, new_line, module,
          new_line2, "structure",
          main, extra, strat,
          if(is.null(populations) == FALSE) paste("-K", i, sep = " "),
          loc, inds, infile,
          #create a unique output file name that specifies k and r
          if(is.null(output_file_base) == FALSE){
            outname <- paste(output_file_base, "k", i, "r", j, sep = "")
            paste("-o", outname, sep = " ")
      },
          seed,
          sep = " ", file = scriptname[i,j])
    }
  }
}