#this function takes a filename corresponding to STRUCTURE output and creates an object of the class deStruct
deStruct <- function(file){
  #read in the lines of a structure file
  mylines <- readLines(file, warn = FALSE)#warn=FALSE because STRUCTURE files have a missing  End of line marker that throws a warning
  #Check that a STRUCTURE file was input
  if(sum(grepl("STRUCTURE by Pritchard", mylines)) == 0){
    stop("The file does not appear to be a STRUCTURE file")
  } 
  #PARSE THE FILE AND PUT DATA INTO APPROPRIATE STRUCTURES
  #get the run parameters lines
  run_param <- grep("Run parameters:", mylines)
  run_params <- mylines[(run_param + 1):(run_param + 5L)]
  run_parameters <- get_run_param(run_params) #get_run_param function formats lines into a dataframe
  
  ###Determine the number of clusters###
  inferred <- grep("Inferred Clusters", mylines)
  inferred_clus <- mylines[(inferred + 1):(inferred + 2)]
  inferred_cluster <- get_inferred_clus(inferred_clus) #get_inferred_clus function formarts lines into a dataframe
  
  ###Expected Heterozygosity###
  E_heterozygosity <- grep("expected heterozygosity", mylines)
  heterozygosity <- mylines[(E_heterozygosity + 1):(E_heterozygosity + length(inferred_cluster$cluster))]
  expected_heterozygosity <- get_expected_heterozygosity(heterozygosity)#get_expected_heterozygosity function to formar to data frame
  
  ###FST values###
  FST <- grep("Mean value of Fst_1", mylines)
  FST_values <- mylines[FST:(FST + length(inferred_cluster$cluster) - 1)]
  mean_FST_value <- get_FST_values(FST_values)#get_FST_values function formarts lines into a dataframe
  
  ###Inferred ancestry of individuals###
  ancestry <- grep("Inferred ancestry of individuals:", mylines)
  #get the inferred ancestry by the number of individuals in run parameters
  number_of_individuals <- as.integer(run_parameters$Value[run_parameters$parameter == "individuals"])
  ancestry_val <- mylines[(ancestry + 2):(ancestry + number_of_individuals + 1)]#remove the header line
  ancestry_value <- get_ancestry_value(ancestry_val, number_of_individuals, inferred_cluster)#get_ancestry_value function to convert ancestry values to dataframe
  
  ###Estimated Allele Frequency###
  allele_start <- grep("Estimated Allele Frequencies in each cluster", mylines)
  allele_end <- grep("Values of parameters used in structure:", mylines)
  allele_freq <- mylines[(allele_start + 4):(allele_end - 2)]
  allele_frequency <- get_allele_frequency(allele_freq)#get_allele_frequency function to convert alelewise ancestry values to a dataframe

  #Put all elements of the structure file into a list
  structure_output <- list(run_parameters = run_parameters, inferred_clusters = inferred_cluster, 
                           HE = expected_heterozygosity, FST= mean_FST_value, 
                           individual_ancestry_frequencies = ancestry_value,
                           allelewise_ancestry_frequency = allele_frequency)
  # assign class
  class(structure_output) <- c("destruct", class(structure_output))
  return(structure_output)
}
