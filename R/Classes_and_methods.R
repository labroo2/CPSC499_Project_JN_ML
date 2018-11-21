file2 <- "../../Final_Project/Structure/Structure/3kcomp_filtered_full_default50000_k2r1_f"
file <- "../../Final_Project/Structure/Structure/3kcomp_filtered_full_default50000_k3r1_f"
deStruct <- function(file){
  #open the connection to a structure file
  mycon <- file(file, open = "r")
  mylines <- readLines(mycon, warn = FALSE)#readlines (warn=FALSE because the structure files have a missing  End of line marker that throws a warning)
  #QC for structure file
  if(sum(grepl("STRUCTURE by Pritchard", mylines)) == 0){
    stop("The file does not appear to be a structure file")
  } 
  #PARSE THE FILE GETTING DATA INTO APPROPRIATE DATA STRUCTURES
  #get the run parameters
  run_param <- grep("Run parameters:", mylines)
  run_params <- mylines[(run_param+1):(run_param+5L)]
  run_params <- strsplit(gsub(" ","",run_params), " ")
  run_parameters <- data.frame(parameter=gsub("[[:digit:]]","",run_params),
                               Value=gsub("([[:alpha:]]|[[:punct:]])","",run_params),stringsAsFactors = FALSE)
  
  ###Determine the number of clusters###
  infered <- grep("Inferred Clusters", mylines)
  infered_clus <- mylines[(infered+1):(infered+2)]
  infered_clus <- strsplit(infered_clus, " ")
  infered_clus <- sapply(infered_clus, function(x) x[x != ""])
  infered_cluster <- as.data.frame(infered_clus)
  colnames(infered_cluster) <-c("cluster","proportion")
  
  ###Epected Heterozygosity###
  E_heterozygosity <- grep("expected heterozygosity", mylines)
  heterozygosity <- mylines[(E_heterozygosity+1):(E_heterozygosity + length(infered_cluster$cluster))]
  heterozygosity <- strsplit(heterozygosity, " ")
  heterozygosity <- t(sapply(heterozygosity, function(x) x[x != ""]))
  expected_heterozygosity <- as.data.frame(heterozygosity[,c(2,4)])
  colnames(expected_heterozygosity) <-c("cluster","HE")
  
  ###FST values###
  FST <- grep("Mean value of Fst_1", mylines)
  FST_values <- mylines[FST:(FST + length(infered_cluster$cluster)-1)]
  FST_values <- strsplit(FST_values, "=")
  FST_values <- t(sapply(FST_values, function(x)  x[x != ""]))
  #remove white spaces to the right
  mean_FST_value <- as.data.frame(trimws(FST_values,which = "right"))
  colnames(mean_FST_value) <- c("cluster","FST")
  
  ###INFERRERD ANCESTRY OF INDIVIDUALS#####
  ancestry <- grep("Inferred ancestry of individuals:", mylines)
  number_of_individuals <- as.integer(run_parameters$Value[run_parameters$parameter == "individuals"])
  ancestry_val <- mylines[(ancestry+1):(ancestry+ number_of_individuals)]
  ancestry_val <- strsplit(ancestry_val, " ")
  ancestry_value <- t(sapply(ancestry_val, function(x) x[x != ""]))
  close(mycon)
  return(list(run_parameters,infered_cluster,expected_heterozygosity,mean_FST_value))
}
deStruct(file3)
head(stru)
