file2 <- "../../Final_Project/Structure/Structure/3kcomp_filtered_full_default50000_k2r1_f"
file3 <- "../../Final_Project/Structure/Structure/3kcomp_filtered_full_default50000_k3r1_f"

#input is the path to structure output file
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
  
  ###Expected Heterozygosity###
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
  #get the inferred ancestry by the number of individuals in run parameters
  number_of_individuals <- as.integer(run_parameters$Value[run_parameters$parameter == "individuals"])
  ancestry_val <- mylines[(ancestry+2):(ancestry+ number_of_individuals+1)]#remove the header line
  #there is alot of white spaces so split by colon to deal with white spaces
  ancestry_val <- strsplit(ancestry_val, ":")
  ancestry_val <- t(sapply(ancestry_val, function(x) trimws(x, which = "both")))
  ancestry_value <- matrix(,nrow = number_of_individuals,ncol = 0)
  for(col in 1:ncol(ancestry_val)){
    ancestry_val1 <- t(sapply(strsplit(ancestry_val[,col], " "), function(x) x[x != ""]))
    ancestry_value <- cbind(ancestry_value,ancestry_val1)
  }
  rownames(ancestry_value) <- ancestry_value[,2]
  ancestry_value <- ancestry_value[,3:ncol(ancestry_value)]
  ancestry_value[,1]<- gsub("[[:punct:]]","",ancestry_value[,1])
  ancestry_value <- data.frame(ancestry_value)
  #Add column names to the dataframe
  colnames(ancestry_value) <- c("percent_missing",paste("cluster_",1:length(infered_cluster$cluster),sep = ""))
  
  ###Estimated Allele Frequency###
  allele_start <- grep("Estimated Allele Frequencies in each cluster", mylines)
  allele_end <- grep("Values of parameters used in structure:", mylines)
  allele_freq <- mylines[(allele_start+4):(allele_end -2)]
  allele_freq <- trimws(allele_freq, which = "both")
  length(allele_freq)
  #remove newline characters = empty characeter strings
  allele_freq <- allele_freq[allele_freq != ""]
  #subset the various elements into string vectors 
  Locus_index <- grep("Locus", allele_freq)
  Locus <- allele_freq[Locus_index]
  missing_data <- allele_freq[Locus_index+2]
  Allele_A <- allele_freq[Locus_index+3]
  Allele_B <- allele_freq[Locus_index+4]
  #get the locus string into a matrix
  Locus_split <- strsplit(Locus, " ")
  Locus_final <- t(sapply(Locus_split, function(x) x[x !=":"]))
  #get the missing data well formatted
  missing <- trimws(gsub("([[:alpha:]]|%)","",missing_data), which = "both")
  #format Allele_A and b
  Allele_A_split <- t(sapply(strsplit(Allele_A, " "), function(x) x[x != ""]))
  Allele_A_split[,2] <- gsub("(\\(|\\))", "", Allele_A_split[,2])
  Allele_B_split <- t(sapply(strsplit(Allele_B, " "), function(x) x[x != ""]))
  Allele_B_split[,2] <- gsub("(\\(|\\))", "", Allele_B_split[,2])
  #Order alele A and B appropriately
  #make a coppy of both files and ensure allele one and two are well ordered
  Allele_1 <- Allele_A_split
  Allele_2 <- Allele_B_split
  Allele_1[which(Allele_A_split[,1] == 2),] <- Allele_B_split[which(Allele_A_split[,1] == 2),]
  Allele_2[which(Allele_B_split[,1] == 1),] <- Allele_A_split[which(Allele_B_split[,1] == 1),]
  #combine this to a datafframe
  allele_frequency <- cbind(Locus_final,missing,Allele_1,Allele_2)
  allele_frequency <- as.data.frame(allele_frequency) 
  
  #subset this by the empty line beween each loci
  close(mycon)
  structure_output <- list(run_parameters = run_parameters, infered_clusters = infered_cluster, 
                           HE = expected_heterozygosity, FST= mean_FST_value, ancestry_values = ancestry_value,
                           Estimate_allele_frequency = allele_frequency)

  # assign class
  class(structure_output) <- c("destruct", class(structure_output))
  return(structure_output)
}
x <- deStruct(file2)

