#Function to formart run parameters from the lines in structure file to a dataframe
get_run_param <- function(run_param_lines){
  run_params <- strsplit(gsub(" ", "", run_param_lines), " ")
  run_parameter <- data.frame(parameter = gsub("[[:digit:]]", "", run_params),
                              Value = gsub("([[:alpha:]]|[[:punct:]])", "", run_params), stringsAsFactors = FALSE)
  return(run_parameter)
}

#Function to format inferred clusters to a data frame
get_inferred_clus <- function(inferred_clus_lines){
  inferred_clus <- strsplit(inferred_clus_lines, " ")
  inferred_clus <- sapply(inferred_clus, function(x) x[x != ""])
  inferred_cluster <- as.data.frame(inferred_clus)
  colnames(inferred_cluster) <-c("cluster","proportion")
  #convert the column data type from charater to numeric
  for(i in 1:ncol(inferred_cluster)){
    inferred_cluster[ ,i] <- as.numeric(as.character(inferred_cluster[ ,i]))
  }
  return(inferred_cluster)
}

#Functionto formart Expected heterozygosity lines into a dataframe
get_expected_heterozygosity <- function(HE_lines){
  heterozygosity <- strsplit(HE_lines, " ")
  heterozygosity <- t(sapply(heterozygosity, function(x) x[x != ""]))
  expected_heterozygosity <- as.data.frame(heterozygosity[ ,c(2,4)])
  colnames(expected_heterozygosity) <-c("cluster","HE")#add column names
  #convert the column data type from charater to numeric
  for(i in 1:ncol(expected_heterozygosity)){
    expected_heterozygosity[,i] <- as.numeric(as.character(expected_heterozygosity[,i]))
  }
  return(expected_heterozygosity)
}

#function to formart FST values into a data frame
get_FST_values <- function(FST_values_lines){
  FST_values <- strsplit(FST_values_lines, "=")
  FST_values <- t(sapply(FST_values, function(x)  x[x != ""]))
  #remove white spaces to the right
  mean_FST_value <- as.data.frame(trimws(FST_values,which = "right"))
  colnames(mean_FST_value) <- c("cluster","FST")#add column names
  #convert the column data type from charater to numeric
  for(i in 2:ncol(mean_FST_value)){
    mean_FST_value[,i] <- as.numeric(as.character(mean_FST_value[,i]))
  }
  mean_FST_value[,1] <- paste("Mean value of Fst_", 1:nrow(mean_FST_value), sep = "") #formart first column
  return(mean_FST_value)
}

#function to format the individual ancestry values into a dataframe 
get_ancestry_value <- function(ancestry_val_lines,number_of_individuals, inferred_cluster){
  #there is alot of white spaces so split by colon to deal with white space
  ancestry_val <- strsplit(ancestry_val_lines, ":")
  ancestry_val <- t(sapply(ancestry_val, function(x) trimws(x, which = "both")))
  ancestry_value <- matrix( , nrow = number_of_individuals, ncol = 0) #empty matrix to hold ancestry values
  #split each of the columns by space and bind them into a matrix
  for(col in 1:ncol(ancestry_val)){
    ancestry_val1 <- t(sapply(strsplit(ancestry_val[ ,col], " "), function(x) x[x != ""]))
    ancestry_value <- cbind(ancestry_value, ancestry_val1)
  }
  rownames(ancestry_value) <- ancestry_value[,2]
  ancestry_value <- ancestry_value[,3:ncol(ancestry_value)]
  ancestry_value[,1]<- gsub("[[:punct:]]", "", ancestry_value[,1])
  ancestry_value <- data.frame(ancestry_value)
  #Add column names to the dataframe
  colnames(ancestry_value) <- c("percent_missing", paste("cluster_", 1:length(inferred_cluster$cluster), sep = ""))
  for(i in 1:ncol(ancestry_value)){
    ancestry_value[,i] <- as.numeric(as.character(ancestry_value[,i]))
  }
  return(ancestry_value)
}

#function to format allelewise ancestry values
get_allele_frequency <- function(allele_freq_lines){
  #remove white spaces on both sides of the string
  allele_freq <- trimws(allele_freq_lines, which = "both")
  #remove newline characters = empty characeter strings
  allele_freq <- allele_freq[allele_freq != ""]
  #subset the various elements into string vectors 
  Locus_index <- grep("Locus", allele_freq)
  Locus <- allele_freq[Locus_index]
  missing_data <- allele_freq[Locus_index + 2]
  Allele_A <- allele_freq[Locus_index + 3]
  Allele_B <- allele_freq[Locus_index + 4]
  #get the locus string into a matrix
  Locus_split <- strsplit(Locus, " ")
  Locus_final <- t(sapply(Locus_split, function(x) x[x !=":"]))
  #get the missing data well formatted
  missing <- trimws(gsub("([[:alpha:]]|%)", "", missing_data), which = "both")
  #format Allele_A and b
  Allele_A_split <- t(sapply(strsplit(Allele_A, " "), function(x) x[x != ""]))
  Allele_A_split[,2] <- gsub("(\\(|\\))", "", Allele_A_split[,2])
  Allele_B_split <- t(sapply(strsplit(Allele_B, " "), function(x) x[x != ""]))
  Allele_B_split[,2] <- gsub("(\\(|\\))", "", Allele_B_split[,2])
  #Order allele A and B appropriately
  #make a copy of both files and ensure allele one and two are well ordered
  Allele_1 <- Allele_A_split
  Allele_2 <- Allele_B_split
  Allele_1[which(Allele_A_split[,1] == 2),] <- Allele_B_split[which(Allele_A_split[,1] == 2),]
  Allele_2[which(Allele_B_split[,1] == 1),] <- Allele_A_split[which(Allele_B_split[,1] == 1),]
  Allele_1 <- Allele_1[,2:ncol(Allele_1)]
  Allele_2 <- Allele_2[,2:ncol(Allele_2)]
  colnames(Allele_1) <- c("Proportion_A1", paste("Allele1_clust", 1:(ncol(Allele_1)-1), sep = ""))
  colnames(Allele_2) <- c("Proportion_A2", paste("Allele2_clust", 1:(ncol(Allele_2)-1), sep = ""))
  #combine this to a dataframe
  allele_frequency <- cbind(Locus_final[,3], missing, Allele_1, Allele_2)
  allele_frequency <- as.data.frame(allele_frequency)
  names(allele_frequency)[1] <- "Locus"
  #convert column data trypes into numeric
  for(i in 2:ncol(allele_frequency)){
    allele_frequency[,i] <- as.numeric(as.character(allele_frequency[,i]))
  }
  return(allele_frequency)
}

