#this function averages values across runs of STRUCTURE at a given value of k for objects of the class deStruct
#because clusters may be ordered differently in the file across replications, the user must rearrange the deStruct so that a fixed column references a given cluster
#in the future, we will implement an option for the function to guess the cluster orders

average_runs <- function(deStruct_list){
  #save the run parameters
  run_params <- as.data.frame(deStruct_list[[1]][1])
  colnames(run_params) <- c("parameter", "Value")
  #average the inferred_clusters
  inferred_clusters <- lapply(deStruct_list, '[[', 2) #get inferred clusters slots from deStruct list
  #convert factor to numeric
  num_clust <- list()
  for(i in 1:length(inferred_clusters)){
    num_clust[[i]] <- as.vector(as.numeric(as.character(inferred_clusters[[i]][[2]])))
  }
  datframe_clust <- as.data.frame(num_clust, fix.empty.names = FALSE) #convert list to dataframe
  clust_means <- rowMeans(datframe_clust) #get cluster averages
  #reformat to be compatible with deStruct class
  inferred_clusters_averaged <- as.data.frame(cbind(1:length(clust_means), clust_means))
  colnames(inferred_clusters_averaged) <- c("cluster", "proportion")
  
  #average the HE values-- same style as above
  HE_values <- lapply(deStruct_list, '[[', 3)
  num_HE <- list()
  for(i in 1:length(HE_values)){
    num_HE[[i]] <- as.numeric(as.character(HE_values[[i]][[2]]))
  }
  datframe_HE <- as.data.frame(num_HE, fix.empty.names = FALSE)
  HE_means <- rowMeans(datframe_HE)
  HE_averaged <- as.data.frame(cbind(1:length(HE_means), HE_means))
  colnames(HE_averaged) <- c("cluster", "HE")
  #paste in the deStruct format for the first column of the HE slot
  for(i in 1:length(HE_averaged[,1])){
    HE_averaged[i,1] <- paste("Mean value of Fst_", HE_averaged[i,1], sep = "")
  }
  
  #average the FST values-- same style as above
  FST_values <- lapply(deStruct_list, '[[', 4)
  num_FST <- list()
  for(i in 1:length(FST_values)){
    num_FST[[i]] <- as.numeric(as.character(FST_values[[i]][[2]]))
  }
  datframe_FST <- as.data.frame(num_FST, fix.empty.names = FALSE)
  FST_means <- rowMeans(datframe_FST)
  FST_averaged <- as.data.frame(cbind(1:length(FST_means), FST_means))
  colnames(FST_averaged) <- c("cluster", "FST")
  
  #average the individual_ancestry_frequencies
  ind_qvals <- lapply(deStruct_list, '[[', 5)
  #convert factors to numeric for all columns at once
  for(i in 1:length(ind_qvals)){
    for(j in 1:length(ind_qvals[[1]]))
      ind_qvals[[i]][[j]] <- as.numeric(as.character(ind_qvals[[i]][[j]]))
  }
  #make an array of the three ancestry dataframes
 ind_array <- array(unlist(ind_qvals), c(dim(ind_qvals[[1]])[1], dim(ind_qvals[[1]])[2], length(ind_qvals)))
 ind_average <- apply(ind_array, 1:2, mean) #average across the 3rd dimension of the array for each row and column
 dimnames(ind_average) <- list(rownames(ind_qvals[[1]]), colnames(ind_qvals[[1]])) #name the resulting dataframe consistently
 ind_averaged <- as.data.frame(ind_average) #make compatible with deStruct class
  
  #average the allelewise ancestry frequencies
  allele_freqs <- lapply(deStruct_list, '[[', 6)
  for(i in 1:length(allele_freqs)){
    for(j in 2:length(allele_freqs[[1]])){
      allele_freqs[[i]][[j]] <- as.numeric(as.character(allele_freqs[[i]][[j]]))
    }
  }
  allele_array <- array(unlist(allele_freqs), c(dim(allele_freqs[[1]])[1], dim(allele_freqs[[1]])[2], length(allele_freqs)))
  allele_average <- apply(allele_array, 1:2, mean)
  dimnames(allele_average) <- list(rownames(allele_freqs[[1]]), colnames(allele_freqs[[1]]))
  allele_averaged <- as.data.frame(allele_average)
  allele_averaged[,1] <- allele_array[[1]][1] #fix the locus column, which can't be converted to numeric in workflow above
  names(allele_averaged[,1]) <- "Locus" #name the locus column
  
  #convert all numeric values back to factors to be compatible with deStruct
  for(i in 1:ncol(inferred_clusters_averaged)){
    inferred_clusters_averaged[,i] <- as.factor(inferred_clusters_averaged[,i])
  }
  for(i in 1:ncol(HE_averaged)){
    HE_averaged[,i] <- as.factor(HE_averaged[,i])
  }
  for(i in 1:ncol(FST_averaged)){
    FST_averaged[,i] <- as.factor(FST_averaged[,i])
  }
  for(i in 1:ncol(ind_averaged)){
    ind_averaged[,i] <- as.factor(ind_averaged[,i])
  }
  for(i in 1:ncol(allele_averaged)){
    allele_averaged[,i] <- as.factor(allele_averaged[,i])
  }
  
  #make deStruct output
  averaged_deStruct <- list(run_parameters = run_params, inferred_clusters = inferred_clusters_averaged, HE = HE_averaged,
                            FST = FST_averaged, individual_ancestry_frequences = ind_averaged, allelewise_ancestry_frequency = allele_averaged)
  #assign class
  class(averaged_deStruct) <- c("destruct", class(averaged_deStruct))
  return(averaged_deStruct)
}
