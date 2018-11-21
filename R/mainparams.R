mainparams <- function(maxpops, burnin, numreps, infile, outfile, numinds, numloci, ploidy, missing, onerowperind, label, popdata,
                       popflag, locdata, phenotype, extracols, markernames, recessivealleles, mapdistances,
                       phased, phaseinfo, markovphase, notambiguous){
  #check to make sure outfile is file prefix only
  if(length(grep("\\.", outfile)) != 0) warning("The outfile name includes a period-- check that only the file prefix was specified, not the extension.")
  if(length(grep(".txt$", outfile)) != 0 | length(grep(".csv$", outfile) != 0)) warning("The outfile parameter appears to include a .txt or .csv extension.")
  
  #check that infile is a .txt file
  if(length(grep(".txt$", infile)) != 1) stop("The infile must specify a text file with extension .txt.")
  
  #check that infile and outfile are 30 characters or fewer
  if(nchar(outfile) > 30) stop("The outfile prefix must be fewer than 30 characters.")
  if(nchar(infile) > 30) stop("The infile prefix must be fewer than 30 characters.")
  
  #check that numinds and numloci are greater than 0
  if(numinds < 1) stop("numinds must be greater than 0")
  if(numloci < 1) stop("numloci must be greater than 0")
  
  #check that Boolean options are all either 0 or 1
  binary_options <- c(onerowperind, label, popdata, popflag, locdata, phenotype, markernames, recessivealleles, mapdistances, phased, phaseinfo,
                      markovphase)
  names(binary_options) <- c("onerowperind", "label", "popdata", "popflag", "locdata", "phenotype", "markernames", "recessivealleles", 
                             "mapdistances", "phased", "phaseinfo", "markovphase")
  for(i in 1:length(binary_options)){
    if(binary_options[i] != 0 && binary_options[i] != 1 && is.na(binary_options[i]) == FALSE) stop(paste(names(binary_options[i])), " must be 0 or 1.")
  }
  
  #notify the user that missing is usually set to -9, 0 may not mean missing data, and NA is not acceptable
  if(missing != -9) warning("By convention, missing is usually set to -9, though any value that doesn't occur elsewhere in the dataset is OK.")
  if(missing == 0) warning("Are you sure that 0 indicates missing data in your dataset?")
  if(is.na(missing) == TRUE) warning("Missing must be a number that does not occur elsewhere in the dataset, not NA.")
  
  #make sure numind and numloci are integers for printing purposes
  numinds <- as.integer(numinds)
  numloci <- as.integer(numloci)
  
  #paste together the error-checked output file
  object_list <- c(infile, outfile, numinds, numloci, label, popdata, popflag, locdata, phenotype, markernames, mapdistances, onerowperind, phaseinfo, phased, recessivealleles,
                   extracols, missing, ploidy, maxpops, burnin, numreps, markovphase, notambiguous)
  names(object_list) <- c("#define INFILE", "#define OUTFILE", "#define NUMINDS", "#define NUMLOCI", "#define LABEL", "#define POPDATA",
                          "#define POPFLAG", "#define LOCDATA", "#define PHENOTYPE", "#define MARKERNAMES", "#define MAPDISTANCES",
                          "#define ONEROWPERIND", "#define PHASEINFO", "#define PHASED", "#define RECESSIVEALLELES",
                          "#define EXTRACOLS", "#define MISSING", "#define PLOIDY", "#define MAXPOPS", "#define BURNIN",
                          "#define NUMREPS", "#define MARKOVPHASE", "#define NOTAMBIGUOUS")
  object_list <- object_list[is.na(object_list) == FALSE] #remove parameters the user is not interested in specifying

  holder <- c()
  for(i in 1:length(object_list)){
    holder[i] <- paste(names(object_list)[i], object_list[i], sep = " ")
  }
  #concatenate
  cat(holder, sep = "\n", file = "mainparams")
}
