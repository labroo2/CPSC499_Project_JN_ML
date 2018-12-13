#this function generates the mainparams file needed for STRUCTURE
mainparams <- function(maxpops = NULL, burnin = 10000, numreps = 50000, infile, outfile, numinds,
                       numloci, ploidy = 2, missing = -9, onerowperind = 0, label = 1,
                       popdata = 0, popflag = 0, locdata = 0, phenotype = 0, extracols = 0,
                       markernames = 1, recessivealleles = 0, mapdistances = 0, phased = 0,
                       phaseinfo = 0, markovphase = NULL, notambiguous = NULL, outpath){
  #check to make sure outfile is file prefix only
  if(length(grep("\\.", outfile)) != 0){
    warning("The outfile name includes a period-- check that only the file prefix
            was specified, not the extension.")
  }
  if(length(grep(".txt$", outfile)) != 0 | length(grep(".csv$", outfile) != 0)){
    warning("The outfile parameter appears to include a .txt or .csv extension.")
  }
  
  #check that infile is a .txt file
  if(length(grep(".txt$", infile)) != 1){
    stop("The infile must specify a text file with extension .txt.")
  }
  
  #check that infile and outfile are 30 characters or fewer
  if(nchar(outfile) > 30) stop("The outfile prefix must be fewer than 30 characters.")
  if(nchar(infile) > 30) stop("The infile prefix must be fewer than 30 characters.")
  
  #check that numinds and numloci are greater than 0
  if(numinds < 1) stop("numinds must be greater than 0")
  if(numloci < 1) stop("numloci must be greater than 0")
  
  #check that Boolean options are all either 0 or 1
  binary_options <- c(onerowperind, label, popdata, popflag, locdata, phenotype,
                      markernames, recessivealleles, mapdistances, phased,
                      phaseinfo, markovphase)
  names(binary_options) <- c(if(!is.null(onerowperind)) "onerowperind", 
                             if(!is.null(label)) "label", 
                             if(!is.null(popdata)) "popdata", 
                             if(!is.null(popflag)) "popflag",
                             if(!is.null(locdata)) "locdata", 
                             if(!is.null(phenotype)) "phenotype", 
                             if(!is.null(markernames)) "markernames", 
                             if(!is.null(recessivealleles)) "recessivealleles", 
                             if(!is.null(mapdistances)) "mapdistances", 
                             if(!is.null(phased)) "phased", 
                             if(!is.null(phaseinfo)) "phaseinfo", 
                             if(!is.null(markovphase)) "markovphase")
  for(i in 1:length(binary_options)){
    if(!is.na(binary_options[i])){
      if(binary_options[i] != 0 && binary_options[i] != 1){
        stop(paste(names(binary_options[i])), " must be 0 or 1.")
      }
    }
  }
  
  #notify the user that missing is usually set to -9, 0 may not mean missing data, and NA is not acceptable
  if(missing != -9){
    warning("By convention, missing is usually set to -9, though any value that doesn't occur
            elsewhere in the dataset is OK.")
  }
  if(missing == 0) warning("Are you sure that 0 indicates missing data in your dataset?")
  if(is.na(missing) == TRUE){
    stop("missing data value must be a number that does not occur elsewhere in the dataset, not NA.")
  }
  
  #make sure numind and numloci are integers for printing purposes
  numinds <- as.integer(numinds)
  numloci <- as.integer(numloci)
  
  #paste together the error-checked output file
  object_list <- c(infile, outfile, numinds, numloci, label, popdata, popflag, locdata,
                   phenotype, markernames, mapdistances, onerowperind, phaseinfo, phased,
                   recessivealleles, extracols, missing, ploidy, maxpops, burnin, numreps,
                   markovphase, notambiguous)
  name_list <- c(if(!is.null(infile)) "#define INFILE", 
                 if(!is.null(outfile)) "#define OUTFILE", 
                 if(!is.null(numinds)) "#define NUMINDS", 
                 if(!is.null(numloci)) "#define NUMLOCI",
                 if(!is.null(label)) "#define LABEL", 
                 if(!is.null(popdata)) "#define POPDATA", 
                 if(!is.null(popflag)) "#define POPFLAG", 
                 if(!is.null(locdata)) "#define LOCDATA",
                 if(!is.null(phenotype)) "#define PHENOTYPE",
                 if(!is.null(markernames)) "#define MARKERNAMES",
                 if(!is.null(mapdistances)) "#define MAPDISTANCES",
                 if(!is.null(onerowperind)) "#define ONEROWPERIND", 
                 if(!is.null(phaseinfo)) "#define PHASEINFO", 
                 if(!is.null(phased)) "#define PHASED",
                 if(!is.null(recessivealleles)) "#define RECESSIVEALLELES", 
                 if(!is.null(extracols)) "#define EXTRACOLS", 
                 if(!is.null(missing)) "#define MISSING",
                 if(!is.null(ploidy)) "#define PLOIDY", 
                 if(!is.null(maxpops)) "#define MAXPOPS", 
                 if(!is.null(burnin)) "#define BURNIN", 
                 if(!is.null(numreps)) "#define NUMREPS",
                 if(!is.null(markovphase)) "#define MARKOVPHASE",
                 if(!is.null(notambiguous)) "#define NOTAMBIGUOUS")
  #remove parameters the user is not interested in specifying
  object_list <- object_list[!is.null(object_list)]
  name_list <- name_list[!is.null(name_list)]
  
  #paste the options and their names 
  holder <- paste(name_list, object_list, sep = " ")
  
  #concatenate
  cat(holder, sep = "\n", file = outpath)
}