################################################################################
#this function generates the extraparams file needed for STRUCTURE
extraparams <- function(noadmix = 0, linkage = 0, usepopinfo = 0, locprior = 0, 
                        freqscorr = 1, onefst = 0, inferalpha = 1, popalphas = 0,
                        alpha = 1.0, inferlambda = 0, popspecificlambda = NULL,
                        lambda = 1.0, fpriormean = 0.01, fpriorsd = 0.05,
                        unifprioralpha = 1, alphamax = 10.0, log10rmin = NULL,
                        log10rmax = NULL, log10propsd = NULL, log10rstart = NULL,
                        gensback = NULL, migrprior = NULL, pfrompopflagonly = 0,
                        locispop = NULL, locpriorinit = NULL, maxlocprior = NULL,
                        printnet = NULL, printlambda = NULL, printqsum = NULL,
                        sitebysite = NULL, printqhat = NULL, updatefreq = NULL,
                        printlikes = NULL, intermedsave = NULL, echodata = NULL,
                        ancestdist = 0, computeprob = 1, admburnin = NULL,
                        alphapropsd = 0.025, startatpopinfo = 0, randomize = NULL,
                        seed = NULL, metrofreq = 10, reporthitrate = NULL){
  #check for errors
  #check that popinfo = 1 if gensback, migrprior, or pfrompopflagonly is specified
  if(gensback > 0 | is.null(gensback) == FALSE && usepopinfo != 1){
    warning("usepopinfo must be turned on with a value of 1 to implement gensback.")
  }
  if(migrprior > 0 | is.null(migrprior) == FALSE && usepopinfo != 1){
    warning("usepopinfo must be turned on with a value of 1 to implement migrprior.")
  }
  if(pfrompopflagonly > 0 | is.null(pfrompopflagonly) == FALSE && usepopinfo != 1){
    warning("usepopinfo must be turned on with a value of 1 to implement pfrompopflagonly.")
  }
  
  #check that migrprior value is sensible
  if(migrprior < 0 | migrprior > 1) stop("migrprior must be between 0 and 1.")
  if(migrprior < 0.001 | migrprior > 0.1){
    warning("Sensible values of migrprior generally range from 0.001 to 0.1")
  }
  
  #if sitebysite is on, check that linkage is on
  if(sitebysite > 0 && linkage < 1){
    stop("linkage model must be turned on with a value of 1 to implement the sitebysite option.")
  }
  
  #if locispop, locpriorinit, or maxlocprior are on, make sure locprior is on
  if(locispop > 0 | locpriorinit > 0 | maxlocprior > 0 && locprior != 1){
    stop("locprior must be turned on with a value of one to implement locispop,
         locpriorinit, and maxlocprior.")
  }
  
  #check that Boolean options are all either 0 or 1
  binary_options <- c(unifprioralpha, pfrompopflagonly, locispop, printnet, 
                      printlambda, printqsum, sitebysite, printqhat, 
                      printlikes, echodata, ancestdist, computeprob, startatpopinfo,
                      randomize, reporthitrate)
  names(binary_options) <- c("unifprioralpha", "pfrompopflagonly", "locispop",
                             "printnet", "printlambda", "printqsum", "sitebysite",
                             "printqhat", "printlikes", "echodata", "ancestdist",
                             "computeprob", "startatpopinfo", "randomize", "reporthitrate")
  for(i in 1:length(binary_options)){
    if(binary_options[i] != 0 && binary_options[i] != 1 && is.null(binary_options[i]) == FALSE){
      stop(paste(names(binary_options[i])), "must be 0 or 1.")
    }
  }
  
  #paste together the error-checked output file
  object_list <- c(noadmix, linkage, usepopinfo, locprior, freqscorr, onefst, inferalpha,
                   popalphas, alpha, inferlambda, popspecificlambda, lambda, fpriormean,
                   fpriorsd, unifprioralpha, alphamax, log10rmin, log10rmax, log10propsd,
                   log10rstart, gensback, migrprior, pfrompopflagonly, locispop, 
                   locpriorinit, maxlocprior, printnet, printlambda, printqsum, sitebysite,
                   printqhat, updatefreq, printlikes, intermedsave, echodata, ancestdist,
                   computeprob, admburnin, alphapropsd, startatpopinfo, randomize, seed,
                   metrofreq, reporthitrate)
  names(object_list) <- c("#define NOADMIX", "#define LINKAGE", "#define USEPOPINFO",
                          "#define LOCPRIOR", "#define FREQSCORR", "#define ONEFST",
                          "#define INFERALPHA", "#define POPALPHAS", "#define ALPHA",
                          "#define INFERLAMBDA", "#define POPSPECIFICLAMBDA", "#define LAMBDA",
                          "#define FPRIORMEAN", "#define FPRIORSD", "#define UNIFPRIORALPHA",
                          "#define ALPHAMAX", "#define LOG10RMIN", "#define LOG10RMAX",
                          "#define LOG10PROPSD", "#define LOG10RSTART", "#define GENSBACK",
                          "#define MIGRPRIOR", "#define PFROMPOPFLAGONLY", "#define LOCISPOP",
                          "#define LOCPRIORINIT", "#define MAXLOCPRIOR", "#define PRINTNET",
                          "#define PRINTLAMBDA", "#define PRINTQSUM", "#define SITEBYSITE",
                          "#define PRINTQHAT", "#define UPDATEFREQ", "#define PRINTLIKES",
                          "#define INTERMEDSAVE", "#define ECHODATA", "#define ANCESTDIST",
                          "#define COMPUTEPROB", "#define ADMBURNIN", "#define ALPHAPROPSD",
                          "#define STARTATPOPINFO", "#define RANDOMIZE", "#define SEED",
                          "#define METROFREQ", "#define REPORTHITRATE")
  #remove parameters the user is not interested in specifying
  object_list <- object_list[is.null(object_list) == FALSE] 

  #paste together the options
  holder <- paste(names(object_list), object_list, sep = " ")
  #concatenate
  cat(holder, sep = "\n", file = "extraparams")
}