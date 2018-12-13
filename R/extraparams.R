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
  if(!is.null(gensback) && !is.null(usepopinfo)){
    if(gensback > 0 && usepopinfo != 1){
      warning("usepopinfo must be turned on with a value of 1 to implement gensback.")
    }
  }

  if(!is.null(migrprior) && !is.null(usepopinfo)){
    if(migrprior > 0 && usepopinfo != 1){
      warning("usepopinfo must be turned on with a value of 1 to implement migrprior.")
    }
  }
  
  if(!is.null(pfrompopflagonly) && !is.null(usepopinfo)){
    if(pfrompopflagonly > 0 && usepopinfo != 1){
    warning("usepopinfo must be turned on with a value of 1 to implement pfrompopflagonly.")
    }
  }
  
  #check that migrprior value is sensible
  if(!is.null(migrprior)){
    if(migrprior < 0 | migrprior > 1) stop("migrprior must be between 0 and 1.")
  }
  if(!is.null(migrprior)){
    if(migrprior < 0.001 | migrprior > 0.1){
      warning("Sensible values of migrprior generally range from 0.001 to 0.1")
    }
  }
  
  #if sitebysite is on, check that linkage is on
  if(!is.null(sitebysite) && !is.null(linkage)){
    if(sitebysite > 0 && linkage < 1){
    stop("linkage model must be turned on with a value of 1 to implement the sitebysite option.")
    }
  }
  
  #if locispop, locpriorinit, or maxlocprior are on, make sure locprior is on
  if(!is.null(locispop) && !is.null(locpriorinit) &&
     !is.null(maxlocprior) && !is.null(locprior)){
    if(locispop > 0 | locpriorinit > 0 | maxlocprior > 0 && locprior != 1){
      stop("locprior must be turned on with a value of one to implement locispop,
         locpriorinit, and maxlocprior.")
    }
  }
  
  #check that Boolean options are all either 0 or 1
  binary_options <- c(unifprioralpha, pfrompopflagonly, locispop, printnet, 
                      printlambda, printqsum, sitebysite, printqhat, 
                      printlikes, echodata, ancestdist, computeprob, startatpopinfo,
                      randomize, reporthitrate)
  names(binary_options) <- c(if(!is.null(unifprioralpha))"unifprioralpha",
                             if(!is.null(pfrompopflagonly)) "pfrompopflagonly",
                             if(!is.null(locispop)) "locispop",
                             if(!is.null(printnet)) "printnet",
                             if(!is.null(printlambda)) "printlambda",
                             if(!is.null(printqsum)) "printqsum",
                             if(!is.null(sitebysite)) "sitebysite",
                             if(!is.null(printqhat)) "printqhat",
                             if(!is.null(printlikes)) "printlikes",
                             if(!is.null(echodata)) "echodata",
                             if(!is.null(ancestdist)) "ancestdist",
                             if(!is.null(computeprob)) "computeprob",
                             if(!is.null(startatpopinfo)) "startatpopinfo",
                             if(!is.null(randomize)) "randomize",
                             if(!is.null(reporthitrate)) "reporthitrate")
  for(i in 1:length(binary_options)){
    if(!is.null(binary_options[i])){
      if(binary_options[i] != 0 && binary_options[i] != 1){
        stop(paste(names(binary_options[i])), "must be 0 or 1.")
      }
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
  name_list <- c(if(!is.null(noadmix)) "#define NOADMIX",
                if(!is.null(linkage)) "#define LINKAGE",
                if(!is.null(usepopinfo)) "#define USEPOPINFO",
                if(!is.null(locprior)) "#define LOCPRIOR", 
                if(!is.null(freqscorr)) "#define FREQSCORR", 
                if(!is.null(onefst)) "#define ONEFST",
                if(!is.null(inferalpha)) "#define INFERALPHA", 
                if(!is.null(popalphas))"#define POPALPHAS", 
                if(!is.null(alpha))"#define ALPHA",
                if(!is.null(inferlambda))"#define INFERLAMBDA", 
                if(!is.null(popspecificlambda))"#define POPSPECIFICLAMBDA", 
                if(!is.null(lambda))"#define LAMBDA",
                if(!is.null(fpriormean)) "#define FPRIORMEAN", 
                if(!is.null(fpriorsd))"#define FPRIORSD", 
                if(!is.null(unifprioralpha))"#define UNIFPRIORALPHA",
                if(!is.null(alphamax)) "#define ALPHAMAX", 
                if(!is.null(log10rmin))"#define LOG10RMIN", 
                if(!is.null(log10rmax)) "#define LOG10RMAX",
                if(!is.null(log10propsd)) "#define LOG10PROPSD", 
                if(!is.null(log10rstart))"#define LOG10RSTART", 
                if(!is.null(gensback))"#define GENSBACK",
                if(!is.null(migrprior)) "#define MIGRPRIOR", 
                if(!is.null(pfrompopflagonly))"#define PFROMPOPFLAGONLY", 
                if(!is.null(locispop))"#define LOCISPOP",
                if(!is.null(locpriorinit)) "#define LOCPRIORINIT", 
                if(!is.null(maxlocprior))"#define MAXLOCPRIOR", 
                if(!is.null(printnet))"#define PRINTNET",
                if(!is.null(printlambda)) "#define PRINTLAMBDA", 
                if(!is.null(printqsum))"#define PRINTQSUM", 
                if(!is.null(sitebysite)) "#define SITEBYSITE",
                if(!is.null(printqhat))"#define PRINTQHAT", 
                if(!is.null(updatefreq))"#define UPDATEFREQ", 
                if(!is.null(printlikes))"#define PRINTLIKES",
                if(!is.null(intermedsave))"#define INTERMEDSAVE", 
                if(!is.null(echodata))"#define ECHODATA",
                if(!is.null(ancestdist))"#define ANCESTDIST",
                if(!is.null(computeprob))"#define COMPUTEPROB", 
                if(!is.null(admburnin))"#define ADMBURNIN", 
                if(!is.null(alphapropsd))"#define ALPHAPROPSD",
                if(!is.null(startatpopinfo)) "#define STARTATPOPINFO", 
                if(!is.null(randomize))"#define RANDOMIZE", 
                if(!is.null(seed))"#define SEED",
                if(!is.null(metrofreq)) "#define METROFREQ", 
                if(!is.null(reporthitrate))"#define REPORTHITRATE")
  #remove parameters the user is not interested in specifying
  object_list <- object_list[!is.null(object_list)]
  name_list <- name_list[!is.null(name_list)]

  #paste together the options
  holder <- paste(name_list, object_list, sep = " ")
  #concatenate
  cat(holder, sep = "\n", file = "extraparams")
}