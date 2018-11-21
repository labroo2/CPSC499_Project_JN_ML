extraparams <- function(noadmix, linkage, usepopinfo, locprior, freqscorr, onefst, inferalpha, popalphas, alpha,
                        inferlambda, popspecificlambda, lambda, fpriormean, fpriorsd, unifprioralpha, alphamax,
                        log10rmin, log10rmax, log10propsd, log10rstart, gensback, migrprior, pfrompopflagonly,
                        locispop, locpriorinit, maxlocprior, printnet, printlambda, printqsum, sitebysite,
                        printqhat, updatefreq, printlikes, intermedsave, echodata, ancestdist, computeprob,
                        admburnin, alphapropsd, startatpopinfo, randomize, seed, metrofreq, reporthitrate){
  #check for errors
  #check that popinfo = 1 if gensback, migrprior, or pfrompopflagonly is specified
  if(gensback > 0 | is.na(gensback) == FALSE && usepopinfo != 1) warning("usepopinfo must be turned on with a value of 1 to implement gensback.")
  if(migrprior > 0 | is.na(migrprior) == FALSE && usepopinfo != 1) warning("usepopinfo must be turned on with a value of 1 to implement migrprior.")
  if(pfrompopflagonly > 0 | is.na(pfrompopflagonly) == FALSE && usepopinfo != 1) warning("usepopinfo must be turned on with a value of 1 to implement pfrompopflagonly.")
  
  #check that migrprior value is sensible
  if(migrprior < 0 | migrprior > 1) stop("migrprior must be between 0 and 1.")
  if(migrprior < 0.001 | migrprior > 0.1) warning("Sensible values of migrprior generally range from 0.001 to 0.1")
  
  #if sitebysite is on, check that linkage is on
  if(sitebysite > 0 && linkage < 1) stop("linkage model must be turned on with a value of 1 to implement the sitebysite option.")
  
  #if locispop, locpriorinit, or maxlocprior are on, make sure locprior is on
  if(locispop > 0 | locpriorinit > 0 | maxlocprior > 0 && locprior != 1) stop("locprior must be turned on with a value of one to implement locispop, locpriorinit, and maxlocprior.")
  
  #check that Boolean options are all either 0 or 1
  binary_options <- c(unifprioralpha, pfrompopflagonly, locispop, printnet, printlambda, printqsum, sitebysite,
                      printqhat, printlikes, echodata, ancestdist, computeprob, startatpopinfo, randomize, reporthitrate)
  names(binary_options) <- c("unifprioralpha", "pfrompopflagonly", "locispop", "printnet", "printlambda", "printqsum", "sitebysite",
                             "printqhat", "printlikes", "echodata", "ancestdist", "computeprob", "startatpopinfo", "randomize", "reporthitrate")
  for(i in 1:length(binary_options)){
    if(binary_options[i] != 0 && binary_options[i] != 1 && is.na(binary_options[i]) ==FALSE) stop(paste(names(binary_options[i])), "must be 0 or 1.")
  }
  
  #paste together the error-checked output file
  object_list <- c(noadmix, linkage, usepopinfo, locprior, freqscorr, onefst, inferalpha, popalphas, alpha,
                   inferlambda, popspecificlambda, lambda, fpriormean, fpriorsd, unifprioralpha, alphamax,
                   log10rmin, log10rmax, log10propsd, log10rstart, gensback, migrprior, pfrompopflagonly,
                   locispop, locpriorinit, maxlocprior, printnet, printlambda, printqsum, sitebysite,
                   printqhat, updatefreq, printlikes, intermedsave, echodata, ancestdist, computeprob,
                   admburnin, alphapropsd, startatpopinfo, randomize, seed, metrofreq, reporthitrate)
  names(object_list) <- c("#define NOADMIX", "#define LINKAGE", "#define USEPOPINFO", "#define LOCPRIOR",
                          "#define FREQSCORR", "#define ONEFST", "#define INFERALPHA", "#define POPALPHAS",
                          "#define ALPHA", "#define INFERLAMBDA", "#define POPSPECIFICLAMBDA", "#define LAMBDA",
                          "#define FPRIORMEAN", "#define FPRIORSD", "#define UNIFPRIORALPHA", "#define ALPHAMAX",
                          "#define LOG10RMIN", "#define LOG10RMAX", "#define LOG10PROPSD", "#define LOG10RSTART",
                          "#define GENSBACK", "#define MIGRPRIOR", "#define PFROMPOPFLAGONLY", "#define LOCISPOP",
                          "#define LOCPRIORINIT", "#define MAXLOCPRIOR", "#define PRINTNET", "#define PRINTLAMBDA",
                          "#define PRINTQSUM", "#define SITEBYSITE", "#define PRINTQHAT", "#define UPDATEFREQ",
                          "#define PRINTLIKES", "#define INTERMEDSAVE", "#define ECHODATA", "#define ANCESTDIST",
                          "#define COMPUTEPROB", "#define ADMBURNIN", "#define ALPHAPROPSD", "#define STARTATPOPINFO",
                          "#define RANDOMIZE", "#define SEED", "#define METROFREQ", "#define REPORTHITRATE")
  object_list <- object_list[is.na(object_list) == FALSE] #remove parameters the user is not interested in specifying

  holder <- c()
  for(i in 1: length(object_list)){
    holder[i] <- paste(names(object_list)[i], object_list[i], sep = " ")
  }
  #concatenate
  cat(holder, sep = "\n", file = "extraparams")
}
