# deStructure
This repository carries functions to facilitate working with the program STRUCTURE.
Scripts which process STRUCTURE output have only been optimized for the ADMIXTURE model and the following settings:

This package has 6 functions

deStruct - parses structure output storing information into a S3 class object with relevant information

avarage_runs - This function averages across replications of STRUCTURE for a given value of K. Note that in this implementation, the user must ensure that the clusters are in a consistent order for the input.

structure_plot - Visualize individual allele ancestries, to delineate the likely level of population subdivision

mainparams -This functions generates the mainparams file needed for the program STRUCTURE v 2.3.

extraparams - This functions generates the extraparams file needed for the program STRUCTURE v 2.3.

structure_scriptwriter - This function writes scripts suitable for running STRUCTURE on a UNIX-based high-performance computing cluster up to a given number of populations and replications

#define LABEL 1
#define POPDATA 1 
#define POPFLAG 1 
#define LOCDATA 0 
#define PHENOTYPE 0 
#define MARKERNAMES 1 
#define MAPDISTANCES 0 
#define ONEROWPERIND 0 
#define PHASEINFO 0 
#define PHASED 0 
#define RECESSIVEALLELES 0 
#define EXTRACOLS 0
#define MISSING -9
#define PLOIDY 2
#define MAXPOPS 1
#define BURNIN 10000
#define NUMREPS 50000
#define NOADMIX 0
#define LINKAGE 0
#define USEPOPINFO 1
#define LOCPRIOR 0
#define INFERALPHA 1
#define ALPHA 1.0
#define POPALPHAS 0 
#define UNIFPRIORALPHA 1 
#define ALPHAMAX 10.0
#define ALPHAPROPSD 0.025
#define FREQSCORR 1 
#define ONEFST 0
#define FPRIORMEAN 0.01
#define FPRIORSD 0.05
#define INFERLAMBDA 0 
#define LAMBDA 1.0
#define COMPUTEPROB 1
#define MIGRPRIOR 0.00
#define GENSBACK 0 
#define PFROMPOPFLAGONLY 1 
#define ANCESTDIST 0 
#define STARTATPOPINFO 0 
#define METROFREQ 10
#define UPDATEFREQ 1 
