\name{structure_scriptwriter}
\alias{structure_scriptwriter}
\title{
structure_scriptwriter
}
\description{
This function writes bash scripts suitable for running STRUCTURE on a UNIX-based high-performance computing cluster up to a given number of populations and replications.
}
\usage{
structure_scriptwriter(task_manager, module, mainparams = NULL, extraparams = NULL,
                        stratparams = NULL, populations, numloci = NULL,
                        numinds = NULL, input_file, output_file_base, seed = NULL,
                        reps_per_k, script_basename)
}
\arguments{
  \item{task_manager}{
(string) user-defined text to be placed at the head of each script which is intended to run a workload manager such as SLURM or TORQUE. 
}
  \item{module}{
(string) The name of the module to be loaded.
}
  \item{mainparams}{
(string) If using the -m flag to specify a mainparams file rather than detecting automatically from the working directory, specify that mainparams file name or path here.
}
  \item{extraparams}{
(string) If using the -e flag to specify a extraparams file rather than detecting automatically from the working directory, specify that extraparams file name or path here.
}
  \item{stratparams}{
(string) If the using the -s flag to specify a stratparams file, specify that stratparams file name or path here.
}
  \item{populations}{
(numeric) Value specifying through how many values of K scripts should be written. The function will output scripts for values of K from 1 to the number specified with populations.
}
  \item{numloci}{
(string) If overriding the number of loci provided in the mainparams file, give a character string of the number of loci to be used here for use with the -L flag.
}
  \item{numinds}{
(string) If overriding the number of individuals provided in the mainparams file, give the number of individuals to be used here for use with the -N flag.
}
  \item{input_file}{
(string) Specify the name of the input file for this run of STRUCTURE, to be used with the -i flag.
}
  \item{output_file_base}{
(string) Specify a basename for the output file from this run of STRUCTURE (number of populations and rep number will be added to the end)
}
  \item{seed}{
(string) If a seed value is to be specified and used with the -D flag, specify it here.
}
  \item{reps_per_k}{
(numeric) Specify the desired number of replications at a given value of K (number of populations).
}
  \item{script_basename}{
(string) Specify a basename for the scripts which will be output from this function, and used to run STRUCTURE.
}
}
\references{
https://web.stanford.edu/group/pritchardlab/structure.html

Falush, D., Stephens, M. & Pritchard, J. K. Inference of population structure using multilocus genotype data: linked loci and correlated allele frequencies. Genetics 164, 1567-1587 (2003)
}
\author{
Marlee Labroo & Joyce Njuguna
}
\examples{
#set a temporary working directory to hold output files
setwd(tempdir())

#run structure_scriptwriter; output is in the temporary working directory
structure_scriptwriter(task_manager = NULL, module = "structurev2.3", 
                        mainparams = NULL, extraparams = NULL, stratparams = NULL,
                        populations = 3, numloci = NULL, numinds = NULL,
                        input_file = "mydata.txt", output_file_base = "my_output_file",
                        seed = NULL, reps_per_k = 3, script_basename = "myscript")
}
