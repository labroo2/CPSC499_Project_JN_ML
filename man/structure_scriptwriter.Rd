\name{structure_scriptwriter}
\alias{structure_scriptwriter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
structure_scriptwriter
}
\description{
This function writes scripts suitable for running STRUCTURE on a UNIX-based high-performance computing cluster up to a given number of populations and replications.
}
\usage{
structure_scriptwriter(task_manager, module, mainparams, extraparams, stratparams, populations, numloci, numinds, input_file, output_file_base, seed, reps_per_k, script_basename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{task_manager}{
An object which holds user-defined text to be placed at the head of each script, and is intended to interact with a workload manager such as SLURM or TORQUE. 
}
  \item{module}{
The name of the module to be loaded.
}
  \item{mainparams}{
If using the -m flag to specify a mainparams file rather than detecting automatically from the working directory, specify that mainparams file name or path here.
}
  \item{extraparams}{
If using the -e flag to specify a extraparams file rather than detecting automatically from the working directory, specify that extraparams file name or path here.
}
  \item{stratparams}{
If the using the -s flag to specify a stratparams file, specify that stratparams file name or path here.
}
  \item{populations}{
Specify through how many values of K scripts should be written. The function will output scripts for values of K from 1 to the number specified with populations.
}
  \item{numloci}{
If overriding the number of loci provided in the mainparams file, give the number of loci to be used here for use with the -L flag.
}
  \item{numinds}{
If overriding the number of individuals provided in the mainparams file, give the number of individuals to be used here for use with the -N flag.
}
  \item{input_file}{
Specify the name of the input file for this run of STRUCTURE, to be used with the -i flag.
}
  \item{output_file_base}{
Specify a basename for the output file from this run of STRUCTURE (number of populations and rep number will be added to the end)
}
  \item{seed}{
If a seed value is to be specified and used with the -D flag, specify it here.
}
  \item{reps_per_k}{
Specify the desired number of replications at a given value of K (number of populations).
}
  \item{script_basename}{
Specify a basename for the scripts which will be output from this function, and used to run STRUCTURE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://web.stanford.edu/group/pritchardlab/structure.html

Falush, D., Stephens, M. & Pritchard, J. K. Inference of population structure using multilocus genotype data: linked loci and correlated allele frequencies. Genetics 164, 1567–1587 (2003)
}
\author{
Marlee Labroo & Joyce Njuguna
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
slurm_header <- paste("#!/bin/bash", "#SBATCH -p normal",
                    "#SBATCH --mem=4g", collapse = "\n")
structure_scriptwriter(task_manager = slurm_header, module = "Structure-2.3.2", populations = 10, input_file = "my_structure_file.txt", output_file_base = "run1_admixture_default50000", reps_per_k = 3, script_basename = "struct"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
