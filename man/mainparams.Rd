\name{mainparams}
\alias{mainparams}
\title{
mainparams
}
\description{
This functions generates the mainparams file needed for the program STRUCTURE v 2.3. The flag descriptions are taken from the STRUCTURE documentation. Note that options described as Boolean here take values of 1 and 0, where 1 = Yes and 0 = No-- not TRUE and FALSE.
}
\usage{
mainparams(maxpops = NULL, burnin = 10000, numreps = 50000, infile, outfile, numinds,
           numloci, ploidy = 2, missing = -9, onerowperind = 0, label = 1,
           popdata = 0, popflag = 0, locdata = 0, phenotype = 0, extracols = 0,
           markernames = 1, recessivealleles = 0, mapdistances = 0, phased = 0,
           phaseinfo = 0, markovphase = NULL, notambiguous = NULL, outpath)
}
\arguments{
  \item{maxpops}{
(int) Number of populations assumed for a particular run of the program. Pritchard et al. (2000a) call this K. Sometimes (depending on the nature of the data) there is a natural value of K that can be used, otherwise K can be estimated by checking the fit of the model at different values of K (see Section 5).
}
  \item{burnin}{
  (int) Length of burnin period before the start of data collection. (See Section 3.3.)
}
  \item{numreps}{
(int) Number of MCMC reps after burnin. (See Section 3.3.)
}
  \item{infile}{
  (string) Name of input data file. Max length 30 characters (or possibly less depending on operating system)
}
  \item{outfile}{
(string) Name for program output files (the suffixes "1", "2",  ..., "m" (for intermediate results) and "f" final results) are added to this name). Existing files with these names will be overwritten. Max length of name 30 characters (or possibly less depending on operating system).
}
  \item{numinds}{
(int) Number of individuals in data file.
}
  \item{numloci}{
(int) Number of loci in data file.
}
  \item{ploidy}{
(int) Ploidy of the organism. Default is 2 (diploid).
}
  \item{missing}{
(int) Value given to missing genotype data. Must be an integer, and must not appear elsewhere in the data set. Default is -9.
}
  \item{onerowperind}{
(Boolean) The data for each individual are arranged in a single row. E.g., for diploid data, this would mean that the two alleles for each locus are in consecutive order in the same row, rather than being arranged in the same column, in two consecutive rows. See section 2 for details about input formats.
}
  \item{label}{
(Boolean) Input file contains labels (names) for each individual. 1 = Yes; 0 = No
}
  \item{popdata}{
(Boolean) Input file contains a user-defined population-of-origin for each individual. 1 = Yes; 0 = No.
}
  \item{popflag}{
(Boolean) Input file contains an indicator variable which says whether to use popinfo when USEPOPINFO==1 (see below). 1 = Yes; 0 = No.
}
  \item{locdata}{
(Boolean) Input file contains a user-defined sampling location for each individual. 1 = Yes; 0 = No. For use in the LOCPRIOR model. Can set LOCISPOP=1 to use the POPDATA instead in the LOCPRIOR model.
}
  \item{phenotype}{
(Boolean) Input file contains a column of phenotype information. 1 = Yes; 0 = No
}
  \item{extracols}{
(int) Number of additional columns of data after the Phenotype before the genotype data start. These are ignored by the program. 0 = no extra columns.
}
  \item{markernames}{
(Boolean) The top row of the data file contains a list of L names corresponding to the markers used.
}
  \item{recessivealleles}{
(Boolean) Next row of data file contains a list of L integers indicating which alleles are recessive at each locus. Setting this to 1 implies that the dominant marker model is in use.
}
  \item{mapdistances}{
(Boolean) The next row of the data file (or the first row if MARKERNAMES==0) contains a list of mapdistances between neighboring loci.
}
  \item{phased}{
(Boolean) For use with linkage model. Indicates that data are in correct phase. If (LINKAGE=1, PHASED=0), then PHASEINFO can be used-this is an extra line in the input file that gives phase probabilities. When PHASEINFO=0 each value is set to 0.5, implying no phase information. When the linkage model is used with polyploids, PHASED=1 is required.
}
  \item{phaseinfo}{
(Boolean) The row(s) of genotype data for each individual are followed by a row of information about haplotype phase. This is for use with the linkage model only. See sections 2 and 3.1 for further details.
}
  \item{markovphase}{
 (Boolean) The phase information follows a Markov model. See sections 2.2 and 9.6 for details.
}
  \item{notambiguous}{
(int) For use with polyploids when RECESSIVEALLELES = 1. Defines the code indicating that genotype data at a marker are unambiguous. Must not match MISSING or any allele value in the data.
}
  \item{outpath}{
 (character) Complete path for the output file
}
}
\value{
No value is returned; the mainparams file is output.
}
\references{
https://web.stanford.edu/group/pritchardlab/structure.html

Falush, D., Stephens, M. & Pritchard, J. K. Inference of population structure using multilocus genotype data: linked loci and correlated allele frequencies. Genetics 164, 1567-1587 (2003)
}
\author{
Marlee Labroo & Joyce Njuguna
}
\examples{
#make a temporary output file path
my_outpath <- tempfile(pattern = "mainparams", tmpdir = tempdir(), fileext = "")
#run mainparams function; output to temp directory
mainparams(maxpops = NULL, burnin = 10000, numreps = 50000, infile = "mydata.txt",
           outfile = "my_output_file", numinds = "300", numloci = "300000",
           ploidy = 2, missing = -9, onerowperind = 0, label = 1,
           popdata = 0, popflag = 0, locdata = 0, phenotype = 0, extracols = 0,
           markernames = 1, recessivealleles = 0, mapdistances = 0, phased = 0,
           phaseinfo = 0, markovphase = NULL, notambiguous = NULL, outpath = my_outpath)
}