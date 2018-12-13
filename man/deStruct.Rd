\name{deStruct}
\alias{deStruct}
\title{
Parse STRUCTURE output files   
}
\description{
This function parses structure output storing Run parameters, 
Inferred clusters, 
Expected heterozygosity,
Mean FST value,
Individual ancestry frequencies 
and Allelewise ancestry frequency information into a S3 class object
}
\usage{
deStruct(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the path to a structure file.
}
}
\value{
An object of the class "destruct", which comprises of a list that has the various information in a structure file: 
\itemize{
  \item{Run parameters:} {a data frame indicating the nummber of individuals, loci populations and other run parameters listed in the structure file}
  \item {Inferred clusters:} {a data frame indicating the proportion of samples assigned to each cluster}
  \item {Expected heterozygosity:} {a dataframe with information on expected heterozygosity between individuals in same cluster}
  \item {Mean FST value:} {a data frame with FST values for each cluster}
  \item {Individual ancestry frequencies:} {a data frame containing inferred asncestry of all individuals}
  \item {Allelewise ancestry frequency:} {a data frame with estimated allele frequencies in each cluster, across all markers}
}
}

\author{
Joyce Njuguna and Marlee Labroo
}
\examples{
#import the example structure file installed with deStructure
mypath <- system.file("extdata", "myresults_default50000k2r1_f", package = "deStructure")

#Run function DeStruct on file
myDestructObject <- deStruct(mypath)
}