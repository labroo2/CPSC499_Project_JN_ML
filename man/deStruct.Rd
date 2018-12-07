\name{deStruct}
\alias{deStruct}
\title{
Parse STRUCTURE output files   
}
\description{
This function parses structure output storing Run parameters 
Inferred clusters 
Expected heterozygosity
Mean FST value
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
Run parameters 
Inferred clusters 
Expected heterozygosity
Mean FST value
Individual ancestry frequencies 
Allelewise ancestry frequency
}

\author{
Joyce Njuguna and Marlee Labroo
}
\examples{
#import the example structure file installed with DeStruct
mypath <- system.file("extdata", "3kcomp_filtered_full_default50000_k2r1_f", package = "DeStruct")

#Run function DeStruct on file
myDestructObject <- deStruct(mypath)
}