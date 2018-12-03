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
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# get the example structure file that comes with deStruct
structure <- system.file("data", "Longi_admixcut045_default50000k2rl_f", package = "deStructure")
#parse the file using destruct function
deStruct(structure)

}
