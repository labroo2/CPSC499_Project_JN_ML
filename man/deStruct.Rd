\name{deStruct}
\alias{deStruct}
\title{
The function parses a structure file storing the individual and loci ancestry information into a S3 class object 
}
\usage{
deStruct(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
This is the path to a structure file
}
}

\value{
An object of the class "destruct", which comproises of a list that has the various information in a structure file.
}

\author{
Joyce Njuguna and Marlee Labroo
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
# get the example structure file that comes with deStruct
structure <- system.file("data", "example_structure.txt", package = "destruct")
deStruct(structure)

}
