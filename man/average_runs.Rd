\name{average_runs}
\alias{average_runs}
\title{
average_runs
}
\description{
This function averages across replications of STRUCTURE for a given value of K. Note that in this implementation, the user must ensure that the clusters to be averaged are in the same order across the listed deStruct objects.
}
\usage{
average_runs(deStruct_list)
}
\arguments{
  \item{deStruct_list}{
a list of the deStruct objects to be averaged
}
}
\details{
Note that the objects in deStruct_list should be listed using list(), not c(), so that each deStruct object is an element in the list.
}
\value{
an object of the class deStruct
}
\author{
Marlee Labroo & Joyce Njuguna
}
\examples{
#import the example structure files installed with deStruct function
mypath <- system.file("extdata", "myresults_default50000k2r1_f", package = "DeStruct")
mypath2 <- system.file("extdata", "myresults_default50000k2r2_f", package = "DeStruct")
mypath3 <- system.file("extdata", "myresults_default50000k2r3_f", package = "DeStruct")

#Run function DeStruct on file
k2r1 <- deStruct(mypath)
k2r2 <- deStruct(mypath2)
k2r3 <- deStruct(mypath3)

#Since the clusters in these files are already consistently ordered, list and average them.
k2_list <- list(k2r1, k2r2, k2r3)
k2_ave <- average_runs(k2_list)
#
}
