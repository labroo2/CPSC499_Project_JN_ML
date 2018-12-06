\name{average_runs}
\alias{average_runs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average_runs
}
\description{
This function averages across replications of STRUCTURE for a given value of K. Note that in this implementation, the user must ensure that the clusters are in a consistent order for the input.
}
\usage{
average_runs(deStruct_list)
}
%- maybe also 'usage' for other objects documented here.
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
\references{
%% ~put references to the literature/web site here ~
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

