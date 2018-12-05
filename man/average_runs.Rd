\name{average_runs}
\alias{average_runs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average_runs
}
\description{
This function averages across replicated runs of STRUCTURE for a given value of K. It requires that the user correctly reorder clusters within each deStruct object currently. The function takes a list of deStruct objects and outputs the averaged deStruct object.
}
\usage{
average_runs(deStruct_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deStruct_list}{
An object which is a list of all deStruct objects to be averaged. The deStruct objects should be listed using the list() command, not c(), so that each deStruct object is an element in the list.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an object of the class deStruct
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marlee Labroo & Joyce Njuguna
}
