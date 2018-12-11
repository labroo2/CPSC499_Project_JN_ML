\name{structure_plot}
\alias{structure_plot}
\alias{structure_plot.destruct}
\title{
Plots 
}
\description{
Visualize individual allele ancestries, to delineate the likely level of population subdivision, 
}
\usage{
structure_plot(object, ...)
}
\arguments{
  \item{object}{
object of destruct S3 class
}
  \item{\dots}{
any additional ggplot parameters
}
}

\value{
Stacked bar plot showing the proportion of individual ancestry from each cluster
}
\author{
Joyce Njuguna and Marlee Labroo
}
\examples{
#import the example structure file installed with DeStruct
mypath <- system.file("extdata", "3kcomp_filtered_full_default50000_k2r1_f", package = "DeStruct")

#Run function DeStruct on file
myDestructObject <- deStruct(mypath)

#make plots
structure_plot(myDestructObject)
}