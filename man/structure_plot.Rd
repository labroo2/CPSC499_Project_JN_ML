\name{structure_plot}
\alias{structure_plot}
\alias{structure_plot.destruct}
\title{
Plots 
}
\description{
Visualize individual allele ancestries using a barplot, to delineate the likely level of population subdivision. The plot shows the proportion of individual ancestry from each cluster
}
\usage{
structure_plot(object, ...)
}
\arguments{
  \item{object}{
 S3 class deStruct object, this function will plot the 5th element "individual_ancestry_frequencies"
}
  \item{\dots}{
no additional arguments are currently implemented
}
}

\value{
gg object (Stacked bar plot) that can be manipulates using ggplo2. 
}
\author{
Joyce Njuguna and Marlee Labroo
}
\examples{
#import the example structure file installed with DeStruct
mypath <- system.file("extdata", "myresults_default50000k2r1_f", package = "deStructure")

#Run function deStruct on file
myDestructObject <- deStruct(mypath)

#make plots
structure_plot(myDestructObject)
}