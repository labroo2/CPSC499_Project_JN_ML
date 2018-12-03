\name{structure_plot}
\alias{structure_plot}
\alias(structure_plot.destruct)
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
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Stacked bar plot showing the prportion of individual ancestry from each cluster
}
\author{
Joyce Njuguna and Marlee Labroo
}

\examples{
#make example destruct object

example_plot_data = list(run_parameters = c("A","B"), 
inferred_clusters = c("X","y"),
HE = c(0.5,0.25), FST= 0.75, 
individual_ancestry_frequencies = data.frame(percent_missing=c(28,29,30,31),
Cluster_1=c(0.99,0.98,0.1,0.3),Cluster_2=c(0.01,0.02,0.9,0.7), 
row.names=c("A","B","C","D"))
allelewise_ancestry_frequency = c("x","Y"))

#plot the ancestry

structure_plot.destruct(example_plot_data)
}
