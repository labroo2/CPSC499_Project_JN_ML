#define the method to Visualize individual allele ancestries
structure_plot <- function(object, ...){
  UseMethod("structure_plot", object)
}
structure_plot.destruct <- function(object, ...){
  #get the individual ancestry frequencies from the destruct input object
  ancestries <- as.matrix(names=rownames(object[["individual_ancestry_frequencies"]]),
                          object[["individual_ancestry_frequencies"]][,-1])
  #reformat the data for a barplot
  ancestries2 <- melt(ancestries)
  #make a barplot using ggplot2 geom bar function
  str_plot <- ggplot(data = ancestries2,  aes_string(x = "X1", y = "value", fill = "X2")) +
    geom_bar(stat = "identity") +
    scale_fill_viridis(~ X2, discrete = TRUE, option = "plasma") +
    theme(axis.text.x = element_blank(),#remove x an y axis names 
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
  return(str_plot)
}

