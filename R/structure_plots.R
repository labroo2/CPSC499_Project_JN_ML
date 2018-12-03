structure_plot <- function(object, ...){
  UseMethod("structure_plot", object)
}
structure_plot.destruct <- function(object, ...){
  require(reshape)
  require(ggplot2)
  require(viridis)
  ancestries <- as.matrix(names=rownames(object[[5]]),
                          object[[5]][,-1])
  ancestries2 <- melt(ancestries)
  str_plot <- ggplot(data = ancestries2,  aes(x = X1, y=value, fill=X2))+
    geom_bar(stat = "identity")+
    scale_fill_viridis(~ X2,discrete = TRUE, option = "plasma")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  return(str_plot)
}

