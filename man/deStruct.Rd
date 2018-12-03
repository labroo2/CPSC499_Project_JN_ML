\name{deStruct}
\alias{deStruct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
deStruct(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file) 
{
    mycon <- file(file, open = "r")
    mylines <- readLines(mycon, warn = FALSE)
    if (sum(grepl("STRUCTURE by Pritchard", mylines)) == 0) {
        stop("The file does not appear to be a STRUCTURE file")
    }
    run_param <- grep("Run parameters:", mylines)
    run_params <- mylines[(run_param + 1):(run_param + 5L)]
    run_params <- strsplit(gsub(" ", "", run_params), " ")
    run_parameters <- data.frame(parameter = gsub("[[:digit:]]", 
        "", run_params), Value = gsub("([[:alpha:]]|[[:punct:]])", 
        "", run_params), stringsAsFactors = FALSE)
    inferred <- grep("Inferred Clusters", mylines)
    inferred_clus <- mylines[(inferred + 1):(inferred + 2)]
    inferred_clus <- strsplit(inferred_clus, " ")
    inferred_clus <- sapply(inferred_clus, function(x) x[x != 
        ""])
    inferred_cluster <- as.data.frame(inferred_clus)
    colnames(inferred_cluster) <- c("cluster", "proportion")
    E_heterozygosity <- grep("expected heterozygosity", mylines)
    heterozygosity <- mylines[(E_heterozygosity + 1):(E_heterozygosity + 
        length(inferred_cluster$cluster))]
    heterozygosity <- strsplit(heterozygosity, " ")
    heterozygosity <- t(sapply(heterozygosity, function(x) x[x != 
        ""]))
    expected_heterozygosity <- as.data.frame(heterozygosity[, 
        c(2, 4)])
    colnames(expected_heterozygosity) <- c("cluster", "HE")
    FST <- grep("Mean value of Fst_1", mylines)
    FST_values <- mylines[FST:(FST + length(inferred_cluster$cluster) - 
        1)]
    FST_values <- strsplit(FST_values, "=")
    FST_values <- t(sapply(FST_values, function(x) x[x != ""]))
    mean_FST_value <- as.data.frame(trimws(FST_values, which = "right"))
    colnames(mean_FST_value) <- c("cluster", "FST")
    ancestry <- grep("Inferred ancestry of individuals:", mylines)
    number_of_individuals <- as.integer(run_parameters$Value[run_parameters$parameter == 
        "individuals"])
    ancestry_val <- mylines[(ancestry + 2):(ancestry + number_of_individuals + 
        1)]
    ancestry_val <- strsplit(ancestry_val, ":")
    ancestry_val <- t(sapply(ancestry_val, function(x) trimws(x, 
        which = "both")))
    ancestry_value <- matrix(, nrow = number_of_individuals, 
        ncol = 0)
    for (col in 1:ncol(ancestry_val)) {
        ancestry_val1 <- t(sapply(strsplit(ancestry_val[, col], 
            " "), function(x) x[x != ""]))
        ancestry_value <- cbind(ancestry_value, ancestry_val1)
    }
    rownames(ancestry_value) <- ancestry_value[, 2]
    ancestry_value <- ancestry_value[, 3:ncol(ancestry_value)]
    ancestry_value[, 1] <- gsub("[[:punct:]]", "", ancestry_value[, 
        1])
    ancestry_value <- data.frame(ancestry_value)
    colnames(ancestry_value) <- c("percent_missing", paste("cluster_", 
        1:length(inferred_cluster$cluster), sep = ""))
    allele_start <- grep("Estimated Allele Frequencies in each cluster", 
        mylines)
    allele_end <- grep("Values of parameters used in structure:", 
        mylines)
    allele_freq <- mylines[(allele_start + 4):(allele_end - 2)]
    allele_freq <- trimws(allele_freq, which = "both")
    length(allele_freq)
    allele_freq <- allele_freq[allele_freq != ""]
    Locus_index <- grep("Locus", allele_freq)
    Locus <- allele_freq[Locus_index]
    missing_data <- allele_freq[Locus_index + 2]
    Allele_A <- allele_freq[Locus_index + 3]
    Allele_B <- allele_freq[Locus_index + 4]
    Locus_split <- strsplit(Locus, " ")
    Locus_final <- t(sapply(Locus_split, function(x) x[x != ":"]))
    missing <- trimws(gsub("([[:alpha:]]|\%)", "", missing_data), 
        which = "both")
    Allele_A_split <- t(sapply(strsplit(Allele_A, " "), function(x) x[x != 
        ""]))
    Allele_A_split[, 2] <- gsub("(\\(|\\))", "", Allele_A_split[, 
        2])
    Allele_B_split <- t(sapply(strsplit(Allele_B, " "), function(x) x[x != 
        ""]))
    Allele_B_split[, 2] <- gsub("(\\(|\\))", "", Allele_B_split[, 
        2])
    Allele_1 <- Allele_A_split
    Allele_2 <- Allele_B_split
    Allele_1[which(Allele_A_split[, 1] == 2), ] <- Allele_B_split[which(Allele_A_split[, 
        1] == 2), ]
    Allele_2[which(Allele_B_split[, 1] == 1), ] <- Allele_A_split[which(Allele_B_split[, 
        1] == 1), ]
    allele_frequency <- cbind(Locus_final[, 3], missing, Allele_1, 
        Allele_2)
    allele_frequency <- as.data.frame(allele_frequency)
    colnames(allele_frequency) <- c("Locus", "missing", "Allele_1", 
        "Proportion_A1", "Allele1_clust1", "Allele1_clust2", 
        "Allele_2", "Proportion_A2", "Allele2_clust1", "Allele2_clust2")
    close(mycon)
    structure_output <- list(run_parameters = run_parameters, 
        inferred_clusters = inferred_cluster, HE = expected_heterozygosity, 
        FST = mean_FST_value, individual_ancestry_frequencies = ancestry_value, 
        allelewise_ancestry_frequency = allele_frequency)
    class(structure_output) <- c("destruct", class(structure_output))
    return(structure_output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
