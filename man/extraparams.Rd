\name{extraparams}
\alias{extraparams}
\title{
extraparams
}
\description{
This functions generates the extraparams file needed for the program STRUCTURE v 2.3. The flag descriptions are taken from the STRUCTURE documentation. Note that options described as Boolean here take values of 1 and 0, where 1 = Yes and 0 = No-- not TRUE and FALSE.
}
\usage{
extraparams(noadmix, linkage, usepopinfo, locprior, freqscorr, onefst, inferalpha, popalphas, alpha, inferlambda, popspecificlambda, lambda, fpriormean, fpriorsd, unifprioralpha, alphamax, log10rmin, log10rmax, log10propsd, log10rstart, gensback, migrprior, pfrompopflagonly, locispop, locpriorinit, maxlocprior, printnet, printlambda, printqsum, sitebysite, printqhat, updatefreq, printlikes, intermedsave, echodata, ancestdist, computeprob, admburnin, alphapropsd, startatpopinfo, randomize, seed, metrofreq, reporthitrate)
}
\arguments{
  \item{noadmix}{
(Boolean) Assume the model without admixture (Pritchard et al., 2000a). (Each individual is assumed to be completely from one of the K populations.) In the output, instead of printing the average value of Q as in the admixture case, the program prints the posterior probability that each individual is from each population. 1 = no admixture; 0 = model with admixture.
}
  \item{linkage}{
(Boolean) Use the linkage model. See section 3.1. RLOG10START sets the initial value of recombination rate r per unit distance. RLOG10MIN and RLOG10MAX set the minimum and maximum allowed values for log10r. RLOG10PROPSD sets the size of the proposed changes to log10r in each update. The front end makes some guesses about these, but some care on the part of the user in required to be sure that the values are sensible for the particular application.
}
  \item{usepopinfo}{
(Boolean) Use prior population information to assign individuals to clusters. See also MIGRPRIOR and GENSBACK. Must have POPDATA=1.
}
  \item{locprior}{
(Boolean) Use location information to improve the performance on data that are weakly informative about structure.
}
  \item{freqscorr}{
(double) Use the F model, in which the allele frequencies are correlated across populations (Falush et al., 2003a). More specifically, rather than assuming a prior in which the allele frequencies in each population are independent draws from a uniform Dirichlet distribution, we start with a distribution which is centered around the mean allele frequencies in the sample. This model is more realistic for very closely related populations (where we expect the allele frequencies to be similar across populations), and can produce better clustering (section 3.2). The prior of Fk is set using FPRIORMEAN, and FPRIORSD. There may be a tendency to overestimate K when FREQSCORR is turned on.
}
  \item{onefst}{
(Boolean) Assume the same value of Fk for all populations (analogous to Wright's traditional FST). This is not recommended for most data, because in practice you probably expect different levels of divergence in each population. When K = 2 it may sometimes be difficult to estimate two values of FST separately (but see Harter et al. (2004)). When you are trying to estimate K, you should use the same model for all K (we suggest ONEFST=0).
}
  \item{inferalpha}{
(Boolean) Infer the value of the model parameter alpha from the data; otherwise alpha is fixed at the value ALPHA which is chosen by the user. This option is ignored under the NOADMIX model. (The prior for the ancestry vector Q is Dirichlet with parameters (alpha,alpha,...,alpha). Small alpha implies that most individuals are essentially from one population or another, while alpha > 1 implies that most individuals are admixed.)
}
  \item{popalphas}{
(Boolean) Infer a separate alpha for each population. Not recommended in most cases but may be useful for situations with asymmetric admixture.
}
  \item{alpha}{
(double) Dirichlet parameter (alpha) for degree of admixture (this is the initial value if INFERALPHA==1)
}
  \item{inferlambda}{
(Boolean) Infer a suitable value for lambda. Not recommended for most analyses.
}
  \item{popspecificlambda}{
(Boolean) Infer a separate lambda for each population.
}
  \item{lambda}{
(double) parameterizes the allele frequency prior, and for most data the default value of 1 seems to work pretty well. If the frequencies at most markers are very skewed towards low/high frequencies, a smaller value of lambda may potentially lead to better performance. It doesn't seem to work very well to estimate lambda at the same time as the other hyperparameters, lambda and F.
}
  \item{fpriormean}{
(double) See FREQSCORR. The prior for Fk is taken to be Gamma with mean FPRIORMEAN, and standard deviation FPRIORSD. Our default settings place a lot of weight on small values of F. We find that this makes the algorithm sensitive to subtle structure, but at some increased risk of overestimating K (Falush et al., 2003a).
}
  \item{fpriorsd}{
(double) See FREQSCORR. The prior for Fk is taken to be Gamma with mean FPRIORMEAN, and standard deviation FPRIORSD. Our default settings place a lot of weight on small values of F. We find that this makes the algorithm sensitive to subtle structure, but at some increased risk of overestimating K (Falush et al., 2003a).
}
  \item{unifprioralpha}{
(double) Assume a uniform prior for alpha which runs between 0 and ALPHAMAX. This model seems to work fine; the alternative model (when UNIFPRIORALPHA=0) is to take alpha as having a Gamma prior, with mean ALPHAPRIORA × ALPHAPRIORB, and variance ALPHAPRIORA x ALPHAPRIORB2.
}
  \item{alphamax}{
(double) Assume a uniform prior for alpha which runs between 0 and ALPHAMAX. This model seems to work fine; the alternative model (when UNIFPRIORALPHA=0) is to take alpha as having a Gamma prior, with mean ALPHAPRIORA x ALPHAPRIORB, and variance ALPHAPRIORA x ALPHAPRIORB2.
}
  \item{log10rmin}{
(double) When the linkage model is used, the switch rate r is taken to have a uniform prior on a log scale, between LOG10RMIN and LOG10RMAX. These values need to be set by the user to make sense in terms of the scale of map units being used.
}
  \item{log10rmax}{
(double) When the linkage model is used, the switch rate r is taken to have a uniform prior on a log scale, between LOG10RMIN and LOG10RMAX. These values need to be set by the user to make sense in terms of the scale of map units being used.
}
  \item{log10propsd}{
(double) When the linkage model is used, the switch rate r is taken to have a uniform prior on a log scale, between LOG10RMIN and LOG10RMAX. These values need to be set by the user to make sense in terms of the scale of map units being used.
}
  \item{log10rstart}{
(double) When the linkage model is used, the switch rate r is taken to have a uniform prior on a log scale, between LOG10RMIN and LOG10RMAX. These values need to be set by the user to make sense in terms of the scale of map units being used.
}
  \item{gensback}{
(int) This corresponds to G (Pritchard et al., 2000a). When using prior population information forindividuals (USEPOPINFO=1), the programtests whether each individual has an immigrant ancestor in the last G generations, where G = 0 corresponds to the individual being an immigrant itself. In order to have decent power, G should be set fairly small (2, say) unless the data are highly informative.
}
  \item{migrprior}{
(double) Must be in [0,1]. This is v in Pritchard et al. (2000a). Sensible values might be in the range 0.001 - 0.1.
}
  \item{pfrompopflagonly}{
(Boolean) This option, new with version 2.0, makes it possible to update the allele frequencies, P, using only a prespecified subset of the individuals. To use this, include a POPFLAG column, and set POPFLAG=1 for individuals who should be used to update P, and POPFLAG=0 for individuals who should not be used to update P. This can be used both with, or without USEPOPINFO turned on. This option will be useful, for example, if you have a standard reference set of individuals from known populations, and then you want to estimate the ancestry of some unknown individuals. Using this option, the q estimate for each unknown individual depends only on the reference set, and not on the other unknown individuals in the sample. This property is sometimes desirable.
}
  \item{locispop}{
 (Boolean) This option instructs the program to use the PopData column in the input file as location data when the LOCPRIOR model is turned on. When LOCISPOP=0, the program requires a LocData column to use LOCPRIOR.
}
  \item{locpriorinit}{
(double) Initial value for the LOCPRIOR parameter r, that parameterizes how informative the populations are (cite Hubisz et al. 09). We found that LOCPRIORINIT=1 helped achieve good convergence.
}
  \item{maxlocprior}{
(double) Range of r is from(0,MAXLOCPRIOR). We suggest MAXLOCPRIOR=20.
}
  \item{printnet}{
(Boolean) Print the "net nucleotide distance" between clusters.
In words, the net nucleotide distance is the average probability that a pair of alleles, one each from populations A and B are different, less the average within-population heterozygosities. Perhaps more intuitively, this can be thought of as being the average amount of pairwise difference between alleles from different populations, beyond the amount of variation found within each population. The distance has the appropriate property that similar populations have distances near 0, and in particular, DAA = 0. Notice that the distance is symmetric, so that DAB =DBA. This distance is suitable for drawing trees of populations to help visualize the levels of difference among the clusters (Falush et al., 2003b).
}
  \item{printlambda}{
(Boolean) Print current value of lambda to screen.
}
  \item{printqsum}{
(Boolean) Print summary of current Q estimates to screen; this prints an average for each value of PopData
}
  \item{sitebysite}{
(Boolean) (Linkage model) Print a complete summary of assignment probabilities for every genotype in the data. This is printed to a separate file with the suffix "s". This file can be big!
}
  \item{printqhat}{
(Boolean) When this is turned on, the point estimate for Q is not only printed into the main results file, but also into a separate file with suﬃx "q". This file is required in order to run the companion program STRAT.
}
  \item{updatefreq}{
(int) Frequency of printing updates to the screen. Set automatically if this =0
}
  \item{printlikes}{
(Boolean) Print the current value of the likelihood to the screen in every iteration.
}
  \item{intermedsave}{
(int) If you're impatient to see preliminary results before the end of the run, you can have results printed to file at intervals during the MCMC run. A total of INTERMEDSAVE such files are printed, at equal intervals following the completion of the BURNIN. Turn this off by setting to 0. Names of these files created using OUTFILE name.
}
  \item{echodata}{
(Boolean) Print a brief summary of the data set to the screen and output ﬁle. (Prints the beginnings and ends of the top and bottom lines of the input ﬁle to allow the user to check that it has been read correctly.)
}
  \item{ancestdist}{
(Boolean) Collect information about the distribution of Q for each individual, as well as just estimating the mean. When this is turned on, the output file includes the left- and right-hand ends of the probability intervals for each q(i). (A probability interval is the Bayesian analog of a confidence interval.) The values printed show the middle 100p percent of the probability interval, where p is a number in the range 0.0 to 1.0 and is set using ANCESTPINT. The distribution of Q is estimated by recording the number of hits in each of a number of boxes between 0 and 1, to form a sort of histogram. The width of these boxes, which are of equal size, is set using NUMBOXES.
}
  \item{computeprob}{
(Boolean) Print the log-likelihood of the data at each update, and estimate the probability of the data given K and the model (see section 5). This is used in estimating K, and is also a useful diagnostic for whether the burnin is long enough. The main reason for turning this off would be to speed up the program (~10-15 percent).
}
  \item{admburnin}{
(int) (For use when RECOMBINE=1.) When using the linkage model, a short burnin with the admixture model (say 500 iterations) is strongly recommended in most circumstances. Without such a burnin, the linkage model often produces peculiar results. Set ADMBURNIN < BURNIN. We have dropped a related parameter (NOADMBURNIN) that was in Version 1.
}
  \item{alphapropsd}{
(double) The Metropolis-Hastings update step for alpha involves picking a value alpha from a Normal with mean alpha and standard deviation ALPHAPROPSD> 0. The value of ALPHAPROPSD does not affect the asymptotic behaviour of the Markov chain, but may have a substantial impact on the rate of convergence. If there is a lot of information about alpha, small values of ALPHAPROPSD are preferable to obtain a reasonable acceptance rate. If there's not much information about alpha, larger values produce better mixing.
}
  \item{startatpopinfo}{
(Boolean) Use given populations as the initial condition for population origins. (Need POPDATA==1). This option provides a check that the Markov chain is converging properly in cases where you expected the inferred structure to match the input labels, and it did not. This option assumes that the PopData in the input ﬁle are between 1 and k where k ≤MAXPOPS. Individuals for whom the PopData are not in this range are initialized at random.
}
  \item{randomize}{
(Boolean) Use a different random number seed for each run, taken from the system clock. (See also SEED.)
}
  \item{seed}{
(Integer) If RANDOMIZE==0, then the simulation seed is initialized to SEED. This allows runs to be repeated exactly. If RANDOMIZE=1 then any value specified in SEED is ignored. Note that even when RANDOMIZE==1, the program output still indicates the starting seed value so that it is possible to repeat particular runs if desired.
}
  \item{metrofreq}{
(int) Frequency of using a Metropolis-Hastings step to update Q under the admixture model. When this is used, a new proposal q(i)"" is chosen for each q(i). This proposal is sampled from the prior (ie q(i)"" ~D(lambda,lambda,...,lambda)). The rationale for having this update is that it may improve mixing when alpha is quite small, by making it easier for individuals to jump between populations. The Metropolis-Hastings move is used once every METROFREQ iterations. If METROFREQ is set to 0, it is never used.
}
  \item{reporthitrate}{
(Boolean) Report acceptance rate of Metropolis update for q(i) (see METROFREQ).
}
}
\value{
Writes the mainparams file.
}
\references{
https://web.stanford.edu/group/pritchardlab/structure.html

Falush, D., Stephens, M. & Pritchard, J. K. Inference of population structure using multilocus genotype data: linked loci and correlated allele frequencies. Genetics 164, 1567-1587 (2003)
}
\author{
Marlee Labroo & Joyce Njuguna
}
