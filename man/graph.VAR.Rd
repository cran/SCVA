\name{graph.VAR}

\alias{graph.VAR}

\title{Plot an estimate of variability}

\description{Information about variability in the data is displayed by three methods. 
Range bar graphs consist of a vertical line for each phase, created by connecting three points: an estimate of central tendency ((trimmed) mean, (broadened) median, M-estimator), the minimum and the maximum. 
Range lines consist of a pair of lines parallel to the X-axis, passing through the lowest and highest values for each phase, and superimposed on the raw data. 
Trended ranges display changes in variability within phases. 
For all these methods, the influence of outliers may be lessend by using a trimmed range, in which only a sample of the data set is used. }

\usage{graph.VAR(design, VAR, dataset = "regular", CL, tr, 
data = read.table(file.choose(new = FALSE)), xlab = NULL, ylab = NULL, 
ylim = NULL, legendxy = NULL, labels = c("A", "B", "A", "B"))}

\arguments{
  \item{design}{Type of single-case design: \code{"AB"}, \code{"ABA"}, \code{"ABAB"}, \code{"CRD"}(completely randomized design), \code{"RBD"} (randomized block design), \code{"ATD"} (alternating treatments design), \code{"MBD"} (multiple-baseline AB design) or \code{"Custom"} (user specified design).}

  \item{VAR}{Estimate of variability: range bar graph (\code{"RB"}), range lines (\code{"RL"}), or trended range (\code{"TR"}).}

  \item{dataset}{Use the whole dataset (\code{"regular"}) or reduce the influence of outliers by removing the 10-20 percent extreme values from each phase (\code{"trimmed"}). Default: the whole dataset is used.}

  \item{CL}{Measure of central tendency: \code{"mean"}, \code{"median"}, \code{"bmed"} (broadened median), \code{"trimmean"} (trimmed mean), or \code{"mest"} (M-estimator of location).}

  \item{tr}{If \code{CL="trimmean"}: the percentage of observations that has to be removed from the end of the distribution before computing the mean. It can be any value from 0 (regular arithmetic mean) to 0.5. Usually 20 percent of the observations is trimmed (so \code{tr=0.2}). 
If \code{CL="mest"}: the desired value for the constant K. Usually a percentile of the standard normal distribution is chosen. Wilcox (2005) suggests using K=1.28, which corresponds to the 90th percentile of the standard normal distribution and covers 80 percent of the underlying distribution.}

  \item{data}{File in which the data can be found. Default: a window pops up in which the file can be selected.}
  
  \item{xlab}{Label x axis.}
  
  \item{ylab}{Label y axis.}
  
  \item{ylim}{Y axis limits in the form \code{c(min, max)}.}

  \item{legendxy}{Optional legend location x and y coordinates in the form \code{c(x coordinate, y coordinate)}. Only used when design is \code{"CRD"}, \code{"RBD"}, \code{"ATD"} or \code{"Custom"}.}
  
  \item{labels}{Optional labels for treatment levels in the form \code{c("A", "B")}.}

}

\details{

When using the default \code{data} argument, a window will pop up to ask in what file the data can be found. This text file containing the data should consist of two columns for single-case phase and alternation designs: the first with the condition labels and the second with the obtained scores. For multiple-baseline designs it should consist of these two columns for EACH unit. This way, each row represents one measurement occasion. It is important not to label the rows or columns.

Missing data should be indicated as \code{NA}. For calculations, missing data are omitted. Please note that some of the complicated plots may not work if there is missing data.

For alternation designs, after the plot is drawn, the location of the legend should be indicated by a left mouse click.

}

\references{

Bulte, I., & Onghena, P. (2012). When the truth hits you between the eyes: A software tool for the visual analysis of single-case experimental data. Methodology, 8, 104-114. 

\url{http://ppw.kuleuven.be/home/english/research/mesrg}

}

\note{

For the calculation of the M-estimator of location, the function mest(x,bend=1.28) from Wilcox (2005) is used. 

Wilcox, R.R. (2005). Introduction to robust estimation and hypothesis testing (2nd ed.). San Diego, CA: Elsevier Academic Press.

}

\seealso{

\code{\link{graph}} to simply plot raw single-case data.

\code{\link{graph.CL}} to plot a measure of central tendency as a line parallel to the abscissa.

\code{\link{graph.TREND}} to display a possible trend in the data.

\code{\link{graphly}} to display an interactive plot.

}

\examples{
data(AB)
graph.VAR(design = "AB", VAR = "RB", CL = "mean", data = AB)
graph.VAR(design = "AB", VAR = "RL", data = AB)
graph.VAR(design = "AB", VAR = "TR", data = AB)
graph.VAR(design = "AB", VAR = "RL", dataset = "trimmed", data = AB)
}

\keyword{Single-case design}
\keyword{Graph}
\keyword{Variability}
\keyword{Range lines}
\keyword{Range bar}
\keyword{Trended range}