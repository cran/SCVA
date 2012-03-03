\name{graph.TREND}

\alias{graph.TREND}

\title{Plot a trend in central location}

\description{Visualizes systematic shifts in central location over time using several methods. 
A vertical line graph plots the deviations from each data point to a measure of central tendency against time. 
Regression lines superimpose a linear function on the raw data by means of least squares regression, the split-middle method or the resistant trend line fitting method.
The presence of a nonlinear trend can be displayed with running medians. }

\usage{graph.TREND(design, TREND, CL, tr, data = read.table(file.choose(new = FALSE)))}

\arguments{
  \item{design}{Type of single-case design: \code{"AB"}, \code{"ABA"}, \code{"ABAB"}, \code{"CRD"}(completely randomized design), \code{"RBD"} (randomized block design), \code{"ATD"} (alternating treatments design), or \code{"MBD"} (multiple-baseline AB design).}

  \item{TREND}{Trend visualization: vertical line plot (\code{"VLP"}); trend lines by means of least squares regression (\code{"LSR"}), split-middle method (\code{"SM"}), resistant trend line fitting (\code{"RTL"}), or running medians (\code{"RM3"}, \code{"RM5"} or \code{"RM42"}, depending on the desired batch size).}

  \item{CL}{Measure of central tendency: \code{"mean"}, \code{"median"}, \code{"bmed"} (broadened median), \code{"trimmean"} (trimmed mean), or \code{"mest"} (M-estimator of location).}

  \item{tr}{If \code{CL="trimmean"}: the percentage of observations that has to be removed from the end of the distribution before computing the mean. It can be any value from 0 (regular arithmetic mean) to 0.5. Usually 20 percent of the observations is trimmed (so \code{tr=0.2}). 
If \code{CL="mest"}: the desired value for the constant K. Usually a percentile of the standard normal distribution is chosen. Wilcox (2005) suggests using K=1.28, which corresponds to the 90th percentile of the standard normal distribution and covers 80 percent of the underlying distribution.}

  \item{data}{File in which the data can be found. Default: a window pops up in which the file can be selected.}

}

\details{

When using the default \code{data} argument, a window will pop up to ask in what file the data can be found. This text file containing the data should consist of two columns for single-case phase and alternation designs: the first with the condition labels and the second with the obtained scores.
For multiple-baseline designs it should consist of these two columns for EACH unit. This way, each row represents one measurement occasion. It is important not to label the rows or columns.

For alternation designs, after the plot is drawn, the location of the legend should be indicated by a left mouse click.

}

\references{

Bulte, I., & Onghena, P. (in press). When the Truth Hits You Between the Eyes: A Software Tool for the Visual Analysis of Single-Case Experimental Data. Manuscript accepted for publication in Methodology. 

\url{http://ppw.kuleuven.be/english/research/mesrg}

}

\note{

For the calculation of the M-estimator of location, the function mest(x,bend=1.28) from Wilcox (2005) is used. 

Wilcox, R.R. (2005). Introduction to robust estimation and hypothesis testing (2nd ed.). San Diego, CA: Elsevier Academic Press.

}

\seealso{

\code{\link{graph}} to simply plot raw single-case data.

\code{\link{graph.CL}} to plot a measure of central tendency as a line parallel to the abscissa.

\code{\link{graph.VAR}} to display variability information.

}

\examples{
data(AB)
graph.TREND(design = "AB", TREND = "VLP", CL = "mean", data = AB)
graph.TREND(design = "AB", TREND = "LSR", data = AB)
graph.TREND(design = "AB", TREND = "SM", data = AB)
graph.TREND(design = "AB", TREND = "RTL", data = AB)
graph.TREND(design = "AB", TREND = "RM3", data = AB)
graph.TREND(design = "AB", TREND = "RM5", data = AB)
graph.TREND(design = "AB", TREND = "RM42", data = AB)
}

\keyword{Single-case design}
\keyword{Graph}
\keyword{Trend}
\keyword{Vertical line plot}
\keyword{Regression line}
\keyword{Running median}
