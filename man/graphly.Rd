\name{graphly}

\alias{graphly}

\title{Interactive plot of single-case data}

\description{The observed single-case data are plotted using plotly, a web-based interactive charting library.}

\usage{graphly(design,data=read.table(file.choose(new=FALSE)),xlab="Measurement Times",
ylab="Scores",ylim=NULL,labels=c("A","B","A","B"))}

\arguments{

  \item{design}{Type of single-case design: \code{"AB"}, \code{"ABA"}, \code{"ABAB"}, \code{"CRD"}(completely randomized design), \code{"RBD"} (randomized block design), \code{"ATD"} (alternating treatments design), \code{"MBD"} (multiple-baseline AB design) or \code{"Custom"} (user specified design).}

  \item{data}{File in which the data can be found. Default: a window pops up in which the file can be selected.} 
  
  \item{xlab}{Label x axis.}
  
  \item{ylab}{Label y axis.}
  
  \item{ylim}{Y axis limits in the form \code{c(min, max)}.}

  \item{labels}{Optional labels for treatment levels in the form \code{c("A", "B")}.}

}

\details{

When using the default \code{data} argument, a window will pop up to ask in what file the data can be found. This text file containing the data should consist of two columns for single-case phase and alternation designs: the first with the condition labels and the second with the obtained scores. For multiple-baseline designs it should consist of these two columns for EACH unit. This way, each row represents one measurement occasion. It is important not to label the rows or columns.

Missing data should be indicated as \code{NA}. For calculations, missing data are omitted.

}

\references{

Bulte, I., & Onghena, P. (2008). An R package for single-case randomization tests. Behavior Research Methods, 40, 467-478.

Bulte, I., & Onghena, P. (2009). Randomization tests for multiple baseline designs: An extension of the SCRT-R package. Behavior Research Methods, 41, 477-485.

Bulte, I., & Onghena, P. (2012). When the truth hits you between the eyes: A software tool for the visual analysis of single-case experimental data. Methodology, 8, 104-114.

\url{http://ppw.kuleuven.be/home/english/research/mesrg}

\url{https://plotly-r.com/overview.html}

}

\seealso{

\code{\link{graph}} to simply plot raw single-case data.

\code{\link{graph.CL}} to plot a measure of central tendency as a line parallel to the abscissa.

\code{\link{graph.VAR}} to display variability information.

\code{\link{graph.TREND}} to display a possible trend in the data.

}

\examples{
data(AB)
graphly(design = "AB", data = AB)}

\keyword{Single-case design}
\keyword{Graph}