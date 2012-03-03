\name{graph.extract}

\alias{graph.extract}

\title{Extract raw data from graphs}

\description{Extract data values from existing graphs in jpeg format.}

\usage{graph.extract(MT, refX, refY, save = "no", image = read.jpeg(file.choose()))}

\arguments{
  \item{MT}{Measurement times: number of observations}

  \item{refX}{2 reference values for the X scale (e.g., \code{refX=c(2,14)}).}

  \item{refY}{2 reference values for the Y scale (e.g., \code{refY=c(10,16)}).}

  \item{save}{Save the extracted data points to a file (\code{save="yes"}) or just see them as output in the R console (default: \code{save="no"}).}

  \item{image}{Jpeg file in which the graph can be found. Default: a window pops up in which the file can be selected.}

}

\details{
When using the default \code{image} argument, a window will pop up to ask in what jpeg file the graph can be found. 

Then the reference values should be located on the graph by clicking with the left mouse button: first the X values and then the Y values. The marked reference points are indicated with a blue cross.

Then each data point should be located sequentially. The marked data points are indicated with a red circle.

When finished, the calibrated true data points are displayed as output in the R console and plotted in a new graph, as a quick visual check.

When choosing to save these data values to a file (\code{save="yes"}), another window will pop up to ask where to save them. Here one can creat a new txt file in the folder of choice.
}

\references{

Bulte, I., & Onghena, P. (in press). When the Truth Hits You Between the Eyes: A Software Tool for the Visual Analysis of Single-Case Experimental Data. Manuscript accepted for publication in Methodology. 

\url{http://ppw.kuleuven.be/english/research/mesrg}

}

\note{
This function is an adaptation of: Timothee. (2010, March 5). Data visualization (in R). Getting data from an image (introductory post). [Web log message]. Retrieved from http://rdataviz.wordpress.com/2010/03/05/getting-data-from-an-image-introductory-post/.

We would like to thank David Howell for pointing us to this function.
}

\keyword{raw data}
\keyword{graph}