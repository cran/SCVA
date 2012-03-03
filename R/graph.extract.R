graph.extract<-function(MT, refX, refY, save="no", image=read.jpeg(file.choose())){
  
  library(ReadImages)
  plot(image)
  refpoints <- locator(n = 4, type = 'p', pch = 4, col = 'blue', lwd = 2)
  refpoints <- as.data.frame(refpoints)	
  datapoints <- locator(n=MT,type='p',pch=1,col='red',lwd=2,cex=2)
  datapoints <- as.data.frame(datapoints)	
  x <- refpoints$x[c(1,2)]
  y <- refpoints$y[c(3,4)]
  cx <- lm(formula=c(refX[1],refX[2])~c(x))$coeff
  cy <- lm(formula=c(refY[1],refY[2])~c(y))$coeff
  datapoints$x <- datapoints$x*cx[2]+cx[1]
  datapoints$y <- datapoints$y*cy[2]+cy[1]
  true.data <- as.data.frame(datapoints)
  plot(true.data,type='b',pch=1,col='blue',lwd=1.1,bty='l')
  rounded <- round(true.data,digits=2)
  if(save=="yes"){
    write.table(rounded,file=file.choose(new=FALSE),col.names=FALSE,row.names=FALSE,append=FALSE,sep="\t")
  }
  return(rounded)

}