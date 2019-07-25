graphly <- function(
  design,
  data = read.table(file.choose(new = FALSE)),
  xlab = "Measurement Times",
  ylab = "Scores",
  ylim = NULL,
  labels = c("A", "B", "A", "B"))
{
  MT <- nrow(data)
  
  # Following required just to appease R CMD check.
  idx <- NULL
  obs <- NULL
  lvl <- NULL
  
  if(design == "MBD")
  {
    N <- ncol(data) / 2
    plots <- vector("list", N)
    
    for(i in 1:N)
    {
      df <- data.frame(idx = 1:MT, lvl = as.factor(data[,(i * 2 - 1)]), obs = data[,(i * 2)])
      A1B1idx <- max(df$idx[df$lvl == "A"]) + 0.5
      levels(df$lvl) <- labels[1:2]
      
      plot1 <- qplot(x = idx, y = obs, colour = lvl, shape = lvl, linetype = lvl, data = df, 
                    geom = c("point", "line"), xlab = xlab, ylab = ylab) +
        geom_vline(xintercept = A1B1idx, linetype="dotted") + 
        theme(legend.title = element_blank()) +
        scale_x_continuous(breaks = pretty_breaks()) + 
        removeGrid()
      
      if(!is.null(ylim)) plot1 <- plot1 + coord_cartesian(ylim = ylim) 
      
      plots[[i]] <- plot1
    }
    
    plot2 <- subplot(plots, nrows = N, shareX = TRUE, shareY = TRUE)
    return(plot2)
  }
    
  df <- data.frame(idx = 1:MT, lvl = as.factor(data[,1]), obs = data[,2])
  
  if(design %in% c("AB", "CRD", "RBD", "ATD", "Custom"))
  {
    A1B1idx <- max(df$idx[df$lvl == "A"]) + 0.5
    levels(df$lvl) <- labels[1:2]
    
  } else
  if(design == "ABA")
  {
    A1B1idx <- max(df$idx[df$lvl == "A1"]) + 0.5
    B1A2idx <- max(df$idx[df$lvl == "B1"]) + 0.5
    
    levels(df$lvl) <- c(labels[1], labels[3], labels[2])
    df$lvl <- factor(df$lvl, levels = unique(labels[1:3]))
    
  } else
  if(design == "ABAB")
  {
    A1B1idx <- max(df$idx[df$lvl == "A1"]) + 0.5
    B1A2idx <- max(df$idx[df$lvl == "B1"]) + 0.5
    A2B2idx <- max(df$idx[df$lvl == "A2"]) + 0.5
    
    levels(df$lvl) <- c(labels[1], labels[3], labels[2], labels[4])
    df$lvl <- factor(df$lvl, levels = unique(labels[1:4]))
  }
  
  plot1 <- qplot(x = idx, y = obs, colour = lvl, shape = lvl, linetype = lvl, data = df, 
                 geom = c("point", "line"), xlab = xlab, ylab = ylab) +
    theme(legend.title = element_blank()) +
    scale_x_continuous(breaks = pretty_breaks()) + 
    removeGrid()
  
  if(!is.null(ylim)) plot1 <- plot1 + coord_cartesian(ylim = ylim) 
  
  if(design %in% c("AB", "ABA", "ABAB"))
  {
    plot1 <- plot1 + geom_vline(xintercept = A1B1idx, linetype="dotted")
  }
  if(design %in% c("ABA", "ABAB"))
  {
    plot1 <- plot1 + geom_vline(xintercept = B1A2idx, linetype="dotted")
  }
  if(design == "ABAB")
  {
    plot1 <- plot1 + geom_vline(xintercept = A2B2idx, linetype="dotted")
  }
  
  plot2 <- ggplotly(plot1)
  return(plot2)
}