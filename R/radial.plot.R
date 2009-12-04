boxed.labels<-function(x,y=NA,labels,
 bg=ifelse(match(par("bg"),"transparent",0),"white",par("bg")),
 border=TRUE,xpad=1.2,ypad=1.2,srt=0,cex=1,adj=0.5,...) {
 
 oldcex<-par("cex")
 par(cex=cex)
 if(is.na(y) && is.list(x)) {
  y<-unlist(x[[2]])
  x<-unlist(x[[1]])
 }
 box.adj<-adj+(xpad-1)*(0.5-adj)
 if(srt == 90 || srt == 270) {
  bheights<-strwidth(labels)
  theights<-bheights*(1-box.adj)
  bheights<-bheights*box.adj
  lwidths<-rwidths<-strheight(labels)*0.5
 }
 else {
  lwidths<-strwidth(labels)
  rwidths<-lwidths*(1-box.adj)
  lwidths<-lwidths*box.adj
  bheights<-theights<-strheight(labels)*0.5
 }
 rect(x-lwidths*xpad,y-bheights*ypad,x+rwidths*xpad,
  y+theights*ypad,col=bg,border=border)
 text(x,y,labels,srt=srt,cex=cex,adj=adj,...)
 par(cex=oldcex)
}


# TODO: Straighten out the grid displaying situation. Allow for printing range labels other than horizontally.
# 
hpm.radial.plot <- function(data.values, data.angles = NULL, plot.type = "p",
                            main = "", xlab = "", ylab = "",
                            margins = c(2,2,3,2),
                            start = (pi / 2), clockwise = TRUE,
                            range = NULL, exact.range = TRUE,
                            labels = NA, label.angles = NULL, horizontal.labels = FALSE, label.shift = 1.1,
                            line.color = par("fg"), lty = par("lty"), lwd = par("lwd"),
                            show.grid = TRUE, show.range.labels = TRUE, grid.unit = NULL,
                            show.radial.grid = TRUE, range.labels = NULL,
                            grid.color= gray(0.9), grid.background = "transparent",
                            point.symbols = NULL, point.color = NULL,
                            show.centroid = FALSE,
                            polygon.color = NULL, ...) {
 
  if (is.null(range)) {
    range <- range(data.values)
  }
  
  # Check the dimensions of the data, and make sure it's stored as a matrix.
  # 
  data.dimensions <- dim(data.values)
  if (is.null(data.dimensions)) {
    # 
    # If the data is a vector, dim() returns NULL.
    # 
    data.length <- length(data.values)
    data.set.count <- 1
    data.values <- matrix(data.values, nrow = 1)
  } else {
    data.length <- data.dimensions[2]
    data.set.count <- data.dimensions[1]
    data.values <- as.matrix(data.values)
  }
  
  # Shift toward zero.
  # 
  data.values <- data.values - range[1]
  
  # Why coerce to positive values?
  # 
  # data.values[data.values < 0] <- NA
  
  # If no angles were provided, generate them based on the data.
  # 
  if (is.null(data.angles[1])) {
    data.angles <- seq(0, (2 * pi) - ((2 * pi) / (data.length + 1)), length.out = data.length)
  }
   
  data.angles.dimensions <- dim(data.angles)
  if (is.null(data.angles.dimensions)) {
    # 
    # If the angles are not already a matrix, generate one of the proper size.
    # 
    data.angles <- matrix(rep(data.angles, data.set.count), nrow = data.set.count, byrow = TRUE)
  } else {
    data.angles <- as.matrix(data.angles)
  }
  
  if (clockwise) {
    data.angles <- -data.angles
  }
  if (start) {
    data.angles <- data.angles + start
  }
  
  if (show.grid) {
    if (length(range) < 3) {
      grid.range <- pretty(range)
    } else {
      grid.range <- range
    }
    
    if (exact.range) {
      if (grid.range[1] < range[1]) {
        grid.range[1] <- range[1]
      }
      if (grid.range[length(grid.range)] > range[2]) {
        grid.range[length(grid.range)] <- range[2]
      }
    } else {
      if (grid.range[1] < range[1]) {
        grid.range <- grid.range[-1]
      }
    }
    
    grid.max <- max(grid.range - range[1])
    grid.angles <- seq(0, (2 * pi) * (49 / 50), by = (2 * pi) / 50)
  } else {
    grid.range <- NA
    grid.max <- diff(range)
  }
  
  # The plot needs to be square, but let's leave the margins alone.
  # 
  # oldpar <- par("xpd", "mar", "pty")
  # par(mar = margins, pty = "s")
  par(pty = "s")
  
  # Set up the plotting area.
  # 
  plot(c(-grid.max, grid.max), c(-grid.max, grid.max), type = "n", axes = FALSE, main = main, xlab = xlab, ylab = ylab)
  
  if (show.grid) {
    for (i in 1:length(grid.range)) {
      grid.x <- cos(grid.angles) * (grid.range[i] - range[1])
      grid.y <- sin(grid.angles) * (grid.range[i] - range[1])
      polygon(grid.x, grid.y, border = grid.color, col = grid.background)
    }
  }
  
  # Let's deal with clipping elsewhere.
  # 
  # par(xpd=TRUE)
  
  # Make sure that plot attributes are as comprehensive as need be.
  # 
  if (length(line.color) < data.set.count) {
    line.color <- 1:data.set.count
  }
  if (length(plot.type) < data.set.count) {
    plot.type <- rep(plot.type, length.out = data.set.count)
  }
  if (length(point.symbols) < data.set.count) {
    point.symbols <- rep(point.symbols, length.out = data.set.count)
  }
  if (length(point.color) < data.set.count) {
    point.color <- rep(point.color, length.out = data.set.count)
  }
  if (length(polygon.color) < data.set.count) {
    polygon.color <- rep(polygon.color, length.out = data.set.count)
  }
  if (length(lty) < data.set.count) {
    lty <- rep(lty, length.out = data.set.count)
  }
  if (length(lwd) < data.set.count) {
    lwd <- rep(lwd, length.out = data.set.count)
  }
  
  # If no labels have been provided, generate them based on the data.
  # 
  if (is.na(labels[1])) {
    label.angles <- seq(0, (2 * pi) - ((2 * pi) / 10), length.out = 9)
    labels <- as.character(round(label.angles, 2))
  }
  
  # If no label angles have been provided, generate them based on the data.
  # 
  if (is.null(label.angles[1])) {
    label.length <- length(labels)
    label.angles <- seq(0, (2 * pi) - ((2 * pi) / label.length), length.out = label.length)
  }
  
  if (clockwise) {
    label.angles <- -label.angles
  }
  if(start) {
    label.angles <- label.angles + start
  }
  
  # Get the vectors of the x and y positions of the radial grid lines.
  # 
  radial.grid.x <- cos(label.angles) * grid.max
  radial.grid.y <- sin(label.angles) * grid.max
  
  if (show.radial.grid) {
    segments(0, 0, radial.grid.x, radial.grid.y, col = grid.color)
  }
  
  
  # Plot the data.
  # 
  for (i in 1:data.set.count) {
    # 
    # Split up plot.type if there is a combination of displays.
    # 
    plot.types <- unlist(strsplit(plot.type[i], ""))
    
    # Set up the symbols, if they aren't provided.
    # 
    if (match("s", plot.types, 0)) {
      if (is.null(point.symbols[i])) {
        point.symbols[i] <- i
      }
      if (is.null(point.color[i])) {
        point.color[i] <- i
      }
    }
    
    # Get the vectors of the x and y positions.
    # 
    data.x <- cos(data.angles[i,]) * data.values[i,]
    data.y <- sin(data.angles[i,]) * data.values[i,]
    
    # Plot radial lines if plot.type includes "r".
    # 
    if (match("r", plot.types, 0)) {
      segments(0, 0, data.x, data.y, col = line.color[i], lty = lty[i], lwd = lwd[i],...)
    }
    
    # Plot a polygon if plot.type includes "p".
    # 
    if (match("p", plot.types, 0)) {
      polygon(data.x, data.y, border = line.color[i], col = polygon.color[i], lty = lty[i], lwd = lwd[i], ...)
    }
    
    # Plot symbol points if plot.type includes "s".
    # 
    if (match("s", plot.types, 0)) {
      points(data.x, data.y, pch = point.symbols[i], col = point.color[i], ...)
    }
    
    # Plot the centroid, if need be.
    # 
    if (show.centroid) {
      if (match("p", plot.types, 0)) {
        nvertices <- length(data.x)
        
        # First get the "last to first" area component.
        # 
        polygonarea <- (data.x[nvertices] * data.y[1]) - (data.x[1] * data.y[nvertices])
        
        for (vertex in 1:(nvertices - 1)) {
          polygonarea <- polygonarea + (data.x[vertex] * data.y[vertex + 1]) - (data.x[vertex + 1] * data.y[vertex])
        }
        polygonarea <- polygonarea / 2
        centroidx <- (data.x[nvertices] + data.x[1]) * ((data.x[nvertices] * data.y[1]) - (data.x[1] * data.y[nvertices]))
        centroidy <- (data.y[nvertices] + data.y[1]) * ((data.x[nvertices] * data.y[1]) - (data.x[1] * data.y[nvertices]))
        
        for (vertex in 1:(nvertices - 1)) {
          centroidx <- centroidx + (data.x[vertex] + data.x[vertex + 1]) * ((data.x[vertex] * data.y[vertex + 1]) - (data.x[vertex + 1] * data.y[vertex]))
          centroidy <- centroidy + (data.y[vertex] + data.y[vertex + 1]) * ((data.x[vertex] * data.y[vertex + 1]) - (data.x[vertex + 1] * data.y[vertex]))
        }
        points(centroidx / (6 * polygonarea), centroidy / (6 * polygonarea), col = point.color[i], pch = point.symbols[i], cex = 2, ...)
      } else {
        points(mean(data.x), mean(data.y), col = point.color[i], pch = point.symbols[i], cex = 2, ...)
      }
    }
  }
  
  # Get the vectors of the x and y positions of the labels.
  # 
  label.x <- cos(label.angles) * grid.max * label.shift
  label.y <- sin(label.angles) * grid.max * label.shift
  
  # Print the grid labels.
  # 
  if (labels[1] != "") {
    if (!horizontal.labels) {
      for(label in 1:length(labels)) {
        label.rotation <- ((180 * label.angles[label]) / pi) + (180 * (label.angles[label] > (pi / 2) && label.angles[label] < (3 * pi / 2)))
        text(label.x[label], label.y[label], labels[label], cex = par("cex.axis"), srt = label.rotation)
      }
    } else {
      text(label.x, label.y, labels, cex = par("cex.axis"))
    }
  }
  
  # Print the radial grid labels.
  # 
  if (show.range.labels) {
    range.labels.y <- rep(-grid.max / 15, length(grid.range))
    
    if (is.null(range.labels)) {
      range.labels = as.character(grid.range)
    }
    
    text(grid.range - range[1], range.labels.y, range.labels, cex = par("cex.lab"))
  }
  
  if (!is.null(grid.unit)) {
    text(grid.max * 1.05, ypos, grid.unit, adj = 0)
  }
  
  # Not much to restore, currently.
  # 
  # par(oldpar)
}
