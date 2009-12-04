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


hpm.radial.plot <- function(data.values, data.angles = NULL, plot.type = "p",
                            main = "", xlab = "", ylab = "",
                            margins = c(2,2,3,2),
                            start = (pi / 2), clockwise = TRUE,
                            range = NULL, exact.range = TRUE,
                            labels = NA, label.angles = NULL, horizontal.labels = FALSE, label.position = 1.1,
                            line.color = par("fg"), lty = par("lty"), lwd = par("lwd"),
                            show.grid = TRUE, show.grid.labels = TRUE, grid.unit = NULL,
                            show.radial.grid = TRUE, radial.labels = NULL,
                            grid.color= gray(0.9), grid.background = "transparent",
                            point.symbols = NULL, point.color = NULL,
                            show.centroid = FALSE,
                            poly.color = NULL, ...) {
 
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
  
  # Why remove the lowest value? Why coerce to positive values?
  # 
  # data.values <- data.values - range[1]
  # data.values[data.values < 0] <- NA
  
  if (is.null(data.angles[1])) {
    # 
    # If no angles were provided, generate them based on the data.
    # But why 2 / data.length?
    # 
    data.angles <- seq(0, (2 * pi) - (2 / data.length), length.out = data.length)
    # data.angles <- seq(0, (2 * pi) - (1 / data.length), length.out = data.length)
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
    
    # Why shift the grid maximum?
    # 
    # grid.max <- max(grid.range - range[1])
    grid.max <- max(grid.range)
    # grid.angles <- seq(0, (1.96 * pi), by = (0.04 * pi))
    grid.angles <- seq(0, ((2 * pi) * (49 / 50)), by = ((2 * pi) / 50))
  } else {
    grid.range <- NA
    grid.max <- diff(range)
  }
  
  # oldpar <- par("xpd", "mar", "pty")
  # par(mar = margins, pty = "s")
  par(pty = "s")
  
  plot(c(-grid.max, grid.max), c(-grid.max, grid.max), type = "n", axes = FALSE, main = main, xlab = xlab, ylab = ylab)
  
  if (show.grid) {
    for (i in seq(length(grid.range), 1, by = -1)) {
      xpos <- cos(grid.angles) * (grid.range[i] - range[1])
      ypos <- sin(grid.angles) * (grid.range[i] - range[1])
      polygon(xpos, ypos, border = grid.color, col = grid.background)
    }
  }
  # par(xpd=TRUE)
  
  # Make sure that plot attributes are as detailed as need be.
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
  if (length(poly.color) < data.set.count) {
    poly.color <- rep(poly.color, length.out = data.set.count)
  }
  if (length(lty) < data.set.count) {
    lty <- rep(lty, length.out = data.set.count)
  }
  if (length(lwd) < data.set.count) {
    lwd <- rep(lwd, length.out = data.set.count)
  }
  
  for (i in 1:data.set.count) {
    if (data.set.count > 1) {
      linecol <- line.color[i]
      polycol <- poly.color[i]
      pointcol <- point.color[i]
      pointsymbols <- point.symbols[i]
      ltype <- lty[i]
      lwidth <- lwd[i]
    } else {
      linecol <- line.color
      polycol <- poly.color
      pointcol <- point.color
      pointsymbols <- point.symbols
      ltype <- lty
      lwidth <- lwd
    }
    
    # split up plot.type if there is a combination of displays
    # 
    rptype <- unlist(strsplit(plot.type[i], ""))
    if (match("s", rptype, 0)) {
      if (is.null(pointsymbols)) {
        pointsymbols <- i
      }
      if (is.null(pointcol)) {
        pointcol <- i
      }
    }
    
    # get the vector of x positions
    # 
    xpos <- cos(data.angles[i,]) * data.values[i,]
    
    # get the vector of y positions
    # 
    ypos <- sin(data.angles[i,]) * data.values[i,]
    
    # plot radial lines if plot.type == "r"    
    # 
    if (match("r", rptype, 0)) {
      segments(0, 0, xpos, ypos, col = linecol, lty = ltype, lwd = lwidth,...)
    }
    
    if (match("p", rptype, 0)) {
      polygon(xpos, ypos, border = linecol, col = polycol, lty = ltype, lwd = lwidth, ...)
    }
    
    if (match("s", rptype, 0)) {
      points(xpos, ypos, pch = pointsymbols, col = pointcol, ...)
    }
    
    if (show.centroid) {
      if (match("p", rptype, 0)) {
        nvertices <- length(xpos)
        
        # first get the "last to first" area component
        # 
        polygonarea <- (xpos[nvertices] * ypos[1]) - (xpos[1] * ypos[nvertices])
        for (vertex in 1:(nvertices - 1)) {
          polygonarea <- polygonarea + (xpos[vertex] * ypos[vertex + 1]) - (xpos[vertex + 1] * ypos[vertex])
        }
        polygonarea <- polygonarea / 2
        centroidx <- (xpos[nvertices] + xpos[1]) * ((xpos[nvertices] * ypos[1]) - (xpos[1] * ypos[nvertices]))
        centroidy <- (ypos[nvertices] + ypos[1]) * ((xpos[nvertices] * ypos[1]) - (xpos[1] * ypos[nvertices]))
        for (vertex in 1:(nvertices - 1)) {
          centroidx <- centroidx + (xpos[vertex] + xpos[vertex + 1]) * ((xpos[vertex] * ypos[vertex + 1]) - (xpos[vertex + 1] * ypos[vertex]))
          centroidy <- centroidy + (ypos[vertex] + ypos[vertex + 1]) * ((xpos[vertex] * ypos[vertex + 1]) - (xpos[vertex + 1] * ypos[vertex]))
        }
        points(centroidx / (6 * polygonarea), centroidy / (6 * polygonarea), col = point.color[i], pch = point.symbols[i], cex = 2, ...)
      } else {
        points(mean(xpos), mean(ypos), col = pointcol, pch = pointsymbols, cex = 2, ...)
      }
    }
  }
  
  if (is.na(labels[1])) {
    label.angles <- seq(0, 1.8 * pi, length = 9)
    labels <- as.character(round(label.angles, 2))
  }
  if (is.null(label.angles[1])) {
    lablen <- length(labels)
    label.angles <- seq(0, pi * (2 - (2 / lablen)), length.out = lablen)
  }
  if (clockwise) {
    label.angles <- -label.angles
  }
  if(start) {
    label.angles <- label.angles + start
  }
  
  xpos <- cos(label.angles) * grid.max
  ypos <- sin(label.angles) * grid.max
  if (show.radial.grid) {
    segments(0, 0, xpos, ypos, col = grid.color)
  }
  
  xpos <- cos(label.angles) * grid.max * label.position
  ypos <- sin(label.angles) * grid.max * label.position
  if (!horizontal.labels) {
    for(label in 1:length(labels)) {
      labelsrt <- ((180 * label.angles[label]) / pi) + 180 * (label.angles[label] > (pi / 2) && label.angles[label] < (3 * pi / 2))
      text(xpos[label], ypos[label], labels[label], cex = par("cex.axis"), srt = labelsrt)
    }
  } else {
    boxed.labels(xpos, ypos, labels, ypad = 0.7, border = FALSE, cex = par("cex.axis"))
  }
  
  if (show.grid.labels) {
    ypos <- rep(-grid.max / 15, length(grid.range))
    if (is.null(radial.labels)) {
      radial.labels = as.character(grid.range)
    }
    boxed.labels(grid.range - range[1], ypos, radial.labels, border = FALSE, cex = par("cex.lab"))
  }
  
  if (!is.null(grid.unit)) {
    text(grid.max * 1.05, ypos, grid.unit, adj = 0)
  }
  
  # par(oldpar)
}

hpm.radial.plot(1:365)
