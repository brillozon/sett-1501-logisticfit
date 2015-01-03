
# Compute the cost function and gradient for regularized logistic regressions.
library(fields)

# library(plot3D)
# library(rgl)

#
# Generate a set of random points with a uniform distribution on each axis
# bounded by the specified box.  The box is: (xmin,xmax,ymin,ymax)
#
generatePoints <- function(amount=50, box = c(-1,1,-1,1)) {
  x <- runif(amount,box[1],box[2])
  y <- runif(amount,box[3],box[4])
  cbind(rnorm(amount),rnorm(amount))
}

#
# Generate a categorization for a set of points that randomly assigns one of
# two classifications (0 or 1) to a point with a probability proportional to
# its distance from a given circle and whether the point is inside or outside
# the circle.
#
assignCategories <- function(points,threshold=0.5,center=c(0,0),radius=1) {
  values <- matrix(0,length(points),1)
  d <- rdist(center,points) - radius
}

#
# Augment a set of points with a bias term and polynomial features up to the
# specified degree.
#
expandFeatures <- function( points, degree=6) {
  for(outer in 1:degree) {
    for(inner in 0:outer) {
      points <- cbind(points,points[,1]^(outer-inner) * points[,2]^inner)
    }
  }
  cbind(1,points)
}

# plotCosts <- function(x,y,costs,gridlines=100) {
#   gp <- matrix(0,gridlines,gridlines)
#   xrange <- range(x)
#   yrange <- range(y)
#   gridx <- seq(xrange[1],xrange[2],length.out=gridlines)
#   gridy <- seq(yrange[1],yrange[2],length.out=gridlines)
#   for(ix in 1:gridlines) {
#     for(iy in 1:gridlines) {
#       gp[ix,iy] <- costs(t(as.matrix(c(gridx[ix],gridy[iy]),1,2)))
#     }
#   }
#   
#   zlim <- range(gp)
#   zlen <- zlim[2] - zlim[1] + 1
#   colorlut <- terrain.colors(zlen)
#   col <- colorlut[ gp - zlim[1] + 1]
#   surface3d(gridx,gridy,gp,color=col,back="lines")
#   
#   #   contour(gridx,gridy,gp,nlevels=10,lwd=3,col="red",lty="dashed")
# #   M <- mesh(gridx,gridy)
# #   surf3D(M$x,M$y,gp)
# }


#
# Plot data points.
#
plotPointsData <- function(p,v) {
  zdatax = p[v==0,1]
  zdatay = p[v==0,2]
  odatax = p[v!=0,1]
  odatay = p[v!=0,2]
  plot(zdatax,zdatay,
       xlab="Parameter1",ylab="Parameter2",
       pch='o')
  points(odatax,odatay, pch='+')
}

#
# Plot data points and a decision contour
#
plotDecisionContour <- function(theta,p,v,degree=6,gridlines=100,threshold=0.0) {
  plotPointsData(p[,2:3],v)

  gp <- matrix(0,gridlines,gridlines)
  xrange <- range(p[,2])
  yrange <- range(p[,3])
  gridx <- seq(xrange[1],xrange[2],length.out=gridlines)
  gridy <- seq(yrange[1],yrange[2],length.out=gridlines)
  for(ix in 1:gridlines) {
    for(iy in 1:gridlines) {
      gp[ix,iy] <- expandFeatures(t(as.matrix(c(gridx[ix],gridy[iy]),1,2)),degree) %*% theta
    }
  }
  contour(gridx,gridy,gp,levels=threshold,lwd=3,col="red",lty="dashed",add=T)
}

#
# Generate the sigmoid transformation of input data.
#
sigmoid <- function(x) 1 / (1 + exp( -x))

#
# Cost function
#
# theta - p x 1 matrix of parameters to optimize
# x     - m x p matrix of data to train with
# y     - m x 1 matrix of classification values for training
# l     - regularization scaling factor
#
reglogregJ <- function(theta, x, y, l) {
  # scalar - number of training data elements
  m <- length(y)
  
  # m x 1 matrix - hypothesis function values for the training data
  h_theta <- sigmoid(x %*% theta)
  
  # m x 1 matrix - cost values for each training data element
  terms <- -y * as.numeric(log(h_theta)) - (1 - y) * as.numeric( log(1 - h_theta))
  
  # p x 1 matrix - parameters with 0 bias term
  nobias <- rbind(0,as.matrix(theta[2:length(theta)]))
  
  # scalar - regularization constant for the parameters
  reg <- l * sum(nobias^2) / (2*m)
  
  # scalar - cost of the current parameter values
  J <- reg + sum(terms) / m
}

#
# Cost function gradient
#
# theta - p x 1 matrix of parameters to optimize
# x     - m x p matrix of data to train with
# y     - m x 1 matrix of classification values for training
# l     - regularization scaling factor
#
reglogregGrad <- function(theta, x, y, l) {
  # scalar - number of training data elements
  m <- length(y)
  
  # p x 1 matrix - parameters with 0 bias term
  nobias <- rbind(0,as.matrix(theta[2:length(theta)]))
  
  # m x 1 matrix - hypothesis function values for the training data
  h_theta <- as.numeric( sigmoid(x %*% theta) )
  
  # p x 1 matrix - regularized gradients for each parameter
  grad <- (colSums(apply(x,2,'*',(h_theta - y))) + l * nobias) / m
}
