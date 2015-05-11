#' SimBM
#' 
#' Simulation of two-dimensional Brownian motion
#' 
#' @param n Length of Brownian motion.
#' @param sigma Standard deviation for error process.
#' 
#' @return return the dataset you want
#' 
#' @examples
#' y <- SimBM(n=10000, sigma=1.2)
#' 
#' @export
SimBM = function(n, sigma=1){
    # This function simulates two-dimensional Brownian Motion
    
    # Check the inputs are valid
    if (n<=0) {
        stop("Input length for Brownian motion ", n," is illegal, use positive number!")
    } else if (sigma <=0) {
        stop("Input sigma ", sigma," for Brownian motion is illegal, use positive number!")
    } else {
        x0 = matrix( rnorm(2, mean=0, sd=1), 2, 1)  # generate starting point from std normal
        
        error = matrix( rnorm(2*(n-1), mean=0, sd=sigma), 2, n-1)  # generate error process
        
        x = apply(cbind(x0, error), 1, cumsum)  # construct the two-dim BM
        #colnames(x) = c("1st_Dim", "2nd_Dim")
        
        return(x)
    }
    
}


#' PlotBM
#' 
#' Plot two-dimensional Brownian motion
#' 
#' @param x 2-D Brownian motion.
#' 
#' @return None, unless there is any error message.
#' 
#' @examples
#' y <- SimBM(n=10000, sigma=1.2)
#' PlotBM(y)
#' 
#' @export
PlotBM = function(x){
    # This function plots two-dimensional Brownian Motion
    
    if(dim(x)[2] != 2){
        stop("The data is not in the right form, the number of column should be 2!")
    } else{
    
        n = dim(x)[1] # the length of the BM
    
        plot(x, type="l", main="Two-Dimensional Brownian Motion", xlab="1st Dimension", ylab="2nd Dimension")
    
        # Indicate starting and ending points
        points(x[1,1],x[1,2], pch=2)
        points(x[n,1],x[n,2], pch=7)
    
        legend("topright", c("Start", "End"), pch=c(2,7))
    }
}
