#' Epidemic outbreak simulation
#' 
#' Generates several epidemic curves with specified distribution and
#' reproduction number.
#' 
#' This function is only used for simulation purposes. The output is a matrix
#' of n columns (number of outbreaks) by m rows (maximum length of an
#' outbreak).
#' 
#' When using rnbinom with "mean" and "size" moments, the variance is given by
#' mean + mean^2/size (see ?rnbinom). One should determine the size accordingly
#' to the \code{R0} value to increase the dispersion. From the previous
#' variance formula, if Var(X) = k*R0, size = R0/(k-1)
#' 
#' @param epid.nb Number of outbreaks to be generated.
#' @param GT Generation time distribution for the pathogen. Must be a
#' R0.GT-class object.
#' @param R0 Basic reproduction number.
#' @param epid.length Length of the epidemic.
#' @param family Distribution type for the new cases, either "poisson" or
#' "negbin".
#' @param negbin.size Over-dispersion parameter, if \code{family} is set to
#' "negbin".
#' @param peak.value Threashold value for incidence before epidemics begins
#' decreasing
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @examples
#' #Loading package
#' library(R0)
#' 
#' ## In this example we simulate n=100 epidemic curves, with peak value at 150 incident cases, 
#' ## and maximum epidemic length of 30 time units.
#' ## Only the outbreak phase is computed. When the peak value is reached, the process is stopped 
#' ## and another epidemic is generated.
#' sim.epid(epid.nb=100, GT=generation.time("gamma",c(3,1.5)), R0=1.5, 
#'          epid.length=30, family="poisson", peak.value=150)
#' 
#' # Here, a 30*100 matrix is returned. Each column is a single epidemic.
#'
#' @importFrom stats rpois rnbinom rmultinom
#'
#' @export
sim.epid <- function
(epid.nb,
 GT,
 R0,
 epid.length,
 family,
 negbin.size=NULL,
 peak.value=50
)
{
  
  #Various content integrity checks
  if (!inherits(GT, "R0.GT")) {
    stop("GT object must be of class R0.GT.")
  }
  
  #If negbin.size is omitted, size parameter is set so that variance = 10*R0. See details.
  if (family=="negbin" & is.null(negbin.size)) {
    negbin.size <- R0/4
  }
  GT <- GT$GT
  #R0.orig <- R0
  
  #Each epidemic is stored as a matrix column
  epidemics <- matrix(data=0, nrow=epid.length, ncol=epid.nb)
  
  #Loop for the required number of epidemic curves
  for (n in 1:epid.nb) {
    
    #Vector of cases with one index case
    sim.epid = c(1,rep(0,epid.length-1))
    
    #Loop on epidemic duration
    for(t in 1:epid.length) { 
      
      #New cases depend on family type
      if (family=="poisson") {
        new <- rpois(sim.epid[t], R0)
      }
      else if (family=="negbin") {
        new <- rnbinom(sim.epid[t], size=negbin.size, mu=R0)
      }
      newd <-  rmultinom(1, sum(new), GT)[,1]
      sim.epid[t:(t+length(GT)-1)] <- sim.epid[t:(t+length(GT)-1)]+newd
      
      # Threshold so that epidemics eventualy dies out
      if (sim.epid[t+1] > peak.value & t < (epid.length-1)) {
        
        #Changing the R value is like implementing control measure. Uncomment if want to doing so
        #R0 <- 0.7
        
        #One the other hand, we can just stop the epidemic and start another one, as we are only interested in the growth period
        sim.epid[(t+2):epid.length] <- 0
        break
      }
    }
    sim.epid <- sim.epid[!is.na(sim.epid)]
    epidemics[,n] <- sim.epid
    #R0 <- R0.orig
  }
  
  return(epidemics)
}
