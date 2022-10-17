#' Generation Time distribution
#' 
#' Create an object of class GT representing a discretized Generation Time
#' distribution.
#' 
#' How the GT is discretized may have some impact on the shape of the
#' distribution. For example, the distribution may be discretized in intervals
#' of 1 time \code{step} starting at time 0, i.e. [0,1), [1,2), and so on. Or
#' it may be discretized as [0,0.5), [0.5, 1.5), ... (the default).
#' 
#' If the GT is discretized from a given continuous distribution, the expected
#' duration of the Generation Time will be less than the nominal, it will be in
#' better agreement in the second discretization.
#' 
#' If \code{p0} is TRUE (default) then the generation time distribution is set
#' to 0 for day 0.
#' 
#' If no truncation is provided, the distribution will be truncated at 99.99
#' percent probability.
#' 
#' @param type Type of distribution.
#' @param val Vector of values used for the empirical distribution, or c(mean,
#' sd) if parametric.
#' @param truncate Maximum extent of the GT distribution.
#' @param step Time \code{step} used in discretization.
#' @param first.half First probability computed on half period.
#' @param p0 Is probability on day 0 0
#' @return A list with components: \item{GT}{The probabilities for each time
#' unit, starting at time 0.} \item{time}{The time at which probabilities are
#' calculated.} \item{mean}{The mean of the discretized GT.} \item{sd}{The
#' standard deviation of the discretized GT.}
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @examples
#' #Loading package
#' library(R0)
#' 
#' # GT for children at house(from Cauchemez PNAS 2011)
#' 
#' GT.chld.hsld1<-generation.time("empirical", c(0,0.25,0.2,0.15,0.1,0.09,0.05,0.01))
#' plot(GT.chld.hsld1, col="green")
#' GT.chld.hsld1
#' # Discretized Generation Time distribution
#' # mean: 2.729412 , sd: 1.611636 
#' # [1] 0.00000000 0.29411765 0.23529412 0.17647059 0.11764706 0.10588235 0.05882353
#' # [8] 0.01176471
#' 
#' GT.chld.hsld2<-generation.time("gamma", c(2.45, 1.38))
#' GT.chld.hsld2
#' # Discretized Generation Time distribution
#' # mean: 2.504038 , sd: 1.372760
#' # [1] 0.0000000000 0.2553188589 0.3247178420 0.2199060781 0.1144367560
#' # [6] 0.0515687896 0.0212246257 0.0082077973 0.0030329325 0.0010825594
#' #[11] 0.0003760069 0.0001277537
#' 
#' 
#' # GT for school & community
#' GTs1<-generation.time("empirical", c(0,0.95,0.05))
#' plot(GTs1, col='blue')
#' 
#' 
#' plot(GT.chld.hsld1, ylim=c(0,0.5), col="red")
#' par(new=TRUE)
#' plot(GT.chld.hsld2, xlim=c(0,7), ylim=c(0,0.5), col="black")
generation.time <- function
(type=c("empirical","gamma","weibull","lognormal"),
val=NULL, 
truncate=NULL,
step = 1,
first.half=TRUE,
p0=TRUE 
) 
{

# all tests required 
	type=match.arg(type)

#if empirical, check values
	if (type=="empirical") {
    	GT=val
		if (any(GT <0))
			stop("Values in 'val' must be positive")
		if (sum(GT) >1)
			warning("Values will be standardized to sum to 1")
 		if (!is.null(truncate)) {
			if (truncate < length(val)) {
	       warning(paste("Empirical distribution truncated at length ",truncate))
	       GT = GT[1:truncate]
			}
		}
# if parametric
	} else {
		if (length(val)<2 )
			stop("val= c(mean,sd) must be provided for parametric GT")
		mean = val[1]
		sd = val[2]
		if (any(c(mean,sd)<=0))
			stop("'mean' and 'sd' must be positive")
		if (is.null(truncate)) { 
			tmax = ceiling(mean+10*sd); # sufficiently large
		} else {
			tmax=truncate
		}
		if (first.half) {
			t.scale = c(0,0.5+c(0:tmax))
		} else {
			t.scale = c(0:tmax)
		} 
		if (type == "gamma") {
			a = mean*mean/(sd*sd) 
			s = sd*sd / mean						
			GT = diff(pgamma(t.scale,shape=a,scale=s))
		#other cases; weibull, ...		
		} else if (type=="lognormal") {
			meanlog= log(mean^2/sqrt(mean^2+sd^2))
			sdlog=sqrt(2)*sqrt(log(sqrt(mean^2+sd^2)/mean))
			GT = diff(plnorm(t.scale,meanlog=meanlog,sdlog=sdlog))
		} else if (type=="weibull") {
			cv <- sd/(mean)
			if (cv < 1e-06) {
				nu <- cv/(sqrt(trigamma(1)) - cv * digamma(1))
				shape <- 1/nu
				scale <- (mean)/(1 + nu * digamma(1))
			} else {
				aa <- log(cv^2 + 1)
				nu <- 2 * cv/(1 + cv)
				repeat {
					gb <- (lgamma(1 + 2 * nu) - 2 * lgamma(1 + nu) - aa)/(2 * (digamma(1 + 2 * nu) - digamma(1 + nu)))
					nu <- nu - gb
					if (abs(gb) < 1e-12) break
				}
				shape <- 1/nu
				scale <- exp(log(mean) - lgamma(1 + nu))
			}
			GT = diff(pweibull(t.scale,shape=shape,scale=scale))
		}
		if (is.null(truncate)) {
			# truncate when GI distribution >0.9999
			GT.cum = cumsum(GT)
      if(length(GT.cum[GT.cum>0.9999])!=0){
  			truncate = (GT.cum > 0.9999)*(1:length(GT.cum))
  			truncate=min(truncate[truncate>0])
  			if (truncate == 0) warning(paste('provide truncate larger than ',mean + 10 * sd))
  			GT = GT[1:truncate]
			}
		}
	}
	if (p0==TRUE) GT[1]=0
	
	time = 0:(length(GT)-1)
	GT = GT/sum(GT)
	mu=sum(GT * time) 
	sigma = sqrt(sum(GT*time^2) - mu^2)
	return(structure(list(GT=GT,time=time,mean=mu,sd=sigma),class="R0.GT"))
}
