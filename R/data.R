#' @title
#' Germany 1918 dataset
#' 
#' @docType data
#' 
#' @description
#' This dataset is used as an example in the `R0` package to estimate reproduction
#' ratios using the available methods. It is the emporal distribution of Spanish 
#' flu in Prussia, Germany, from the 1918-19 influenza epidemic.
#'
#' @references 
#' Peiper O: Die Grippe-Epidemie in Preussen im Jahre 1918/19. Veroeffentlichungen aus dem Gebiete der Medizinalverwaltung 1920, 10:  417-479 (in German). \cr 
#' Nishiura H. Time variations in the transmissibility of pandemic influenza in Prussia, Germany, from 1918-19. In: Theoretical Biology and Medical Modelling
#' 
#' @format
#' A vector of numeric values named with date of record.
#' 
#' @usage
#' data(Germany.1918)
"Germany.1918"





#' @title
#' 2009 A/H1N1 observed generation time distribution
#' 
#' @docType data
#' 
#' @description
#' Observed generation time distribution for children in household for the 2009
#' A/H1N1 influenza pandemic.
#'
#' @references 
#' S. Cauchemez, A. Bhattarai, T.L. Marchbanks, R.P. Fagan, S. Ostroff, N.M. Ferguson, et al., Role of Social Networks in Shaping Disease Transmission During a Community Outbreak of 2009 H1N1 Pandemic Influenza, Pnas. 108 (2011) 2825-2830. 
#' 
#' @format
#' A vector of unnamed numeric values.
#' 
#' @usage
#' data(GT.chld.hsld)
"GT.chld.hsld"





#' @title
#' H1N1 serial interval
#' 
#' @docType data
#' 
#' @description
#' Serial interval (in its core definition, i.e. the time delay between symptom 
#' onset between pairs of infectors and infectees), taken from traced cases of 
#' H1N1 viruses.
#' 
#' Vector of values that represents the time lag between symptoms onset for pairs 
#' of infector/infectee, for a dataset of complete traced cases. Each value 
#' accounts for a pair of infector/infectee. 
#' This serial interval is often substitued for the generation time distribution, 
#' as it is easier to observe.
#' 
#' @references 
#' S. Cauchemez, A. Bhattarai, T.L. Marchbanks, R.P. Fagan, S. Ostroff, N.M. Ferguson, et al., Role of Social Networks in Shaping Disease Transmission During a Community Outbreak of 2009 H1N1 Pandemic Influenza, Pnas. 108 (2011) 2825-2830. 
#' 
#' @format
#' A vector of unnamed numeric values.
#' 
#' @usage
#' data(H1N1.serial.interval)
"H1N1.serial.interval"
