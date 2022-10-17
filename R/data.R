

#' Germany.1918 exemple dataset
#' 
#' Temporal distribution of Spanish flu in Prussia, Germany, from 1918-19
#' 
#' 
#' @name Germany.1918
#' @docType data
#' @format The format is: num [1:126] 10 4 4 19 6 13 28 23 35 27 ...
#' @references Nishiura H. Time variations in the transmissibility of pandemic
#' influenza in Prussia, Germany, from 1918-19. In: Theoretical Biology and
#' Medical Modelling
#' @source Peiper O: Die Grippe-Epidemie in Preussen im Jahre 1918/19.
#' Veroeffentlichungen aus dem Gebiete der Medizinalverwaltung 1920, 10:
#' 417-479 (in German).
#' @keywords datasets
#' @examples
#' 
#' data(Germany.1918)
#' ## maybe str(Germany.1918) ; plot(Germany.1918) ...
#' 
NULL





#' 2009 A/H1N1 observed Generation Time distribution
#' 
#' Observed generation time distribution for children in household for the 2009
#' A/H1N1 influenza pandemic.
#' 
#' 
#' @name GT.chld.hsld
#' @docType data
#' @format The format is: num [1:8] 0 0.25 0.2 0.15 0.1 0.09 0.05 0.01
#' @source S. Cauchemez, A. Bhattarai, T.L. Marchbanks, R.P. Fagan, S. Ostroff,
#' N.M. Ferguson, et al., Role of Social Networks in Shaping Disease
#' Transmission During a Community Outbreak of 2009 H1N1 Pandemic Influenza,
#' Pnas. 108 (2011) 2825-2830.
#' @keywords datasets
#' @examples
#' 
#' data(GT.chld.hsld)
#' ## maybe str(GT.chld.hsld) ; plot(GT.chld.hsld) ...
#' 
NULL





#' H1N1 serial interval sample
#' 
#' Data taken from traced cases of H1N1 viruses.
#' 
#' Vector of values that represents the time lag between symptoms onset for
#' pairs of infector/infectee, for a dataset of complete traced cases. Each
#' value accounts for a pair of infector/infectee. This serial interval is
#' often substitued for the generation time distribution, as it is easier to
#' observe.
#' 
#' @name H1N1.serial.interval
#' @docType data
#' @format The format is: num [1:355] 1 1 3 2 1 2 1 3 2 4 ...
#' @keywords datasets
#' @examples
#' 
#' data(H1N1.serial.interval)
#' ## maybe str(H1N1.serial.interval) ; plot(H1N1.serial.interval) ...
#' 
NULL





#' Estimation of R0 and Real-Time Reproduction Number from Epidemics
#' 
#' Estimation of reproduction numbers for disease outbreak, based on incidence
#' data. The R0 package implements several documented methods. It is therefore
#' possible to compare estimations according to the methods used. Depending on
#' the methods requested by user, basic reproduction number (commonly denoted
#' as R0) or real-time reproduction number (referred to as R(t)) is computed,
#' along with a 95\% Confidence Interval. Plotting outputs will give different
#' graphs depending on the methods requested : basic reproductive number
#' estimations will only show the epidemic curve (collected data) and an
#' adjusted model, whereas real-time methods will also show the R(t) variations
#' throughout the outbreak time period. Sensitivity analysis tools are also
#' provided, and allow for investigating effects of varying Generation Time
#' distribution or time window on estimates.
#' 
#' \tabular{ll}{Package: \tab R0\cr Type: \tab Package\cr Title: \tab
#' Estimation of R0 and Real-Time Reproduction Number from Epidemics\cr
#' Version: \tab 1.2-6\cr Date: \tab 2015-05-21\cr Author: \tab Pierre-Yves
#' Boelle, Thomas Obadia\cr Maintainer: \tab Thomas Obadia
#' <thomas.obadia@iplesp.upmc.fr>\cr Depends: \tab R (>= 2.13.0), MASS\cr
#' License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr}
#' 
#' @name R0-package
#' @aliases R0-package R0
#' @docType package
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords package
NULL



