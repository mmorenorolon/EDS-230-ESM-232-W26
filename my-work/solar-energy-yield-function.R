#' Estimating Photovoltaic Energy Yield
#' 
#' @author Melannie Moreno Rolón
#' 
#' @param A numeric scalar value, representing the total panel area (m^2) 
#' 
#' @param r numeric scalar value between 0 and 1, representing panel yield (dimensionless)
#'    Default value is 0.2
#'    
#' @param H numeric scalar value, representing the annual average solar radiation (kWh * m^-2)
#' 
#' @param PR numeric scalar value between 0 and 1, representing the performance ratio (dimensionless)
#'    Default value is 0.75
#'    
#' @returns numeric scalar value, representing the estimated energy yield (kWh)
calculate_solar_energy <- function(A, r = 0.2, H, PR = 0.75) {
 
  E <- A * r * H * PR

  return(E)
}