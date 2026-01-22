#' Almond yield anomaly model (per water year)
#'
#' Inputs:
#'   clim: data.frame with columns:
#'     day (int), month (int), year (int), wy (int),
#'     tmax_c (numeric, °C), tmin_c (numeric, °C), precip (numeric, mm/day)
#'   params: list of model parameters (see example below)
#'
#' Output:
#'   data.frame with one row per wy containing:
#'     wy, indices used by the model, and yield_anom (ton/acre)
#'
almond_model <- function(clim, params) {
  
  # --- Basic checks ---
  req <- c("day","month","year","wy","tmax_c","tmin_c","precip")
  missing_cols <- setdiff(req, names(clim))
  if (length(missing_cols) > 0) {
    stop("clim is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure numeric
  clim$tmax_c <- as.numeric(clim$tmax_c)
  clim$tmin_c <- as.numeric(clim$tmin_c)
  clim$precip <- as.numeric(clim$precip)
  
  # --- Helper functions for indices ---
  in_months <- function(m, months_vec) m %in% months_vec
  
  # Simple chill index example:
  # Count days in chill_months where daily mean temp is below t_chill
  chill_index <- function(df) {
    tmean <- (df$tmax_c + df$tmin_c) / 2
    sum(tmean < params$t_chill, na.rm = TRUE)  # units: days
  }
  
  # Growing Degree Days (GDD) example:
  # GDD per day = max(0, tmean - t_base), summed across gdd_months
  gdd_index <- function(df) {
    tmean <- (df$tmax_c + df$tmin_c) / 2
    sum(pmax(0, tmean - params$t_base), na.rm = TRUE)  # units: °C*day
  }
  
  # Extreme heat days example:
  hot_days_index <- function(df) {
    sum(df$tmax_c >= params$t_hot, na.rm = TRUE)  # units: days
  }
  
  # Winter precip example:
  precip_index <- function(df) {
    sum(df$precip, na.rm = TRUE)  # units: mm
  }
  
  # --- Split by water year and compute indices ---
  wys <- sort(unique(clim$wy))
  
  out <- lapply(wys, function(w) {
    df_wy <- clim[clim$wy == w, ]
    
    df_chill  <- df_wy[in_months(df_wy$month, params$chill_months), ]
    df_gdd    <- df_wy[in_months(df_wy$month, params$gdd_months), ]
    df_hot    <- df_wy[in_months(df_wy$month, params$hot_months), ]
    df_precip <- df_wy[in_months(df_wy$month, params$precip_months), ]
    
    Chill   <- chill_index(df_chill)
    GDD     <- gdd_index(df_gdd)
    HotDays <- hot_days_index(df_hot)
    Pwin    <- precip_index(df_precip)
    
    # --- Model equation (edit this to match your class model!) ---
    yield_anom <- params$b0 +
      params$b_chill * Chill +
      params$b_gdd   * GDD +
      params$b_p     * Pwin +
      params$b_hot   * HotDays
    
    data.frame(
      wy = w,
      Chill = Chill,
      GDD = GDD,
      HotDays = HotDays,
      Pwin = Pwin,
      yield_anom = yield_anom
    )
  })
  
  do.call(rbind, out)
}

# Example parameter set (PLACEHOLDER values!)
# Replace b's and/or indices to match the assignment’s true model.
almond_params_example <- list(
  # month windows (edit as needed)
  chill_months  = c(11, 12, 1, 2),
  gdd_months    = c(2, 3, 4, 5, 6),
  hot_months    = c(6, 7, 8),
  precip_months = c(11, 12, 1, 2, 3),
  
  # thresholds (°C)
  t_chill = 7,    # chill threshold example
  t_base  = 4,    # GDD base temp example
  t_hot   = 35,   # extreme heat threshold example
  
  # coefficients (PLACEHOLDERS; units depend on indices)
  b0 = 0,
  b_chill = 0.01,
  b_gdd   = 0.001,
  b_p     = 0.0005,
  b_hot   = -0.02
)
