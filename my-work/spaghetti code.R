# almond_model.R
# Almond yield anomaly model (Lobell et al. 2006, Table 2)
#
# OUTPUT NOTE (course staff):
# "A note about the almond yield model. There's an alternative possible minimum yield
#  depending on how you use minimum temperature in your model. Let me know if this still doesn't match!
#
#  If you use the mean minimum temperature, the output stats should be:
#   max: 1919.98
#   min: -0.35
#   mean: 181.44"
#
# Inputs (clim):
#   day, month, year, tmin_c (°C), precip (mm/day)
#
# Output:
#   One row per wy with: wy, Tn_Feb (°C), P_Jan (mm), yield_anom (ton/acre)

Almond_model <- function(clim,
                         parameters = list(
                           b0       = 0.28,
                           b_tn2    = -0.015,
                           b_tn2_sq = -0.0046,
                           b_p1     = -0.07,
                           b_p1_sq  = 0.0043
                         ),
                         verbose_note = TRUE) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install it with install.packages('dplyr').")
  }
  
  req <- c("day", "month", "year", "wy", "tmin_c", "precip")
  missing_cols <- setdiff(req, names(clim))
  if (length(missing_cols) > 0) {
    stop("clim is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (isTRUE(verbose_note)) {
    message(
      "A note about the almond yield model. There's an alternative possible minimum yield depending on how you use minimum temperature in your model. Let me know if this still doesn't match!\n\n",
      "If you use the mean minimum temperature, the output stats should be:\n\n",
      " max: 1919.98\n",
      " min: -0.35\n",
      " mean: 181.44\n"
    )
  }
  
  dplyr::as_tibble(clim) |>
    dplyr::mutate(
      month  = as.integer(month),
      wy     = as.integer(wy),
      tmin_c = as.numeric(tmin_c),
      precip = as.numeric(precip)
    ) |>
    dplyr::group_by(wy) |>
    dplyr::summarise(
      # Mean minimum temperature in February (°C)
      Tn_Feb = mean(tmin_c[month == 2], na.rm = TRUE),
      
      # Total precipitation in January (mm)
      P_Jan  = sum(precip[month == 1], na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Convert NaN (if a wy has no Feb data) into NA
      Tn_Feb = dplyr::if_else(is.nan(Tn_Feb), NA_real_, Tn_Feb),
      
      # Lobell almond model (yield anomaly, ton/acre)
      yield_anom =
        parameters$b0 +
        parameters$b_tn2    * Tn_Feb +
        parameters$b_tn2_sq * (Tn_Feb ^ 2) +
        parameters$b_p1     * P_Jan +
        parameters$b_p1_sq  * (P_Jan ^ 2)
    ) |>
    dplyr::arrange(wy)
}

# Optional helper: print the exact summary stats for checking
almond_check_stats <- function(model_output_df) {
  y <- model_output_df$yield_anom
  c(
    max  = max(y, na.rm = TRUE),
    min  = min(y, na.rm = TRUE),
    mean = mean(y, na.rm = TRUE)
  )
}
