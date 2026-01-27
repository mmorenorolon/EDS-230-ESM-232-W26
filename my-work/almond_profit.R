almond_profit <- function(tmin_c, precip,price_lb = 1.99, cost_acre = 3807) {
  
  yields <- almond_yield(tmin_c = tmin_c, precip = precip)
  
  # Calculating profit
  price_ton <- price_lb * 2000
  
  yields_profit <- yields %>% 
    mutate(revenue = price_ton * yield,
           profit = revenue - cost_acre) %>% 
    select(-mean_temp,-sum_precip)
  
  return(yields_profit)
}