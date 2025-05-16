### WA Counties Analysis START

WA_county <- read.csv("WA_county.csv")

regression_p_value_table <- function() {
  result <- tibble(
    Month = character(),
    Significant_Counties = integer(),
    Positive_Slope_Counties = integer(),
    Negative_Slope_Counties = integer(),
    Not_Significant = integer()
  )
  
  for (selected_month in c("All Months", month.name)) {
    sig_counties <- 0
    pos_slope_counties <- 0
    neg_slope_counties <- 0
    not_sig <- 0
    
    for (selected_county in unique(WA_county$COUNTY)) {
      month_num <- match(selected_month, month.name)
      
      data <- WA_county |>
        filter(COUNTY == selected_county & year(DATE) != 2025) |>
        mutate(Year = year(DATE))
      
      if (selected_month != "All Months") {
        data <- data |> filter(month(DATE) == month_num)
      }
      
      if (nrow(data) > 1 && sum(!is.na(data$TAVG)) > 1) {
        model <- lm(TAVG ~ Year, data = data)
        
        if ("Year" %in% rownames(summary(model)$coefficients)) {
          p_value <- summary(model)$coefficients["Year", "Pr(>|t|)"]
          slope <- coef(model)["Year"]
          
          if (!is.na(p_value) && p_value < 0.05) {
            sig_counties <- sig_counties + 1
            if (slope > 0) pos_slope_counties <- pos_slope_counties + 1
            else if (slope < 0) neg_slope_counties <- neg_slope_counties + 1
          }
          else not_sig <- not_sig + 1
        }
      }
    }
    
    result <- result |> add_row(
      Month = selected_month,
      Significant_Counties = sig_counties,
      Positive_Slope_Counties = pos_slope_counties,
      Negative_Slope_Counties = neg_slope_counties,
      Not_Significant = not_sig
    )
  }
  
  return(result)
}

significance_table <- regression_p_value_table()

significance_table$Month <- factor(significance_table$Month, levels = month.name)

long_data <- significance_table |>
  pivot_longer(
    cols = c(Negative_Slope_Counties, Not_Significant, Positive_Slope_Counties),
    names_to = "Category",
    values_to = "Count"
  )

long_data$Category <- factor(long_data$Category, levels = c("Positive_Slope_Counties",
                                                            "Not_Significant",
                                                            "Negative_Slope_Counties"))

month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

long_data$Month <- factor(long_data$Month, levels = month_order)




### WA Counties Analysis END
