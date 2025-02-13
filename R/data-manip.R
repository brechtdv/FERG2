
#Function which take a data frame as imput and return the data frame with corresponding population
add_pop<-function (df) {
  tot <- pop %>% group_by(ISO3, YEAR, AGE) %>% summarise(POP = sum(POP)) %>% 
    mutate(SEX = "All sexes") %>% bind_rows(pop, .)
  if (!("ISO3" %in% names(df))) 
    stop("The input must contain a variable named ISO3")
  if (!("REF_YEAR_START" %in% names(df))) 
    stop("The input must contain a variable named REF_YEAR_START")
  if (!("REF_YEAR_END" %in% names(df))) 
    stop("The input must contain a variable named REF_YEAR_END")
  if (!("REF_AGE_START" %in% names(df))) 
    stop("The input must contain a variable named REF_AGE_START")
  if (!("REF_AGE_END" %in% names(df))) 
    stop("The input must contain a variable named REF_AGE_END")
  if (!("REF_SEX" %in% names(df))) 
    stop("The input must contain a variable named REF_SEX")
  df$ID_ROW <- seq.int(nrow(df))
  by <- join_by(ISO3, REF_SEX == SEX, REF_YEAR_START <= YEAR, 
                REF_YEAR_END >= YEAR, REF_AGE_START <= AGE, REF_AGE_END >= 
                  AGE)
  x <- left_join(df, tot, by) %>% group_by(ID_ROW, ISO3, REF_YEAR_START, 
                                           REF_YEAR_END, REF_AGE_START, REF_AGE_END, REF_SEX) %>% 
    summarise(POP = sum(POP))
  df <- left_join(df, x)
  rm(list = c("tot", "by", "x"))
  n <- sum(is.na(df$POP))
  if (n > 0) 
    warning(paste("Warning:", n, " rows have missing data for the population variable. Please check if ISO3 code is correctly specified and if the dates are included in the study field."))
  return(df)
}

#  Function to calculate mean and uncertainty intervals
mean_ci <- function (x, ...) {
  c(mean = mean(x, ...), quantile(x, probs = c(0.025, 0.975), 
                                  ...))
}

#  Function to calculate mean, median and uncertainty intervals
mean_median_ci <- function (x, ...) {
  c(mean = mean(x, ...), median = median(x, ...), quantile(x, probs = c(0.025, 0.975), 
                                  ...))
}