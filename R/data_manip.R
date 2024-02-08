
#Function which take a data frame as imput and return the data frame with corresponding population
#add the name of the variable as argument? add warning message regarding ISO3 variable
add_pop<-function(df){
  sub<-pop[c(1,2,5)]
  #Check arguments 
  if (!("ISO3" %in% names(df)))
    stop("The input must contain a variable named ISO3")
  if (!("REF_YEAR_START" %in% names(df)))
    stop("The input must contain a variable named REF_YEAR_START")
  if (!("REF_YEAR_END" %in% names(df)))
    stop("The input must contain a variable named REF_YEAR_END")
  #Add the population information
  by<-join_by(ISO3, REF_YEAR_START<=YEAR, REF_YEAR_END>=YEAR)
  x<- left_join(df,sub, by)%>%
    group_by(ISO3,REF_YEAR_START, REF_YEAR_END)%>%
    summarise(POP=sum(POP))
  df<-left_join(df,x)
  rm(list=c("sub","by","x"))
  n<-sum(is.na(df$POP))
  if(n>0)
    warning(paste("Warning:", n, " rows have missing data for the population variable. Please check if ISO3 code is correctly specified and if the dates are included in the study field."))
  return(df)}

