#'double_deposit
#'
#'The function determines the time it takes (in years) for a bank deposit to double its initial capital (discounting the purchasing power loss due to inflation) based on the interest rate and annual inflation.
#' @param amount initial capital ($)
#' @param annual_interest annual interest rate
#' @param annual_inflation annual inflation rate (default = 0.02)
#' @param amount_double double initial capital ($)
#' @return years (yr)
#' @author Mariano Viz



double_deposit <- function(amount, annual_interest, annual_inflation = 0.018, amount_double = amount*2, yr = 0) {
  
  
  #error checking
  if (amount < 0) 
    return("initial capital must be positive")
  
  if (annual_interest < 0) 
    return("for initial capital to double interest rate cannot be negative")
  
  if (annual_interest < annual_inflation) 
    return("real value cannot doouble if annual_interest < annual_inflation")

  
  #use while for repeating the function until the condition amount>=amount_double is met (and thus obtain the time it takes to double the initial capital)
  #second condition (annual_interest > annual_inflation) makes growth always positive, which makes the first condition always true and prevents the cycle from running non-stop
  #other possible option for preventing the function from running non-stop could setting a year limit (e.g. yr < 9999)  
  while((amount < amount_double) &&(annual_interest > annual_inflation)){
    
    amount = amount * (1 + (annual_interest - annual_inflation))
    
    yr = yr + 1
  }
  
  return(yr)
}