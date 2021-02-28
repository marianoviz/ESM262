#' predominant_diatom
#'
#' This function determines the predominance between the diatoms Asterionella formosa and Cyclotella meneghiniana in lake waters as a function of the concentration of limiting nutrients phosphate (PO4) and silicate (SiO2).
#' @param p_concentration phosphate (PO4) concentration (mg/L)
#' @param si_concentration silicate (SiO2) concentration (mg/L)
#' @param lake_temp lake water temperature (C)
#' @param p_min_ast minimum amount of phosphate needed for Asterionella to have a positive rate of growth (mg/L) (default = 0.01)
#' @param p_min_cyc minimum amount of phosphate needed for Cyclotella to have a positive rate of growth (mg/L) (default = 0.2)
#' @param si_min_ast minimum amount of silicate needed for Asterionella to have a positive rate of growth (mg/L) (default = 1.9)
#' @param si_min_cyc minimum amount of silicate needed for Cyclotella to have a positive rate of growth (mg/L) (default = 0.6)
#' @param min_lake_temp minimum lake water temperature for diatoms to survive (C) (default = 4)
#' @param max_lake_temp maximum lake water temperature for diatoms to survive (C) (default = 28)
#' @return cyc_predominates, ast_predominates, coexistence, no_survival_conditions
#' @author Mariano Viz
 



predominant_diatom = function(p_contentration, si_concentration,
                              lake_temp,
                              p_min_ast = 0.01, p_min_cyc = 0.2,
                              si_min_ast = 1.9, si_min_cyc = 0.6,
                              min_lake_temp = 4, max_lake_temp = 28) {
  #error checking
  if (p_contentration < 0) 
    return("nutrients concentration cannot be negative")
  
  if (si_concentration < 0) 
    return("nutrients concentration cannot be negative")
  
  if (lake_temp > 50) 
    return("lake temp over 50C? ---> check value")

  
  if (lake_temp >= min_lake_temp & lake_temp <=  max_lake_temp) {predominant = case_when (
    p_contentration >= p_min_cyc  & si_concentration >= si_min_cyc & si_concentration <= si_min_ast ~ "cyc_predominates", #Cyclotella predominates
    p_contentration >= p_min_ast & p_contentration <= p_min_cyc & si_concentration >= si_min_ast ~ "ast_predominates", #Asterionella predominates
    p_contentration > p_min_cyc & si_concentration > si_min_ast ~ "coexistence", #both Asterionella and Cyclotella coexist without predominance
    p_contentration < p_min_ast ~"no_survival_conditions", #No diatoms can survive
    si_concentration < si_min_cyc ~ "no_survival_conditions",
    p_contentration < p_min_cyc & si_concentration < si_min_ast ~ "no_survival_conditions")
                            } else
  predominant = "no_survival_conditions"
  
  return(predominant)

}