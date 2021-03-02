#' fish_summary
#' 
#' The function takes a vector and find the most common fish, the rarest fish, the total number of fish, and has the option to plot a histogram of the number of each fish type.
#' @param name_vec vector of fish names
#' @param graph histogram with count of fish types (default = FALSE)
#' @return Total number of fish = t, Most common fish = c, Rarest fish = l, Histogram with count of fish types = p
#' @author Claudia Flores & Mariano Viz


fish_summary <- function(name_vec, graph = FALSE) {
  
  f <- as.factor(name_vec) #convert vector to factor class
  c <- names(which.max(summary(f))) #get the name of the most common fish
  l <- names(which.min(summary(f))) #get the name of the least common fish
  t <- sum(summary(f)) #get the total number of fish
  
 
  if(graph == TRUE) {
    #Plot title showing  total number, and most and least common fish
    plottitle = sprintf("Total number of fish: %s", t)
    
    #Histogram with count of fish types
    p <- ggplot(data.frame(name_vec=name_vec), 
                aes(name_vec, fill=name_vec)) + 
      geom_histogram(stat="count", 
                     show.legend = FALSE)+
      labs(title = plottitle,x = "Fish Type", y = "Count") +
      theme_minimal()
    
    
    values <- list( sprintf("Total number of fish: %s", t), 
                    sprintf("Most common fish: %s ", c), 
                    sprintf("Rarest fish: %s", l), p 
    )
    
  } else
    
    values <- list(sprintf("Total number of fish: %s", t), 
                  sprintf("Most common fish: %s ", c), 
                  sprintf("Rarest fish: %s", l)) 
  
  return(values)
}



