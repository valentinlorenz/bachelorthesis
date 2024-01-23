means_table = function(var_data, clusters, n){
  # combine data with cluster numbers
  var_data$cluster <- clusters
  
  # create new dataframe with ncol(var_data) columns and as many rows as there are clusters
  means <- data.frame(matrix(ncol=length(var_data), nrow=n))
  colnames(means) <- colnames(var_data)
  
  # remove ID column
  means <- means[2:length(means)]
  
  # go through every cluster
  for (i in 1:n){
    to_calculate_mean <- var_data[var_data$cluster == i,] # subselect all values of a specific cluster
    to_calculate_mean <- to_calculate_mean[2:length(to_calculate_mean)] # remove ID column
    
    # calculate mean for every variable
    for (k in 1:length(to_calculate_mean)){
      means[i,k] <- mean(to_calculate_mean[[k]], na.rm = TRUE)
    }
  }
  
  # add row that has values for all clusters
  
  means[9,] <- c(colMeans(means[1:length(means)-1]), 0)
  
  return(means)
}

sd_from_mean <- function(means){
  
  # create new, empty table to write results into
  sd_table <- data.frame(matrix(nrow=nrow(means)-1, ncol=ncol(means)))
  colnames(sd_table) <- colnames(means)
  
  # for every cluster
  for (i in 1:(nrow(sd_table))){
    # for every variable
    for (k in 1:(ncol(sd_table))){
      # calculate deviation of cluster mean from mean of all clusters
      deviation <- means[i, k] - means[9, k]
      
      # calculate standard deviation of variable
      sd <- sd(means[1:nrow(sd_table),k])
      
      # how many standard deviations from the mean?
      
      sd_dev <- deviation/sd
      
      # filling with ++++ (> 2 sd), +++ (> 1.5 SD), ++ (> 1 SD), + (> 0.5 SD)
      
      if (sd_dev >= 2){
        sd_table[i, k] <- "++++"
      }
      else if (sd_dev >= 1.5){
        sd_table[i, k] <- "+++"
      }
      else if (sd_dev >= 1){
        sd_table[i, k] <- "++"
      }
      else if (sd_dev >= 0.5){
        sd_table[i, k] <- "+"
      }
      
      # filling with ---- (> 2 sd), --- (> 1.5 SD), -- (> 1 SD), - (> 0.5 SD)
      
      else if (sd_dev <= -2){
        sd_table[i, k] <- "----"
      }
      else if (sd_dev <= -1.5){
        sd_table[i, k] <- "---"
      }
      else if (sd_dev <= -1){
        sd_table[i, k] <- "--"
      }
      else if (sd_dev <= -0.5){
        sd_table[i, k] <- "-"
      }
      
      # otherwise leave field empty
      
      else{
        sd_table[i, k] <- ""
      }
      
      
    }
  }
  
  sd_table$cluster <- 1:nrow(sd_table)
  
  return(sd_table)
  
}


