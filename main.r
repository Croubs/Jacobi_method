sistemEquations<- function() {
  cicle<-1
  while (cicle==1) {    
    # Function to create a matrix with user provided values
    create_custom_matrix <- function(n) {
      outMatrix <- matrix(0, nrow = n, ncol = n)
      
      for (i in 1:n) {
        for (j in 1:n) {
          prompt <- paste("Enter the [", i, ",", j, "] number: ")
          item <- readline(prompt = prompt)
          
          outMatrix[i,j] <- as.numeric(item)
        }
      }
      
      return(outMatrix)
    }
    
    # Function to create the vector of equations values
    create_values_matrix <- function(n) {
      outMatrix <- matrix(0, nrow = n, ncol = 1)
      
      for (i in 1:n) {
        prompt <- paste("Enter the value of the ", i," equation: ")
        item <- readline(prompt = prompt)
        
        outMatrix[i,1] <- as.numeric(item)
      }
      
      return(outMatrix)
    }
    
    # Get matrix size
    res <- readline(prompt = "Enter the n value for nxn matrix: ")
    n <- as.numeric(res)
    
    # Get the system matrix and values vector
    custom <- create_custom_matrix(n)
    values <- create_values_matrix(n)
    
    cicle <- readline("Do you want to try again?\n press 1 to continue: ")
  }
  print("End of program...")
}

sistemEquations()