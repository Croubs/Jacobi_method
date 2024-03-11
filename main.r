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
    
    # Function to create the vectors
    create_vector <- function(n, message) {
      outMatrix <- matrix(0, nrow = n, ncol = 1)
      
      for (i in 1:n) {
        prompt <- paste("Enter the value for the", message,i,": ")
        item <- readline(prompt = prompt)
        
        outMatrix[i,1] <- as.numeric(item)
      }
      
      return(outMatrix)
    }

    # Jacobi method function
    jacobi_method <- function(A, b, x_anterior, n, relative_error) {
      # Create 2 matrices
      D_inverse <- matrix(0, nrow = n, ncol = n)
      R <- matrix(0, nrow = n, ncol = n)

      # Variable to know if there's an error
      BAND <- FALSE

      # Set values for D and R
      for (i in 1:n) {
        for (j in 1:n) {
          # Can't be 0s in the principal diagonal
          if (A[i,i] == 0) {
            print("Error, 0 in the principal diagonal")
            BAND <- TRUE
            break
          } else {
            if (i == j) {
              D_inverse[i,j] <- 1/A[i,j]
              R[i,j] <- 0
            } else {
              R[i,j] <- A[i,j]
              D_inverse[i,j] <- 0
            }
          }
        }
      }

      if (BAND) {
        return(0)
      }

      # Recursive part
      error <- 1000
      x <- matrix(0, nrow = n, ncol = 1)
      while (error > relative_error) {
        # Get new values
        x <- D_inverse %*% (b - R%*%x_anterior)

        # Get the relative error for the firts value
        error <- abs((x[1,1]-x_anterior[1,1])/x[1,1])
        # Now for the rest of values
        for (i in 2:n) {
          aux <- abs((x[i,1]-x_anterior[i,1])/x[i,1])
          # Get the biggest relative error
          if (aux > error) {
            error <- aux
          }
        }

        # Set new old values
        x_anterior <- x

        # Show the current solution
        success_message = paste("Solutions with a relative error of: ", error)
        print(success_message)
        print(x)
      }
      
      # Show the final solution
      success_message = paste("Solutions with a relative error of: ", error)
      print(success_message)
      print(x)

      return(1)
    }
    
    # Reorder the matrix function
    reorder_matrix <- function(matrix) {
      # Move the biggest values to the principal diagonal
      for (i in 1:n) {
        for (j in 1:n) {
          if (abs(matrix[i,i]) < abs(matrix[i,j])) {
            # Move the row
            aux <- matrix[,i]
            matrix[,i] <- matrix[,j]
            matrix[,j] <- aux
          }
        }
      }

      for (i in 1:n) {
        sum <- 0
        for (j in 1:n) {
          # Sum every value in the row but the one in the principal diagonal
          if(i != j) {
            sum <- sum + abs(matrix[i,j])
          }
        }

        # Check what's bigger
        if(abs(matrix[i,i]) < sum) {
          message <- paste("Error: the element in the principal diagonal of the",i,"row is not bigger than the rest elements sum")
          print(message)
          print("Matrix reordered:")
          print(matrix)
          return(0)
        }
      }

      return(matrix)
    }

    # Get matrix size
    res <- readline(prompt = "Enter the n value for nxn matrix: ")
    n <- as.numeric(res)
    
    # Get the system matrix, values vector and initial values
    custom <- create_custom_matrix(n)
    custom <- reorder_matrix(custom)
    if (is.matrix(custom)) {
      values <- create_vector(n, "equation")
      initial <- create_vector(n, "variable")

      # Get relative error
      res <- readline(prompt = "Enter the relative error: ")
      error <- as.numeric(res)

      jacobi_method(custom, values, initial, n, error)
    }
    
    cicle <- readline("Do you want to try again?\n press 1 to continue: ")
  }
  print("End of program...")
}

sistemEquations()