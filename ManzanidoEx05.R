# Manzanido, Clarence A.
# 2022-08808
# CMSC 150 B1L


# Function that do the simplex method for the minimization and maximization
Simplex <-function(tableau, isMax){
  # initialize the variables
  matrix = tableau
  n = nrow(matrix)
  m = ncol(matrix)
  basicSolution = numeric(m-1) # initialize the basic solution with 0s(m-1 to exclude the RHS)
  basicSolPerIter = numeric(m-1)
  hasNegative = TRUE
  Z = 0
  iterationNum = 1
  iterations = list()
  # iterate until theres no negative values in the bottom row of the matrix
  while(hasNegative){
    iterations[[iterationNum]] = list(
      tableau = matrix,
      basicSolution = basicSolPerIter
    )
    #get the the pivot column
    PC = 0    #pivot column index
    highestnegative = 0
    for (i in 1:(m-1)){
      # check if the current element is negative and if it has a higher magnitude than the current highest
      if (matrix[n, i] < 0 && matrix[n,i] < highestnegative) { 
          PC = i                # update the pivot column index
          highestnegative = matrix[n, i]    # update the highest negative
      }
    }
    
    # break the loop when theres no negative value in the bottom row
    if(highestnegative == 0){
      hasNegative = FALSE
      break
    }
    
    # get the and pivot row
    PE = 99999
    PR = 0
    for (i in 1:(n-1)){ #iterate in the pivot column to get the pivot row
      if(matrix[i, PC] > 0){     # do the test ratio
        curElement = matrix[i, m] / matrix[i, PC]  
        if( curElement< PE & curElement != 0  ){
          PR = i        # update the index of the PR with the current lowest positive test ratio
          PE = curElement
        }
      }
    }
    
    if (PR == 0){
      return (NULL)
    }

    #pivot element
    PE = matrix[PR, PC]  
    
    # normalize the pivot row
    matrix[PR, ] = matrix[PR, ] / PE
    
    
    # Eliminate other entries in the current column
    for (i in 1:n){
      if(PR != i){
        NR = matrix[i, PC] * matrix[PR, ]
        matrix[i, ] = matrix[i, ] - NR
      }
    }
    
    # Tableau for each iteration
    print(paste("Iteration: ", iterationNum))
    print(matrix)
    
    

    
    # basicSolution for each iteration
    print("Basic Solution")
    for (i in 1:(m-1)){
      row = i
      if(i == (m-1) && matrix[n, (m-1)]){ # the final answer for Z is the value at the last row, last column 
        row = i + 1 
      }
      basicSolPerIter[i] = matrix[n, row] # insert the solution to basicSolution
    }
    print(basicSolPerIter)
    
    iterationNum = iterationNum + 1
    
  }
  
  # get the solutions 
  if (isMax){     # if the input is for maximization
    for (i in 1:(m-1)){ # iterate per column e
      nonzeroCounter = 0   #counter for nonzero
      row = 0     # placeholder index of the row that have a nonzero
      for (j in 1:n){  #iterate per row
        if (matrix[j, i] != 0){
          nonzeroCounter = nonzeroCounter + 1   # increment the counter for nonzeroes
          row = j
        }
      }
      
      if(nonzeroCounter == 1 ){ # it only has solution when it has exactly one nonzero 
        basicSolution[i] = matrix[row, m] # insert the solution per column in the basicSolution
      }
      else {
        basicSolution[i] = 0  #insert 0 since no solution
      }
      
    }
    Z = matrix[n, m]    # update the maximize value
    
    
  } else {        # else, for the minimization
    for (i in 1:(m-1)){
      row = i
      if(i == (m-1) && matrix[n, (m-1)]){ # the final answer for Z is the value at the last row, last column 
        row = i + 1 
      }
      basicSolution[i] = matrix[n, row] # insert the solution to basicSolution
    }
    Z = matrix[n, m]  # update  Z for the minimization
  }
  
  # return the labeled list containing the final matrix, computed basic solution and Z
  return(list(
    finalTableau = matrix, 
    basicSolution = basicSolution, 
    Z = Z,
    iterations = iterations
  ))
  
}

# Input Parameter
tableau = matrix(c(6,14,1,0,0,0,800,
                   1000,2000,0,1,0,0,120000,
                   1,1,0,0,1,0,93,
                   -4000,-5000,0,0,0,1,0), 4,7, TRUE )

isMax = TRUE

Simplex(tableau, isMax)
