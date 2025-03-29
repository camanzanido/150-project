source("data.R")
# Create basic constraint matrix function (without slack variables)
create_constraint_matrix = function(selected_indices, nutri) {
  # nutritional constraints (min, max)
  minMax = rbind(
    c(1900, 2500), # calories 
    c(0, 300),     # cholesterol 
    c(0, 65),    #total_fat 
    c(0, 2400),   # sodium 
    c(0, 300),    # carbohydrates
    c(25, 100),   # diet fiber 
    c(50, 100),   # protein
    c(5000, 50000),  # vitamin A
    c(50, 20000),   #vitamin C 
    c(800, 1600),    # calcium
    c(10, 30)       # iron
  )
  #print(constraints)
  
  n_vars = length(selected_indices)
  n_nutrient_constraints = nrow(minMax) * 2  # each nutrient has min and max
  n_serving_constraints = n_vars * 2         # each food has min and max serving
  total_constraints = n_nutrient_constraints + n_serving_constraints
  
  # Create constraint matrix
  lhs = matrix(0, nrow = total_constraints, ncol = n_vars)
  rhs = numeric(total_constraints)
  
  # Fill nutrient constraints
  row_idx = 1
  for (i in 1:nrow(minMax)) {
    nutrient_vals = nutri[selected_indices, i]
    
    # Minimum nutrient constraint (≥)
    lhs[row_idx, ] = nutrient_vals
    rhs[row_idx] = minMax[i,1]
    row_idx = row_idx + 1
    
    # Maximum nutrient constraint (≤) converted to (≥) by multiplying by -1
    lhs[row_idx, ] = (-1) * nutrient_vals
    rhs[row_idx] = (-1) * minMax[i,2]
    row_idx = row_idx + 1
  }
  
  # serving size constraints for each food (0 ≤ xi ≤ 10)
  for (i in 1:n_vars) {
    # Minimum serving constraint (≥ 0)
    lhs[row_idx, i] =1
    rhs[row_idx] = 0
    row_idx = row_idx + 1
    
    # Maximum serving constraint (≤ 10) converted to (≥) by multiplying by -1
    lhs[row_idx, i] = -1
    rhs[row_idx] = -10  # maximum 10 servings
    row_idx = row_idx + 1
  }
  
  const = cbind(lhs, rhs)
  return(const)
}

# Create basic objective function 
create_objective_function = function(selected_indices, price_serve) {
  obj = price_serve[selected_indices] # get the price of the selected food by using their indices
  obj = c(obj, 1) # add 1 for Z
  return(obj)
}

# create slack variables
create_slack_variables = function(matrix, row_len, col_len, selected_indices){
  n = length(selected_indices)
  init_tableau = matrix(0, nrow = row_len, ncol = col_len +1)  # add +1 for the Z
 
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)-1){ 
      if(i == nrow(matrix)){
        init_tableau[i,j] = (-1) * matrix[i,j]  # multiply by -1 the objective function 
      }
      
      else {init_tableau[i,j] = matrix[i,j]} # assign matrix value to the tableau

    }
  }
  
  # this will create the x variables in the tableau
  idx = ncol(matrix)
  for (i in 1:row_len){
    for (j in ncol(matrix):(col_len+1)){   
      init_tableau[i,idx] = 1 
    }
    idx = idx+1
  }
  
  for (i in 1:row_len-1){   # iterate until row length -1 because the [nrow,ncol] of the tableau is 0
    init_tableau[i,ncol(init_tableau)] = matrix[i,ncol(matrix)] # assign the value of rhs to the last column of the tableau
  }
  return (init_tableau)
}
