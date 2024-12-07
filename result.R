source("data.R")
create_result = function(basicSolution, selected_indices){
  col = length(basicSolution)
  til = length(selected_indices)
  start = col-til
  new_serving = c()

  #print(paste("start: ", start))
  indx = start
  # get the indices of the ffo d in the 
  new_menu_idx = c()
  cost = c()
  for (i in 1:length(selected_indices)){
    if (basicSolution[indx] != 0){
      new_serving = c(new_serving,basicSolution[indx])
      cost = c(cost, basicSolution[indx]*price_serve[selected_indices[i]])
      new_menu_idx = c(new_menu_idx, selected_indices[i])
    }
    indx = indx + 1
  }
  #print(new_serving)
  
  food_menu = foods[new_menu_idx]
  #print(new_menu_idx)
  new_cost = round(cost, digits = 2)
  #print(new_cost)
  
  return(cbind.data.frame(Foods = food_menu, Servings = new_serving, Cost = new_cost))
  
  
  
}