library(shiny)
library(bslib)
#library(shinythemes)
source("data.R")
source("ManzanidoEx05.R")
source("init_matrix.R")
source("result.R")
# UI
ui =page_navbar(
  # theme = shinytheme("united"),
  #titlePanel(div(class="title","Diet Problem Solver")),
  title = "Diet Problem Solver",
  nav_panel("Main",                 # panel that contains the food selection, etc.
          fluidRow(
            fluidRow(
              card(                           # card that contains the food selection
                card_header("Select Foods"), 
                card_body(
                  div(class = "custom-checkbox-group",
                      checkboxGroupInput("selected_foods",    # render the checkboxes for the foods
                                         label = NULL,
                                         choices = foods,
                                         selected = character(0),
                                         inline=TRUE)
                  ),
                )
              ),
              layout_columns(                                                 # this will hold the buttons place them per column
                actionButton("clear", "Reset", class = "btn-warning"),         # reset button
                actionButton("select_all", "Select All", class = "btn-success"),  # select all button
                actionButton("optimize", "Optimize", class = "btn-primary")   # optimize button
              )
              
            ),
            
            layout_columns(       # this the will hold the selected food card and the fluid row for the optimization result
              card(
                card_header("Selected Foods"), 
                card_body(div(class = "table-info", tableOutput("selected_table"))) # render the selected foods
              ),
              fluidRow(
                card(uiOutput("optimization_result_ui"),), # render the result of the optimization
                
                card(
                  card_header("Optimized menu"), 
                  card_body(
                    tableOutput("new_menu"))    # render the table for new menu
                )
              ),
 
            )
          )),
  nav_panel("Details",      # another panel for the details of the optimization result
    fluidRow(
      card(
        card_header("Final Solution"),
        card_body(
          tableOutput("finalSol")   #render the final solution table 
        )
        
      ),
      card(
        card_header("Details"),
        card_body(
          uiOutput("iterations"),)  # render each iterations occured in the simplex function
      ),
    )

    
  ),

  # inline css
  tags$style(HTML("   
    .custom-checkbox-group {
        display: flex;
        flex-direction: row; 
        flex-wrap: wrap; 
        gap: 20px; 
    }
    
    .checkbox-inline {
    margin:20px;
    }
   .infeasible-result {
     background-color: #f8d7da;
     color: #842029;
   }
     table.table-info {
    background-color: #d9edf7; 
    color: #31708f;
     }
  

  "))
)
# Server
server = function(input, output, session) {
  
  selected = reactiveVal(character(0)) # create reactive value object for the selected food 

  # event handler for the selecting foods
  observeEvent(input$selected_foods, {
    selected(input$selected_foods)
  })
  
  # event handler for the reset button
  observeEvent(input$clear, {
    selected(character(0))
    updateCheckboxGroupInput(session, "selected_foods", 
                             selected = character(0))
    # null the ooptimization result, new menu, iterations, and final solution
    output$optimization_result_ui = renderUI(NULL)
    output$new_menu = renderTable(NULL)
    output$iterations = renderUI(NULL)
    output$finalSol = renderTable(NULL)
  })
  
  # event handler for the select all button
  observeEvent(input$select_all, {
   
    selected(foods)  # select all foods
    updateCheckboxGroupInput(session, "selected_foods", 
                             selected = foods)
  })
  
  output$selected_table = renderTable({
    sel = selected()
    if (length(sel) == 0) { # null since nothing selected yet
      return(NULL)
    }
    in_foods = food_items$Foods %in% sel # get all the food items that is selected
    selected_foods = food_items[in_foods, , drop = FALSE] 
    if (nrow(selected_foods) > 0) { 
      return(selected_foods)  # return the selected food if > 0
    }
    return(NULL) 
  })
  
  # event handler for the button optimize
  observeEvent(input$optimize, {
    sel = selected()
    if (length(sel) == 0) {  # prompt the user when optimize is clicked but no selected food
      showModal(modalDialog(
        title = "No Foods Selected",
        "Please select at least one food item.",
        easyClose = TRUE
      ))
      return()
    }
    
    selected_indices = which(foods %in% sel) # get the indices of the selected food in the matrix food item
    constraints = create_constraint_matrix(selected_indices, nutri) # create the constraints by calling the function from init_matrix.R
    objective = create_objective_function(selected_indices, price_serve)  # create the objective function by calling the function from init_matrix.R
    
    print("objetive function")
    print(objective)
    
    print("constraints")
    print(constraints)
    rewritten_equ = rbind(constraints, objective)  #combined constraints and the the objective function
    transposed = t(rewritten_equ)   # transpose it
    
    # create initial tableau
    row_len = nrow(transposed) # get the row length of transposed matrix
    col_len = ncol(transposed) + length(selected_indices) # and the number of the selected selected food, it would be the x's
    initial_tableau = create_slack_variables(transposed,row_len, col_len, selected_indices) # create the inital tableau by calling the create_slack variables from init_matrix.R
    
    print("Initial Tableau")
    print(initial_tableau)
    
    output$iterations = renderUI(NULL)
    output$finalSol = renderTable(NULL)
    output$new_menu = renderTable(NULL)
    
    tryCatch({
      minimize = Simplex(initial_tableau, FALSE)  # call the simplex function, add False in 2nd arg for minimzation, in ManzanidoEx05.R
      if(!is.null(minimize)){
        minimize$basicSolution = round(minimize$basicSolution, digits = 2)
        finalSolution = minimize$basicSolution
        minimize$Z = round(minimize$Z, digits = 2)
        menu = create_result(minimize$basicSolution, selected_indices) # interpret the result by calling the create_result func in the result.R
        # opt_cost = paste("the cost of this optimal diet is $ ", minimize$Z, " per day.")
        }
    }, error = function(e) {
      print("Error in Simplex:")
      print(e)
    })
    
    # this will show the food
    output$optimization_result_ui = renderUI({
      if (is.null(minimize)) {
        #div(class = "infeasible-result",
          value_box(
            title = "It is not possible to meet the nutritional constraints with the food that you have selected. Possible reason is that the pivot element is 0.",
            value = "The problem is Infeasible ",
            theme = "pink"
          
        )

      }
      else{
        value_box(
          title = "The cost of this optimal diet per day is",
          value = minimize$Z,
          showcase = bsicons::bs_icon("currency-dollar"),
          theme = "bg-success"
          
        )
      }
    })
    if (!is.null(minimize)){
      output$new_menu = renderTable({
        return (menu)  # return the interpreted result
      })
      output$iterations = renderUI({
        if(length(minimize$iterations) == 0){  # check if iterations exist
          return(NULL)
        }
        
        
        iterations_list = lapply(seq_along(minimize$iterations), function(i) { # use lapply to make each iteration in iterations list 
          iteration = minimize$iterations[[i]]                                # have its own renderTable for tableau and basic solution
          
          div(   # create a container for the current iteration
            h4(paste("Iteration", i)), # current iteration
            
            tags$h5("Tableau:"),  # current tableau
            renderTable({
              data.frame(iteration$tableau)  # Convert inside renderTable
            }, colnames = FALSE), # remove colnames
            
            tags$h5("Basic Solution:"),  # current basic solution
            renderTable({
              data.frame(matrix(iteration$basicSolution, nrow = 1)) #create 
            }, rownames = FALSE, colnames = FALSE),  # remove row and column name
            
            hr()  # separator
          )
        })
        
        return(iterations_list)
      })
      
      output$finalSol = renderTable({ 
        data.frame(matrix(finalSolution, nrow = 1))  # Reshape the vector into a single row
      }, rownames = FALSE, colnames= FALSE) # Disable row names for a clean table
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

# References:
# https://r-lang.com/seq_along-function-in-r/
# https://www.statology.org/a-guide-to-apply-lapply-sapply-and-tapply-in-r/
# https://stackoverflow.com/questions/30443625/how-do-i-build-a-reactive-dataframe-in-r-shiny