library(shiny)
source("data.R")
source("ManzanidoEx05.R")
source("init_matrix.R")
source("result.R")
# UI
ui = fluidPage(
  titlePanel(div(class="title","Diet Problem Solver")),
  
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

    .optimization-result {
        padding: 20px;
        margin: 15px 0;
        background-color: #d1e7dd;
        border-radius: 8px;
        color: #0f5132;
        text-align: center;
        font-size: 1.2em;
        font-weight: 500;
      }
      
    .infeasible-result {
        background-color: #f8d7da;
        color: #842029;
      }
  ")),
  # Top selection panel
  fluidRow(
    column(12,
           wellPanel(
             h4("Select Foods"),
             div(class = "custom-checkbox-group",
                 checkboxGroupInput("selected_foods", 
                                    label = NULL,
                                    choices = foods,
                                    selected = character(0),
                                    inline=TRUE)
             )
           )
    )
  ),
  
  # Buttons row
  fluidRow(
    column(12,
           div(style = "text-align: center; margin: 10px 0;",
               actionButton("clear", "Reset", class = "btn-warning"),
               actionButton("select_all", "Select All", class = "btn-success"),
               actionButton("optimize", "Optimize Diet", class = "btn-primary")
           )
    )
  ),
  
  # Selected foods table
  fluidRow(
    column(12,
           # h3("Selected Foods"),
           tableOutput("selected_table"),
           uiOutput("optimization_result_ui"),
           tableOutput("new_menu"),
           # verbatimTextOutput("finalSol")
           
    )
  ),
  # Add this to your existing UI definition
  fluidRow(
    column(12,
           h3("Simplex Iterations"),
           tableOutput("iterations")
    )
  ),
  # Add this to your existing UI definition
  fluidRow(
    column(12,
           h3("Final Solution"),
           tableOutput("finalSol")
    )
  )
)
# Server
server = function(input, output, session) {
  
  selected = reactiveVal(character(0)) # create reactive value object for the user 
  
  iterationData = reactiveValues(
    tableaus = list(), # store intermediate tableaus
    finalSolution = NULL,
    finalCost = NULL,
    message = NULL
  )
  # event handler for the selecting foods
  observeEvent(input$selected_foods, {
    selected(input$selected_foods)
  })
  
  # event handler for the reset button
  observeEvent(input$clear, {
    selected(character(0))
    updateCheckboxGroupInput(session, "selected_foods", 
                             selected = character(0))
  })
  
  # event handler for the select all button
  observeEvent(input$select_all, {
   
    selected(foods)  # select all foods
    updateCheckboxGroupInput(session, "selected_foods", 
                             selected = foods)
  })
  
  observe({
    lapply(seq_along(iterationData$tableaus), function(i) {
      local({
        iteration = i
        output[[paste0("tableau_", iteration)]] = renderTable({
          iterationData$tableaus[[iteration]]
        })
      })
    })
  })
  output$tableaus = renderUI({
    if (length(iterationData$tableaus) == 0) return(NULL)
    div(
      lapply(seq_along(iterationData$tableaus), function(i) {
        tableOutput(paste0("tableau_", i))
      })
    )
  })
  output$selected_table = renderTable({
    sel = selected()
    if (length(sel) == 0) {
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
    constraints = create_constraint_matrix(selected_indices, nutri) # create the constraints
    objective = create_objective_function(selected_indices, price_serve)  # create the objective function
    
    print("objetive function")
    print(objective)
    
    print("constraints")
    print(constraints)
    rewritten_equ = rbind(constraints, objective)  #combined constraints and the the objective function
    transposed = t(rewritten_equ)   # transpose it
    
    # create initial tableau
    row_len = nrow(transposed) # get the row length of transposed matrix
    col_len = ncol(transposed) + length(selected_indices) # and the number of the selected selected food, it would be the x's
    initial_tableau = create_slack_variables(transposed,row_len, col_len, selected_indices) # create the inital tableau
    
    print("Initial Tableau")
    print(initial_tableau)
    
    tryCatch({
      minimize = Simplex(initial_tableau, FALSE)
      if(!is.null(minimize)){
        minimize$basicSolution = round(minimize$basicSolution, digits = 2)
        finalSolution = minimize$basicSolution
        minimize$Z = round(minimize$Z, digits = 2)
        menu = create_result(minimize$basicSolution, selected_indices)
        opt_cost = paste("the cost of this optimal diet is $ ", minimize$Z, " per day.")
      
        output$iterations = renderUI({
          # Check if iterations exist
          if(length(minimize$iterations) == 0){
            return(NULL)
          }
          
          # Create a container to hold all iterations
          all_iterations = list()
          
          # Loop through each iteration
          for(i in 1:length(minimize$iterations)) {
            # Get the current iteration data
            iter = minimize$iterations[[i]]
            
            # Convert tableau to a dataframe
            tableau_df <- as.data.frame(iter$tableau)
            
            # Create a div for the current iteration
            current_iteration = div(
              h4(paste("Iteration", i)),
              
              # Tableau table
              tags$h5("Tableau:"),
              renderTable({
                tableau_df
              }, colnames = FALSE),
              
              # Basic Solution table
              tags$h5("Basic Solution:"),
              renderTable({
                data.frame(matrix(iter$basicSolution, nrow = 1))
              }, rownames = FALSE, colnames = FALSE),
              
              # Separator
              hr()
            )
            
            # Add the current iteration to the list
            all_iterations[[i]] = current_iteration
          }
          
          # Combine all iterations into a single output
          do.call(tagList, all_iterations)
        })
        }
    }, error = function(e) {
      print("Error in Simplex:")
      print(e)
    })
    
    # this will show the food
    output$optimization_result_ui = renderUI({
      if (is.null(minimize)) {
        div(class = "optimization-result infeasible-result",
            "The problem is infeasible")
      }
      else{
        div(class = "optimization-result", 
            opt_cost)
      }
    })
    if (!is.null(minimize)){
      output$new_menu = renderTable({
        return (menu)
      })
    }
    if (!is.null(minimize)) {
      output$finalSol = renderTable({
       
        data.frame(matrix(finalSolution, nrow = 1))  # Reshape the vector into a single row
      }, rownames = FALSE, colnames= FALSE) # Disable row names for a clean table
    }
    
    # if(!is.null(minimize)){
    # 
    # }
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)