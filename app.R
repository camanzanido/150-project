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
  nav_panel("Main", 
          fluidRow(
            fluidRow(
              card(
                card_header("Select Foods"), 
                card_body(
                  div(class = "custom-checkbox-group",
                      checkboxGroupInput("selected_foods", 
                                         label = NULL,
                                         choices = foods,
                                         selected = character(0),
                                         inline=TRUE)
                  ),
                )
              ),
              layout_columns(
                actionButton("clear", "Reset", class = "btn-warning"),
                actionButton("select_all", "Select All", class = "btn-success"),
                actionButton("optimize", "Optimize Diet", class = "btn-primary")
              )
              
            ),
            
            layout_columns(
              card(
                card_header("Selected Foods"), 
                card_body(div(class = "table-info", tableOutput("selected_table")))
              ),
              fluidRow(
                card(uiOutput("optimization_result_ui"),),
                
                card(
                  card_header("Optimized menu"), 
                  card_body(
                    tableOutput("new_menu"))
                )
              ),
 
            )
          )),
  nav_panel("Details",
    fluidRow(
      card(
        card_header("Final Solution"),
        card_body(
          tableOutput("finalSol")
        )
        
      ),
      card(
        card_header("Details"),
        card_body(
          tableOutput("iterations"),)
      ),
    )

    
  ),

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
  # .optimization-result {
  #   padding: 20px;
  #   margin: 15px 0;
  #   background-color: #d1e7dd;
  #     border-radius: 8px;
  #   color: #0f5132;
  #     text-align: center;
  #   font-size: 1.2em;
  #   font-weight: 500;
  # }
  # 
  # .infeasible-result {
  #   background-color: #f8d7da;
  #     color: #842029;
  # }
)
# Server
server = function(input, output, session) {
  
  selected = reactiveVal(character(0)) # create reactive value object for the user 

  # event handler for the selecting foods
  observeEvent(input$selected_foods, {
    selected(input$selected_foods)
  })
  
  # event handler for the reset button
  observeEvent(input$clear, {
    selected(character(0))
    updateCheckboxGroupInput(session, "selected_foods", 
                             selected = character(0))
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
    
    output$iterations = renderUI(NULL)
    output$finalSol = renderTable(NULL)
    output$new_menu = renderTable(NULL)
    
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
              
              tags$h5("Tableau:"),
              renderTable({
                tableau_df
              }, colnames = FALSE),
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
        #div(class = "infeasible-result",
          value_box(
            title = "The problem is",
            value = "Infeasible ",
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
        return (menu)
      })
    }
    if (!is.null(minimize)) {
      output$finalSol = renderTable({
       
        data.frame(matrix(finalSolution, nrow = 1))  # Reshape the vector into a single row
      }, rownames = FALSE, colnames= FALSE) # Disable row names for a clean table
    }
    
 
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)