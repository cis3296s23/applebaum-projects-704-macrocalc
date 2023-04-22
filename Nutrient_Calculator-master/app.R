library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(reticulate)
library(jsonlite)
library(shinyjs)
library(shinyFiles)

rsconnect::setAccountInfo(name='macrocalc',
                          token='154D97C14C26B3416F6B1C64CFE8EB01',
                          secret='A3XVc3aLgcEk3vl8e8mm5VcgMzAnJcwbXtyc4ULS')

# load files from Canada Nutrient File
nutr_files <- list.files(pattern = "*.rda")
lapply(nutr_files,load,.GlobalEnv)
# format quantities for ingredients

# take everything before parenthesis
ca_measure_name$units <- regmatches(ca_measure_name$MeasureDescription, regexpr("^([^(]+)", ca_measure_name$MeasureDescription, perl = T))
# only take what is in parenthesis
ca_measure_name$description <- gsub("^([^(]+)", "", ca_measure_name$MeasureDescription, perl = T)
# extract numeric values
r <- regexpr("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", ca_measure_name$units)
out <- rep(NA,nrow(ca_measure_name))
out[r!=-1] <- regmatches(ca_measure_name$units, r)
ca_measure_name$numeric <- out
# convert fractions to decimal
fractions <- grep("\\/", ca_measure_name$numeric)
ca_measure_name$numeric[fractions] <- sapply(ca_measure_name$numeric[fractions], function(x) eval(parse(text=x)))
# fill in blank numeric values
ca_measure_name$numeric[is.na(ca_measure_name$numeric)] <- 1
# everything numberic
ca_measure_name$numeric <- round(as.numeric(ca_measure_name$numeric), 5)
# now remove numbers from units
ca_measure_name$units <- gsub("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", "", ca_measure_name$units)


# format ingredient choices
ca_food_choices <- ca_food_name$FoodID
names(ca_food_choices) <- ca_food_name$FoodDescription


# format daily values
daily_value <- read.table("daily_values.txt", sep = "\t", header=T, stringsAsFactors = F)

# navbar Solution [might not be good for mobile view]
# ui <- shinyUI(
#   navbarPage("Navbar test", 
#              tabPanel("Home", h4("This will be the homepage where users can interact with recipes/ingredients(uploading/modifying/favoriting/adding to log).")),
#              tabPanel("Activity Page", h4("In the activity page the user can view total breakdown/comapre macros/remove meals/view their log")),
#              tabPanel("Settings", h4("here the user may be able to edit their profile(set goals)")
#                       )
#              )
# )


ui <- dashboardPage(
  dashboardHeader(title = "MacroCalc"),
  skin = "purple",
  dashboardSidebar(
    tags$head(
      HTML('<script src="https://accounts.google.com/gsi/client" async defer></script><script src="https://unpkg.com/jwt-decode/build/jwt-decode.js"></script>'),
      includeScript("signin.js"),
      useShinyjs()
    ),
    sidebarMenu(
      menuItem("Home", tabName = "Hometab" , icon = icon("dashboard"),
               menuSubItem("Ingredient Selection", tabName = "sub1"),
               menuSubItem("Recipes", tabName = "subhome")
      ),
      menuItem("Activity Page", tabName =  "Activitytab", icon = icon("calendar")),
      tags$p("Notice: Consult your physician.")
      # OldNote: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling.
    ),
    div(id="g_id_onload", "data-callback"="handleCredentialResponse", "data-client_id"="789616587258-lt9ji16j9u7jp998itd5kivgq249t0v3.apps.googleusercontent.com", "data-context"="signin",
        "data-ux_mode"="popup", "data-auto_prompt"="true", "data-auto_select"="true"),
    div(id="g_id_signin", class="g_id_signin", "data-type"="standard", "data-shape"="rectangular", "data-theme"="outline", "data-text"="signin_with", "data-size"="large", "data-logo_alignment"="left"
    ),
    useShinyjs()
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sub1", 
		  selectizeInput(
			'food_id', '1. Ingredient', choices = ca_food_choices,
			options = list(
			  placeholder = 'Type to search for ingredient',
			  onInitialize = I('function() { this.setValue(""); }')
			)
		  ),
		  conditionalPanel('input.food_id != ""', 
						   selectizeInput('measure_unit', '2. Measure Unit', choices = c("Select an ingredient" = "")),
						   numericInput('quantity', '3. Quantity', value = 1, min = 0, step = 1)),
		  actionButton("add", "Add ingredient"),
		  actionButton("remove", "Remove ingredient"),
		  numericInput("serving", "Number of servings contained", min = 0.01, step = 1, value = 1),
		  hidden(textInput("meal_name", "Meal Name:")),
		  fluidRow(
			box(title = "Ingredients",
				solidHeader = T,
				width = 4,
				collapsible = T,
				div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
			box(title = "Nutrition Table",
				solidHeader = T,
				width = 4, 
				collapsible = T,
				collapsed = F,
				tags$p(textOutput("serving", inline = T)),
				div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;"))
		  )
		  # ,
		  # fluidRow(
		  #   box(title = "Nutrition Table",
		  #       solidHeader = T,
		  #       width = 4, 
		  #       collapsible = T,
		  #       collapsed = F,
		  #       tags$p(textOutput("serving", inline = T)),
		  #       div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;"))
		  #   )

      ),
      
      # tabItem(tabName = "subhome", 
      #         actionButton("save_recipe", "Save Recipe", icon = shiny::icon("cloud-arrow-down")),
      #         downloadButton('download_ingredient_json', 'Download Ingredients'),
      #         fileInput("upload_ingredient_json", "Upload Ingredients", accept = ".json"),
      # ),
      tabItem(tabName = "subhome", 
              fluidRow(
                box(title = "Your recipes",
                    solidHeader = T,
                    width = 12,
                    collapsible = T,
                    div(actionButton("save_recipe", "Save Recipe", icon = shiny::icon("cloud-arrow-up")),
                        hidden(actionButton("delete_recipe", "Delete Recipe", icon = shiny::icon("trash"))),
                        hidden(actionButton("save_log", "Save to log", icon = shiny::icon("save"))),
                        style = "font-size: 70%; margin: 5px;"
                    ),
                    div(textOutput("recipe_table_edit_id"), style = "font-size: 70%; margin: 5px;"),
                    div(textOutput("recipe_table_edit_name"), style = "font-size: 70%; margin: 5px;"),
                    div(textOutput("recipe_table_edit_serving"), style = "font-size: 70%; margin: 5px;"),
                    div(DT::DTOutput("recipe_table_edit"), style = "font-size: 70%; margin: 5px;"),
                    div(actionButton("load_recipe", "Load All Recipe", icon = shiny::icon("cloud-arrow-down")), style = "font-size: 70%; margin: 5px;"),
                    div(DT::DTOutput("recipe_table"), style = "font-size: 70%; margin: 5px;")
                )
              ),
              fluidRow(
                box(title = "Download / Upload Ingredients",
                    solidHeader = T,
                    width = 12,
                    div(downloadButton('download_ingredient_json', 'Download Ingredients'), style = "font-size: 70%;"),
                    div(fileInput("upload_ingredient_json", "Upload Ingredients", accept = ".json"), style = "font-size: 70%;")
                )
              )
      ),
      tabItem(tabName = "Activitytab",
              fluidRow(
                box(title = "Your Log",
                    solidHeader = T,
                    width = 15,
                    collapsible = T,
                    hidden(actionButton("delete_entry", "Delete Entry", icon = shiny::icon("trash"))),
                    div(DT::DTOutput("log_table"), style = "font-size: 70%; margin: 5px;")
                )
              ),
              fluidRow(
                valueBoxOutput("calories"),
                valueBoxOutput("over_nutrient"),
                valueBoxOutput("rich_nutrient")
              ),
              fluidRow(
                # box(title = "Ingredients",
                #     solidHeader = T,
                #     width = 4,
                #     collapsible = T,
                #     div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
                box(title = "Macronutrients", solidHeader = T,
                    width = 8, collapsible = T,
                    plotlyOutput("macro_plot"))
              ), # row
              fluidRow(
                # box(title = "Nutrition Table",
                #     solidHeader = T,
                #     width = 4, 
                #     collapsible = T,
                #     collapsed = F,
                #     tags$p(textOutput("serving", inline = T)),
                #     div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;")),
                box(title = "Minerals", solidHeader = T,
                    width = 8, collapsible = T,
                    plotlyOutput("mineral_plot"))
              ),# row
              fluidRow(
                box(title = "Vitamins", solidHeader=T,
                    width = 12, collapsible = T,
                    plotlyOutput("vitamin_plot"))
              ) # row
      )
    ) # tabItems
  ) # body
  
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ########## Global Var here
  g_user_email <- reactiveVal("tranbaoson2005@gmail.com")
  g_user_name <- reactiveVal("son tran")
  # define a reactive value to track authentication state
  g_authenticated <- reactiveVal(FALSE)
  g_food_id <- reactiveVal(0)
  g_measure_unit <- reactiveVal("")
  g_quantity <- reactiveVal(0)
  g_meal_data <- reactiveVal(list(list()))
  g_edit_meal_id <- reactiveVal(0)
  g_all_recipe_rendered <- reactiveVal(FALSE)
  
  log_data <- reactiveValues(data_df = NULL)
  
  ########## define function here
  clear_ing_df <- function() {
    ing_df$df <- data.frame("quantity" = numeric(), 
                            "units" = character(), 
                            "ingredient_name" = character(), 
                            "FoodID" = numeric(), 
                            stringsAsFactors = F)
    ing_df$measure <- data.frame("numeric" = numeric(),
                                 "units" = character(),
                                 "description" = character(),
                                 "ConversionFactorValue" = numeric(),
                                 "MeasureID" = numeric(),
                                 "FoodID" = numeric(),
                                 stringsAsFactors = F)
    return(ing_df)
  }
  
  clear_all_data <- function() {
    clear_ing_df()
    ingredients_list(ing_df$df)
    clear_edit_data()
  }
  
  clear_edit_data <- function() {
    g_edit_meal_id(0)
    updateTextInput(session, "meal_name", value = "")
    updateNumericInput(session, "serving", value = 1)
  }
  
  load_ingredients_data <- function(my_object) {
    # clear existing data before upload ingredient
    clear_ing_df()
    ingredients_list(ing_df$df)
    
    for (i in 1:nrow(my_object)) {
      # cat(paste0("Quantity: ", my_object$quantity[i], "\n"))
      # cat(paste0("Units: ", my_object$units[i], "\n"))
      # cat(paste0("Ingredient name: ", my_object$ingredient_name[i], "\n"))
      # cat(paste0("Food ID: ", my_object$FoodID[i], "\n\n"))
      
      # prepare to import
      g_food_id(as.numeric(my_object$FoodID[i]))
      g_measure_unit(my_object$units[i])
      g_quantity(as.numeric(my_object$quantity[i]))
      
      ing_df$df[nrow(ing_df$df) + 1,] <- c(g_quantity(),
                                           g_measure_unit(),
                                           my_object$ingredient_name[i],
                                           g_food_id())
      
      # [1] "1"            " fritter "    "Corn fritter" "6"
      # [1] "1"                                "ml "                              "Chinese dish, chow mein, chicken" "5" 
      # [1] quantity        units           ingredient_name FoodID         
      # <0 rows> (or 0-length row.names)
      # [1] numeric               units                 description           ConversionFactorValue MeasureID             FoodID               
      # <0 rows> (or 0-length row.names)
      
      # [1] "input measure:"
      # # A tibble: 1 × 6
      # numeric units description ConversionFactorValue MeasureID FoodID
      # <dbl> <chr> <chr>                       <dbl>     <int>  <int>
      #   1     100 ml    ""                          0.930       341      5
      
      
      # get actual working ingredient dataframe for dplyr
      input_measure <- measure_df()
      input_measure <- input_measure[paste(measure_df()$units, measure_df()$description) == g_measure_unit(), ]
      if(nrow(input_measure) > 1){
        input_measure <- input_measure[which(abs(input_measure$numeric-g_quantity())==min(abs(input_measure$numeric-g_quantity()))),]
      }
      isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
    }
    ingredients_list(ing_df$df) 
  }
  
  
  
  ########## SAVE RECIPE
  # Define a reactive variable to store the list of ingredients
  ingredients_list <- reactiveVal(list())
  
  # Delete recipe
  observeEvent(g_edit_meal_id(), {
    if (g_edit_meal_id() > 0) {
      show("delete_recipe")
      show("save_log")
    }
    else {
      hide("delete_recipe")
      hide("save_log")
    }
  })
  observeEvent(input$delete_recipe, {
    if (g_edit_meal_id() > 0) {
      # Import the database module
      database <- import("db")
      database$delete_recipe(g_edit_meal_id())
      
      # refresh recipes table
      if(g_all_recipe_rendered()){
        click("load_recipe")
      }
      
      # clear all data
      clear_all_data()
      
      # update recipe table edit here
      output$recipe_table_edit <- renderDT({
        datatable(as.data.frame(ingredients_list()), editable = FALSE, 
                  options = list(pageLength = 5), selection = "single")
      })
    }
    
  })
  
  # Save log
  observeEvent(input$save_log, {
    
    # Convert the list to JSON text
    recipe_data <- toJSON(ingredients_list())
    
    # Save data to the log table db
    # Import the database module
    database <- import("db")
    
    database$save_log(g_user_email(), input$meal_name, input$serving, recipe_data)
    saved_log = database$get_log(g_user_email())
    
    # load it on the activity table
    output$log_table <- renderDT({
      # Define column names
      names <- c("ID", "Meal Name", "Amount", "Details")
      
      # Convert data to data.frame
      data_df <- do.call(rbind, saved_log)
      colnames(data_df) <- names
      
      # Create the datatable
      datatable(data_df, editable = FALSE, options = list(pageLength = 5), selection = "single")
      
      #update global var for use in deletion
      log_data$data_df <- data_df
    })
    
  })
  
  #Delete log
  #first handle button visibility based on row selection
  observe({
    selected_rows <- input$log_table_rows_selected
    
    if (length(selected_rows) > 0) {
          #print("nothidden")
          show("delete_entry")
        } else {
          #print("yeshidden")
          hide("delete_entry")
        }
  })
  #handle button click
  observeEvent(input$delete_entry, {
    selected_row <- input$log_table_rows_selected
    
    if (length(selected_row) == 1) {
      database <- import("db")
      
      #get the ID for selected row
      id_val <- log_data$data_df[selected_row, "ID"]
      
      print("IDVALstart")
      print(id_val[[1]])
      print("IDVAlend")
      
      database$delete_log(id_val[[1]])

      #refresh log table
      logs <- database$get_log(g_user_email())
      
      # load it on the activity table
      output$log_table <- renderDT({
        # Define column names
        names <- c("ID", "Meal Name", "Amount", "Details")
        
        # Convert data to data.frame
        data_df <- do.call(rbind, logs)
        colnames(data_df) <- names
        
        # Create the datatable
        datatable(data_df, editable = FALSE, options = list(pageLength = 5), selection = "single")
        
        #update global var when refreshing
        log_data$data_df <- data_df
      })
      
      
      clear_all_data()
      
    }
    
  
  })

  # Save recipe
  observeEvent(input$save_recipe, {
    result <- showModal(
      modalDialog(
        title = "Enter meal name",
        textInput("p_meal_name", "Meal Name", value = input$meal_name),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_save_recipe", "Submit")
        )
      )
    )
  })
  observeEvent(input$submit_save_recipe, {
    # Handle the input value here
    meal_name <- input$p_meal_name
    print(paste("Save meal name: ", meal_name))
    
    time_delay <- 3000
    if (is.null(g_user_email()) || nchar(g_user_email()) == 0) {
      showModal(modalDialog("Please login before saving meal."))
      delay(time_delay, removeModal())
      return()
    }
    
    if (nchar(meal_name) == 0) {
      showModal(modalDialog("Please enter a meal name."))
      delay(time_delay, removeModal())
      return()
    }
    
    if (length(ingredients_list()) == 0) {
      showModal(modalDialog("Ingredient list is empty. Please add some ingredients."))
      delay(time_delay, removeModal())
      return()
    }
    
    # Convert the list to JSON text
    recipe_data <- toJSON(ingredients_list())
    
    # Import the database module
    database <- import("db")
    print("=====save recipe")
    print(list(g_user_email(), meal_name, input$serving, recipe_data))
    
    # check meal id for create new or update
    if (g_edit_meal_id() > 0) {
      database$update_recipe(g_edit_meal_id(), meal_name, input$serving, recipe_data)
    }
    # create new
    else {
      database$save_recipe(g_user_email(), meal_name, input$serving, recipe_data)
    }
    
    # Show confirmation message
    showModal(modalDialog(paste0("Save Recipes '", meal_name ,"' successfully!"), easyClose = TRUE))
    delay(1000, removeModal())
    
    # refresh recipes table
    if(g_all_recipe_rendered()){
      click("load_recipe")
    }
    
    # # update new meal name if changed
    # updateTextInput(session, "meal_name", value = meal_name)
    clear_all_data()
  })
  
  
  # testing
  # data <- list(
  #   list("1", "aaa", 15, "[{\"quantity\":\"5\",\"units\":\"ml \",\"ingredient_name\":\"Chinese dish, chow mein, chicken\",\"FoodID\":\"5\"}]"),
  #   list("2", "nbbbb", 25, "[{\"quantity\":\"5\",\"units\":\"g \",\"ingredient_name\":\"Fried chicken, mashed potatoes and vegetables\",\"FoodID\":\"8\"}]")
  # )
  observeEvent(input$load_recipe, {
    
    # Import the database module
    database <- import("db")
    
    # Print all recipe of this user
    recipes <- database$get_recipes(g_user_email())
    print("======print all recipes")
    print(recipes)
    g_meal_data(recipes)
    
    output$recipe_table <- renderDT({
      # Define column names
      names <- c("ID", "Meal Name", "Amount", "Details")
      
      # Convert data to data.frame
      data_df <- do.call(rbind, recipes)
      colnames(data_df) <- names
      
      # Create the datatable
      datatable(data_df, editable = FALSE, options = list(pageLength = 5), selection = "single")
    })
    
    g_all_recipe_rendered(TRUE)
    
  })
  
  observeEvent(input$recipe_table_rows_selected, {
    selected_row <- isolate(input$recipe_table_rows_selected)
    if (length(selected_row) > 0) {
      row_data <- g_meal_data()[[selected_row]]
      
      # load this data to the table next to save recipe to leverage the existing feature for editing
      g_edit_meal_id(as.numeric(row_data[[1]]))
      updateTextInput(session, "meal_name", value = row_data[[2]])
      updateNumericInput(session, "serving", value = as.numeric(row_data[[3]]))
      
      if (length(fromJSON(row_data[[4]])) > 0) {
        load_ingredients_data(fromJSON(row_data[[4]]))
      }
      
      # showModal(modalDialog(
      #   title = "Mofify Meal",
      #   disabled(textInput("id_input", label = "ID:", value = row_data[[1]])),
      #   textInput("name_input", label = "Name:", value = row_data[[2]]),
      #   textInput("amount_input", label = "Amount:", value = row_data[[3]]),
      #   hidden(textInput("details_input", label = "Details Json Data:", value = row_data[[4]])),
      #   fluidRow(
      #     box(title = "Details",
      #         solidHeader = T,
      #         width = 12,
      #         collapsible = T,
      #         div(DT::DTOutput("recipe_table_details"), style = "font-size: 70%;"))
      #   ),
      #   footer = tagList(
      #     modalButton("Cancel"),
      #     actionButton("save_meal", "Save")
      #   ),
      #   easyClose = TRUE
      # ))
      # 
      # # Display details data in table
      # output$recipe_table_details <- renderDataTable({
      #   datatable(as.data.frame(fromJSON(row_data[[4]])), editable = FALSE, options = list(pageLength = 5), selection = "single")
      # })
    }
  })
  observeEvent(input$save_meal, {
    id_value <- as.numeric(input$id_input)
    name_value <- input$name_input
    amount_value <- as.numeric(input$amount_input)
    details_value <- input$details_input
    
    print("=========modified meal data")
    print(list(id_value, name_value, amount_value, details_value))
    
    selected_row <- isolate(input$recipe_table_rows_selected)
    if (length(selected_row) > 0) {
      data <- g_meal_data()
      data[[selected_row]][[1]] <- id_value
      data[[selected_row]][[2]] <- name_value
      data[[selected_row]][[3]] <- amount_value
      data[[selected_row]][[4]] <- details_value
      
      # Update the table
      output$recipe_table <- renderDT({
        # Define column names
        names <- c("id", "name", "amount", "details")
        
        # Convert data to data.frame
        data_df <- do.call(rbind, data)
        colnames(data_df) <- names
        
        # Create the datatable
        datatable(data_df, editable = FALSE, options = list(pageLength = 5), selection = "single")
      })
      
      # save to db
      # Import the database module
      database <- import("db")
      
      # Update recipe
      database$update_recipe(id_value, name_value, amount_value, details_value)
      
      
      g_meal_data(data)
    }
    
    # Close the modal dialog
    removeModal()
  })
  
  observeEvent(ingredients_list(), {
    print(length(ingredients_list()))
    if (length(ingredients_list()) > 0) {
      
      id <- "new"
      if (g_edit_meal_id() > 0) {
        id <- g_edit_meal_id()
      }
      
      output$recipe_table_edit_id <- renderText({
        paste0("ID: ", id)
      })
      output$recipe_table_edit_name <- renderText({
        paste0("Name: ", input$meal_name)
      })
      output$recipe_table_edit_serving <- renderText({
        paste0("Serving amounts: ", input$serving)
      })
    }
    else {
      output$recipe_table_edit_id <- renderText({
        ""
      })
      output$recipe_table_edit_name <- renderText({
        ""
      })
      output$recipe_table_edit_serving <- renderText({
        ""
      })
    }
    
    # update recipe table edit here
    output$recipe_table_edit <- renderDT({
      datatable(as.data.frame(ingredients_list()), editable = FALSE, 
                options = list(pageLength = 5), selection = "single")
    })
  })
  
  
  
  ##########LOGIN WITH GOOGLE
  observeEvent(input$g.email, {
    g_user_email(input$g.email)
    g_user_name(input$g.name)
    g_authenticated(TRUE)
  })
  
  # check if user is authenticated
  observe({
    if (g_authenticated()) {
      
      print("email is")
      print(g_user_email)
      
      
      # Import the database module
      database <- import("db")
      
      # Get user data from db using email
      user_info_db <- database$get_user_info(g_user_email())
      # New user
      if (is.null(user_info_db)) {
        print("New user! Save data to db")
        database$save_user_info(user_info$name, g_user_email())
      }
      # Existing user
      else {
        print("Existing user, print all users")
        test <- database$get_all_data()
        dput(test)
      }
    }
    
    
  })
  ########## DOWNLOAD/UPLOAD Ingredients
  # Download Ingredients as json file
  output$download_ingredient_json <- downloadHandler(
    filename = function() {
      paste("my_data", ".json", sep = "")
    },
    content = function(file) {
      jsonlite::write_json(ingredients_list(), file)
    }
  )
  
  # Upload Ingredients as JSON file
  observeEvent(input$upload_ingredient_json, {
    req(input$upload_ingredient_json)
    my_object <- jsonlite::fromJSON(input$upload_ingredient_json$datapath)
    # Do something with the json data
    print(my_object)
    
    # Show confirmation message
    showModal(modalDialog("Loaded Ingredients successfully!", easyClose = TRUE))
    delay(1000, removeModal())
    
    # clear data
    clear_edit_data()
    
    if(g_all_recipe_rendered()){
      click("load_recipe")
    }
    
    load_ingredients_data(my_object)
  })
  
  # make reactive to store ingredients
  ing_df <- shiny::reactiveValues()
  clear_ing_df()
  
  # observe ingredient inputs
  observeEvent(input$food_id, {
    g_food_id(input$food_id)
  })
  
  observeEvent(input$measure_unit, {
    g_measure_unit(input$measure_unit)
  })
  observeEvent(input$quantity, {
    g_quantity(input$quantity)
  })
  
  # step 1 get singular ingredient
  measure_df <- eventReactive(g_food_id(),{
    measure_df <- ca_food_name[ca_food_name$FoodID==g_food_id(), "FoodID"] %>% 
      left_join(ca_conversion_factor) %>% 
      left_join(ca_measure_name) %>% 
      select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
    # measure_df <- rbind(measure_df, c(numeric = 100, units = "g", description = "", ConversionFactorValue = 1, MeasureID = ""))
    measure_df
  })
  # step 2 update the measure unit for singular ingredient
  observe({
    units <- unique(paste(measure_df()$units, measure_df()$description))
    updateSelectInput(session, "measure_unit", "2. Measure Unit", choices = units)
    
  })
  
  
  
  # step 3 update the ingredient dataframe
  observeEvent(input$remove, {
    isolate(ing_df$df <- ing_df$df[-(nrow(ing_df$df)), ])
    isolate(ing_df$measure <-
              ing_df$measure[-nrow(ing_df$measure), ])
    ingredients_list(ing_df$df)
  })
  
  
  observeEvent(input$add, {
    isolate(ing_df$df[nrow(ing_df$df) + 1, ] <- c(
      input$quantity,
      input$measure_unit,
      names(ca_food_choices[ca_food_choices == input$food_id]),
      as.numeric(input$food_id)
      
      ##give users a popup if they dont add an ingredient and restart the script
      # # cat("Value of food_id is:", input$food_id, "yoyo\n")
      # if(input$food_id == '') {
      #   script_path <- parent.frame()$ofile
      #   
      #   print("please select a food")
      #   print(script_path)
      #   system(paste("Rscript", script_path))
      # }
    ))
    
    # get actual working ingredient dataframe for dplyr
    input_measure <- measure_df()
    input_measure <-
      input_measure[paste(measure_df()$units, measure_df()$description) == input$measure_unit,]
    if (nrow(input_measure) > 1) {
      input_measure <-
        input_measure[which(abs(input_measure$numeric - input$quantity) == min(abs(
          input_measure$numeric - input$quantity
        ))), ]
    }
    isolate(ing_df$measure[nrow(ing_df$measure) + 1,] <- input_measure)
    # update choices
    updateNumericInput(session, 'quantity', '3. Quantity', 1)
    updateSelectizeInput(session, 'measure_unit', '2. Measure Unit')
    updateSelectInput(session, 'food_id', '1. Ingredient', choices = ca_food_choices)
    ingredients_list(ing_df$df)
  })
  
  
  # main nutrition data frame
  nutrition_df <- reactive({
    measure_food_df <- ing_df$measure
    ing_quant <- ing_df$df
    measure_food_df$quantity <- ing_quant$quantity
    measure_food_df <- measure_food_df %>%
      left_join(ca_nutrient_amount) %>%
      left_join(ca_nutrient_name) %>%
      # filter(NutrientID %in% select_nutrients) %>%
      mutate(NutrientName = tolower(NutrientName)) %>%
      mutate(NutrientValue = as.numeric(NutrientValue) * as.numeric(ConversionFactorValue) * as.numeric(quantity) / as.numeric(numeric) / input$serving) %>%
      select(NutrientName, NutrientValue, NutrientID, NutrientUnit, ConversionFactorValue, quantity, FoodID) %>% 
      group_by(NutrientName) %>% 
      summarize(Value = round(sum(NutrientValue, na.rm = T),2),
                Unit = first(NutrientUnit),
                NutrientID = first(NutrientID))
    
    measure_food_df
  })
  # display nutrients necessary for label
  nutrient_table <- reactive({
    select_nutrients <- c(208, 204, 606, 605, 601, 307, 205, 291, 269, 203, 814, 401, 301, 303)
    measure_food_df <- nutrition_df() %>% filter(NutrientID %in% select_nutrients)
    measure_food_df <- measure_food_df[order(match(measure_food_df$NutrientID, select_nutrients)),] %>%
      select(NutrientName, Value, Unit)
    measure_food_df
  })
  # df with dv%
  dv_df <- reactive({
    dv_df <- daily_value %>% left_join(nutrition_df())
    # hack for total sat fats and trans fats
    dv_df$Value[2] <- sum(nutrition_df()$Value[nutrition_df()$NutrientID %in% c(605, 606)], na.rm = T)
    dv_df$Unit[2] <- "g"
    dv_df$pct_dv <- round(dv_df$Value / dv_df$DV, 3) * 100
    dv_df
  })
  
  output$macro_plot <- renderPlotly({
    df_macro <- dv_df() %>% filter(Group == "macronutrients")
    plot_macro <- ggplot(df_macro) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      labs(x = "Nutrient", y = "% Daily Value") + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_fill_gradient(low = "green",
                          high = "red", 
                          limits = c(0, 100),
                          na.value = "darkred",
                          name = "% Daily Value") +
      theme(panel.background = element_rect(fill = "mintcream"), 
            legend.position = "none") +
      scale_x_discrete(labels = c("Cholesterol", "Fat", "Fibre", "Sodium",  "Sugars", "Saturated and \n Trans Fats"))
    ggplotly(plot_macro)
  })
  output$mineral_plot <- renderPlotly({
    df_min <- dv_df() %>% filter(Group == "mineral")
    plot_min <- ggplot(df_min) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      labs(x = "Nutrient", y = "% Daily Value")  + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_fill_gradient(low = "red",
                          high = "green",
                          limits = c(0, 100),
                          na.value = "khaki1", 
                          name = "% Daily Value") +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "lightyellow"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggplotly(plot_min)
  })
  output$vitamin_plot <- renderPlotly({
    df_vit <- dv_df() %>% filter(Group == "vitamin")
    req(g_quantity())
    plot_vit <- ggplot(df_vit) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      geom_hline(yintercept = 100) +
      labs(x = "Nutrient", y = "% Daily Value")  + 
      theme_gray() + 
      ylim(0, NA) +
      scale_fill_gradient(low = "red",
                          high = "green",
                          limits = c(0, 100),
                          na.value = "khaki1") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "aliceblue"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggplotly(plot_vit)
  })
  # dt indicator
  output$ing_df <- DT::renderDataTable(ing_df$df[,1:3], 
                                       # colnames = c("Quantity", "Units", "Ingredient"), 
                                       rownames=F, options = list(pageLength = 5))
  output$nutrient_table <- DT::renderDataTable(nutrient_table())
  # value boxes
  output$calories <- renderValueBox({
    valueBox(paste0(nutrition_df()$Value[nutrition_df()$NutrientID == 208], "kcal"), 
             "Calories", icon = icon("fire"), color = "yellow")
  })
  output$over_nutrient <- renderValueBox({
    nutrition_df <- dv_df() %>% 
      # filter(NutrientID %in% c(601, 204, 307, 269, 0)) %>% 
      tidyr::drop_na(pct_dv) %>% filter(pct_dv > 100)
    if(nrow(nutrition_df) > 0){
      valueBox("Over Daily Value", HTML(paste0(nutrition_df$Nutrient, sep="<br>")), icon = icon("exclamation-triangle"), color = "red")
    } else {
      valueBox("All nutrients", "below recommended DV", icon = icon("exclamation-triangle"), color = "green")
    }
  })
  output$rich_nutrient <- renderValueBox({
    nutrition_df <- dv_df() %>% tidyr::drop_na(pct_dv) %>% filter(pct_dv >= 50) %>% filter(pct_dv < 100)
    if(nrow(nutrition_df) > 0){
      valueBox("High levels* of ", HTML(paste0(c(nutrition_df$Nutrient,"*above 50% recommended DV"),  sep="<br>")), icon = icon("exclamation-triangle"), color = "green")
    } else {
      valueBox(HTML("All nutrients"), "below 50% recommended DV", icon = icon("exclamation-triangle"), color = "orange")
    }
    
  })
  output$serving <- renderText(paste("for 1 serving (", input$serving, "servings in recipe)"))
}

# # For release
# # Run the application
# shinyApp(ui = ui, server = server)
# # runApp(shinyApp(ui = ui, server = server), port=7147) # for testing with gg sign in


# For testing gg sign-in on localhost or RStudio
# gg sign-in didn't work with 127.0.0.1, open a browser tab or change it to localhost
# http://localhost:7147/
# shinyApp(ui = ui, server = server)
runApp(shinyApp(ui = ui, server = server), port=7147, launch.browser = TRUE) # for testing with gg sign in
