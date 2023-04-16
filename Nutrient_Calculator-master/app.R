library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(reticulate)
library(jsonlite)
library(shinyjs)

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
  dashboardHeader(title = "Nutrition Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Hometab" , icon = icon("dashboard")),
      menuSubItem("opt. home subtab", tabName = "subhome"),
      menuItem("Activity Page", tabName =  "Activitytab", icon = icon("calendar")),
      menuItem("Settings", tabName = "Settingstab", icon = icon("cog")),
      tags$p("Notice: some info note")
      # OldNote: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling.
    ),
    actionButton("save_recipe", "Save Recipe"),
    actionButton("download_ingredient_json", "Download Ingredients"),
    actionButton("upload_ingredient_json", "Upload Ingredients"),
    useShinyjs()
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Hometab", selectizeInput(
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
      numericInput("serving", "Number of servings contained", min = 0.01, step = 1, value = 1),),
      tabItem(tabName = "subhome", h1("we could split recipe interaction into sub-sections like this")),
      tabItem(tabName = "Activitytab", 
              fluidRow(
                valueBoxOutput("calories"),
                valueBoxOutput("over_nutrient"),
                valueBoxOutput("rich_nutrient")
              ),
              fluidRow(
                box(title = "Ingredients",
                    solidHeader = T,
                    width = 4,
                    collapsible = T,
                    div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
                box(title = "Macronutrients", solidHeader = T,
                    width = 8, collapsible = T,
                    plotlyOutput("macro_plot"))
              ), # row
              fluidRow(
                box(title = "Nutrition Table",
                    solidHeader = T,
                    width = 4, 
                    collapsible = T,
                    collapsed = F,
                    tags$p(textOutput("serving", inline = T)),
                    div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;")),
                box(title = "Minerals", solidHeader = T,
                    width = 8, collapsible = T,
                    plotlyOutput("mineral_plot"))
              ),# row
              fluidRow(
                box(title = "Vitamins", solidHeader=T,
                    width = 12, collapsible = T,
                    plotlyOutput("vitamin_plot"))
              ) # row
      ),
      tabItem(tabName = "Settingstab", h1("welcome to settings"))
      
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
  
  
  
  ########## SAVE RECIPE
  # Define a reactive variable to store the list of ingredients
  ingredients_list <- reactiveVal(list())
  
  
  
  # handle button click
  observeEvent(input$save_recipe, {
    print("====ingredients list")
    print(ingredients_list)
    
    # Convert the list to JSON text
    recipe_data <- toJSON(ingredients_list())
    
    # Print the JSON text
    print("====recipe info")
    print(recipe_data)
    
    # Check if user email exists or not
    if (is.null(g_user_email()) || nchar(g_user_email()) == 0) {
      print("No user email!!!")
    }
    else {
      # Import the database module
      database <- import("db")
      print("=====save recipe")
      print(list(g_user_email(), recipe_data))
      database$save_recipe(g_user_email(), input$serving, recipe_data)
      # Print all recipe of this user
      recipes <- database$get_recipes(g_user_email())
      print("======print all recipes")
      dput(recipes)
    }
    
  })
  
  ##########LOGIN WITH GOOGLE
  g_authenticated(TRUE)
  
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
  observeEvent(input$download_ingredient_json, {
    file_path <- file.choose()
    jsonlite::write_json(ingredients_list(), file_path)
    # Show confirmation message
    showModal(modalDialog("Saved ingredients to JSON file!"))
  })
  # Upload Ingredients as JSON file
  observeEvent(input$upload_ingredient_json, {
    # Open file dialog to select JSON file
    file_path <- file.choose()
    # Read JSON data from file
    json_data <- readLines(file_path)
    # Convert JSON data to R object
    my_object <- jsonlite::fromJSON(json_data)
    # Show confirmation message
    showModal(modalDialog("Loaded Ingredients successfully!", easyClose = TRUE))
    delay(1000, removeModal())
    # Print loaded object
    # print(my_object)
    
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
      ingredients_list(ing_df$df)
    }
  })
  


  # make reactive to store ingredients
  ing_df <- shiny::reactiveValues()
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
    isolate(ing_df$df<-ing_df$df[-(nrow(ing_df$df)),])
    isolate(ing_df$measure <- ing_df$measure[-nrow(ing_df$measure),])
    ingredients_list(ing_df$df)
  })
  
  
  observeEvent(input$add, {
    isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$quantity,
                                                 input$measure_unit, 
                                                 names(ca_food_choices[ca_food_choices == input$food_id]), 
                                                 as.numeric(input$food_id)))

    # get actual working ingredient dataframe for dplyr
    input_measure <- measure_df()
    input_measure <- input_measure[paste(measure_df()$units, measure_df()$description) == input$measure_unit, ]
    if(nrow(input_measure) > 1){
      input_measure <- input_measure[which(abs(input_measure$numeric-input$quantity)==min(abs(input_measure$numeric-input$quantity))),]
    }
    isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
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
        panel.background = element_rect(fill = "lightyellow"))
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

# Run the application 
# shinyApp(ui = ui, server = server)
# Run the app interactively
runApp(shinyApp(ui = ui, server = server), port=7147) # for testing with gg sign in
# 