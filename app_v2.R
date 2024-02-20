#Application that creates the rainfall landing
#Code by: Ricardo Pommer Muñoz, based on 
# Tushar Kundu's teacher tool

#Updated on 12/26/23

#&&&&&&&&&&&&&&&&&&
#SETUP ----
#&&&&&&&&&&&&&&&&&&

#Required packages

library(DT)
library(shiny)
library(stringr)
library(ggplot2)
library(keyring)
library(shinymanager)
library(gtools)
library(shinylogs)
library(dplyr)
library(DBI)
library(RPostgreSQL)

# Define your database connection parameters
db_params <- list(
  dbname = "cafe_yield",
  host = "localhost",  # Use "localhost" if the database is on the same machine as the Shiny app
  port = 5432,         # Default PostgreSQL port
  user = "admin",
  password = "coolpwd"
)


# UI definitions
ui <- fluidPage(
  tagList(includeCSS('www/style.css')),
  
  #Data page
  titlePanel("Portal de Lluvias Personalizado"),
  mainPanel(
    width = 10,
    tabsetPanel(
      tabPanel("Datos", 
               fluidPage(
                 h3(textOutput("dynamicVeredaTitle")), # Dynamic Title for Vereda
                 ### FOR DEBUG ONLY
                 h4(textOutput("connectionStatus")),
                 h4(textOutput("currentUserId")),
                 h4(verbatimTextOutput("dbFeedback")),
                 DTOutput("correlationTable"),
                 actionButton("show_plot", "Ver lluvia histórica"),
                 actionButton("insurance_sim", "Simular Seguro"),
                 h3("Lluvia y Pérdidas"), # Title for the second table
                 DTOutput("yieldRainTable"), # Second table output
                 plotOutput("scatterPlot")
               )
      ),
      tabPanel("Instrucciones"))
  )
  
)

# authentication page design
ui <- secure_app(ui, enable_admin = T,
                 tags_top = 
                   tags$div(
                     tags$h2("Portal de Lluvias Personalizado", style = "font-family: Helvetica; align:center"),
                   ),
                 tags_bottom = 
                   tags$div(tags$p(
                     "Si tiene preguntas, por favor contactar via WhatsApp al +17328443418 o al", tags$a(
                       href = "mailto:rap2194@columbia.edu?Subject=Portal de Lluvias Personal",
                       target="_top", "administrador"
                     ),
                     style = "font-family: Helvetica;"
                   )
                   ),
                 language = "es")

set_labels(
  language = "es",
  "Por favor ingrese con su número de participante (recibido por WhatsApp)" = "Bienvenido: por favor ingrese aqui"
)

#Server ----
# Define server logic required
server <- function(input, output, session) {
  
  # Connect to the database at the start of the session
  db_con <- dbConnect(RPostgreSQL::PostgreSQL(), 
                      dbname = db_params$dbname,
                      host = db_params$host, 
                      port = db_params$port,
                      user = db_params$user, 
                      password = db_params$password)
  ### DEBUGGING
  #create feedback msg
  feedbackMessage <- reactiveVal("")
  
  # Database test query inside tryCatch to handle potential errors
  tryCatch({
    # Test the connection by executing a simple query
    test_query <- dbGetQuery(db_con, "SELECT 1;")
    output$connectionStatus <- renderText("Database connection successful.")
    
  }, error = function(e) {
    # Error handling
    output$connectionStatus <- renderText(paste("Database connection failed:", e$message))
  })
  
  # Make sure to close the database connection when the session ends
  onSessionEnded(function() {
    dbDisconnect(db_con)
  })
  
  
  # Render the current user's ID for testing
  output$currentUserId <- renderText({
    # Retrieve the current user's ID using auth$user
    current_user_id <- auth$user
    # Return the user ID as a text output
    paste("Current User ID:", current_user_id)
  })
  
  # *Track usage----
  #Check the credentials of the user
  track_usage(
    storage_mode = store_json(path = "logs/")
  )
  
  # *Authentication module----
  # Load hacky credentials
  credentials <- readRDS("credentials.rds")
  
  #Check the credentials of the user
  auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Reactive expression to get the user's vereda based on their authentication
  user_vereda <- reactive({
    # NO ENCRYPTION FOR NOW
    
    dplyr::filter(credentials, user == auth$user) %>% 
      pull(vereda)
  })
  
  output$dynamicVeredaTitle <- renderText({
    paste0("Parecidos de lluvia con vereda '", user_vereda(), "'")
  })
  
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })
  
  # historic yield-rain data
  yield_rain_data <- read.csv("yield_rain_javier.csv") %>% 
    arrange(desc(Año))
  
  
  # Initialize a reactive value to store user input for each row
  user_input <- reactiveVal(yield_rain_data)
  
  # Observe any edits and update the reactive value
  observeEvent(input$yieldRainTable_cell_edit, {
    info <- input$yieldRainTable_cell_edit
    
    # Use showNotification for debugging
    shiny::showNotification(paste("Edited cell at row", info$row, "and column", info$col))
    
    modified_data <- user_input()
    modified_data[info$row, info$col] <- info$value
    user_input(modified_data)
    
    # TEST TO PRINT
    # Check if the "Mi cafe" column was edited and if the value has changed
    if (colnames(modified_data)[info$col] == "Mi cafe" && old_value != info$value) {
      # Update the feedbackMessage to indicate the change
      feedbackMessage("changed")
      shiny::showNotification("Mi cafe changed")
    }
    
    # WRITE THE CHANGES INTO DB
    user_id <- auth$user
    
    # Construct the SQL query to update the database
    query <- sprintf(
      "INSERT INTO mi_cafe_data (user_id, year, mi_cafe) VALUES (%s, %d, '%s') ON CONFLICT (user_id, year) DO UPDATE SET mi_cafe = EXCLUDED.mi_cafe",
      user_id, 
      modified_data[info$row, 'Año'], 
      info$value
    )
    
    # Instead of using print, update the reactive value:
    result <- tryCatch({
      dbExecute(db, query)
    }, error = function(e) {
      feedbackMessage(paste("Error executing query:", e$message))
      return(NULL)  # Return NULL on error
    })
    
    if (!is.null(result)) {
      if (result == 0) {
        feedbackMessage("No rows were inserted or updated.")
      } else {
        feedbackMessage(paste(result, "rows were inserted or updated successfully."))
      }
    }
  })
  
  
  # Second table (yield_rain_data)
  output$yieldRainTable <- renderDT({
    # Move "Mi cafe" to the second position
    cols_order <- c("Año", "Mi cafe", setdiff(names(modified_data), c("Año", "Mi cafe")))
    modified_data <- modified_data[, cols_order]
    
    # Render the DataTable without nested renderDT
    datatable(modified_data, selection = 'single', editable = TRUE, options = list(pageLength = 10))
  }, 
  server = FALSE)
  }

    
    observeEvent(input$edit, {
      showModal(modalDialog(
        title = "Edit Mi Cafe",
        selectInput("miCafeValue", "Mi cafe", choices = c("Seleccione", "Normal", "Malo"), selected = yield_rain_data[input$myTable_selected[1], "Mi cafe"]),
        actionButton("save", "Save"),
        footer = NULL
      ))
    })
    

    observeEvent(input$save, {
      removeModal()
      newData <- yield_rain_data
      if (length(input$yieldRainTable) == 1) {
        newData[input$yieldRainTable_selected[1], "Mi cafe"] <- input$miCafeValue
        myData(newData)
      }
    })

# Run the application 
shinyApp(ui = ui, server = server)
#shinyAppAuth0(ui = ui, server = server)


