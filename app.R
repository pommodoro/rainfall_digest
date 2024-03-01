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
                 # ### FOR DEBUG ONLY
                 # h4(textOutput("connectionStatus")),
                 # h4(textOutput("currentUserId")),
                 # h4(verbatimTextOutput("dbFeedback")),
                 # ###
                 DTOutput("correlationTable"),
                 actionButton("show_plot", "Ver lluvia histórica"),
                 actionButton("insurance_sim", "Simular Seguro"),
                 h3("Lluvia y Pérdidas"), # Title for the second table
                 DTOutput("yieldRainTable"), # Second table output
                 uiOutput('editUI'),
                 plotOutput("scatterPlot")
               )
      ),
      tabPanel("Instrucciones",
               fluidPage(
                 # Embed a YouTube video
                 tags$h3("Instalación Pluviómetro"),
                 tags$iframe(width = "560", height = "315",
                             src = "https://www.youtube.com/embed/Nbtqqh6iKWA",  # Replace VIDEO_ID with your actual YouTube video ID
                             frameborder = "0",
                             allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                             allowfullscreen = NA),
                 tags$h3("Instructivo Plataforma"),
                 tags$iframe(width = "560", height = "315",
                             src = "https://www.youtube.com/embed/TmOTJMtfdgw",  # Replace VIDEO_ID with your actual YouTube video ID
                             frameborder = "0",
                             allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                             allowfullscreen = NA),
                 
               )))
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
  
  # Render the correlation table
  output$correlationTable <- renderDT({
    #
   
    # Example of reading data inside renderDT
    data_cor <- read.csv("vereda_target_cor.csv") %>% 
      filter(vereda == auth$vereda,
             vereda_code == 19130012) %>% 
      select(tecnicafe, santiago, bogota) %>% 
      pivot_longer(
        cols = everything(),
        names_to = "Ubicación",
        values_to = "Puntaje"
      ) %>% 
      mutate(Puntaje = round(((Puntaje + 1) / 2) * 10, digits = 1))
      
    # Potentially filter or manipulate data based on reactive values here
    datatable(data_cor)
  })
  
  # Connect to the database at the start of the session
  db_con <- dbConnect(RPostgreSQL::PostgreSQL(), 
                      dbname = db_params$dbname,
                      host = db_params$host, 
                      port = db_params$port,
                      user = db_params$user, 
                      password = db_params$password)
  
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
  
  # initialize user_input to a reactive value
  user_input <- reactiveVal()
    
  
  # LOAD UP DATABASE BY CHECKING USERNAME
  observe({
    # Ensure there's a valid user ID before attempting to fetch data
    if (!is.null(auth$user)) {
      user_id <- as.integer(auth$user)
      
      db_con <- dbConnect(RPostgreSQL::PostgreSQL(), dbname = db_params$dbname, host = db_params$host,
                          port = db_params$port, user = db_params$user, password = db_params$password)
      
      query <- sprintf("SELECT * FROM mi_cafe_data WHERE user_id = %d ORDER BY year DESC", user_id)
      db_data <- dbGetQuery(db_con, query)
      dbDisconnect(db_con)  # Always disconnect after finishing the query
      
      # Remove the 'user_id' column and update reactive value
      db_data <- db_data[, !names(db_data) %in% c("user_id"), drop = FALSE]
      
      colnames(db_data) <- c("Año", "Mi Café", "Café en el Cauca", "Tecnicafé/Cajibío", "Bogotá", "Santiago, Chile")
      
      user_input(db_data)  # Update the reactive variable
    }
  })
  # Second table (yield_rain_data)
  output$yieldRainTable <- renderDT({
    # Render the DataTable
    datatable(user_input(),
              selection = 'single',  # Enable single row selection
              options = list(
                dom = 'tp',  # Define the table control elements to appear on the page (t for table, p for pagination)
                pageLength = 10,  # Set number of rows per page
                language = list(
                  paginate = list(
                    `next` = "Siguiente",
                    previous = "Anterior"
                  )
                )
              )
    )
  })
  
  # editing rows---
  # row editing logic
  output$editUI <- renderUI({
    # Check if a row has been selected
    inputId <- input$yieldRainTable_rows_selected
    if (length(inputId) == 1) {  # Make sure one and only one row is selected
      selectedRow <- user_input()[inputId, ]
      fluidRow(
        column(4,
               radioButtons("mi_cafe_input", "Estado de 'Mi café':",
                            choices = c("Normal", "Malo", "No se/No Aplica"),
                            selected = selectedRow$`Mi cafe`
               )
        ),
        column(2,
               actionButton("updateBtn", "Actualizar", class = "btn-primary")
        )
      )
    } else {
      return(NULL)  # Return nothing if no row is selected or multiple rows are selected
    }
  })
  
 
  observeEvent(input$updateBtn, {
    
    current_user_id <- as.integer(auth$user)  # Or however you're storing/accessing the current user's ID
    
    # Extract the current data and the selected row
    newData <- user_input()
    selectedRow <- input$yieldRainTable_rows_selected
    selectedData <- newData[selectedRow, ]
    
    selectedYear <- as.integer(selectedData[1])
    selectedMiCafe <- input$mi_cafe_input
    selectedRowIndex <- input$yieldRainTable_rows_selecte
    
    
    # Construct your SQL UPDATE statement
    # Note: Ensure that your 'year' and 'user_id' columns are correctly named as per your database schema
    sqlUpdate <- sprintf(
      "UPDATE mi_cafe_data SET mi_cafe = '%s' WHERE user_id = '%d' AND year = %d",
      selectedMiCafe,  # The new value from the radio buttons
      current_user_id,          # The user ID from your authentication system
      selectedYear         # The year from the selected row in your data table
    )

    # Execute the SQL UPDATE statement
    db_con <- dbConnect(RPostgreSQL::PostgreSQL(), dbname = db_params$dbname, host = db_params$host,
                        port = db_params$port, user = db_params$user, password = db_params$password)
    tryCatch({
      dbExecute(db_con, sqlUpdate)
      
      # Immediately fetch the updated data for the updated record
      sqlFetch <- sprintf(
        "SELECT * FROM mi_cafe_data WHERE user_id = %d AND year = %d",
        current_user_id,
        selectedYear
      )
      
      # perform the query
      updatedRow <- dbGetQuery(db_con, sqlFetch)
  
      # store everything except the user_id
      rowToWrite <- updatedRow[2:7]
      
      #Update the reactive data source
      if(nrow(updatedRow) == 1) {
        # Get the current data from the reactive value
        currentData <- user_input()

        # Find the index of the row that was updated
        selectedRowIndex <- input$yieldRainTable_rows_selected
        
        # Update only the specific row in the reactive dataset, excluding the user_id
        currentData[selectedRowIndex, ] <- rowToWrite

        # Update the reactive value with the modified data
        user_input(currentData)
        
        #Display a success message
        showNotification("Data updated successfully", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Failed to update data:", e$message), type = "error")
    })
    
    # Don't forget to disconnect from the database
    dbDisconnect(db_con)
  })
}




# Run the application 
shinyApp(ui = ui, server = server)
#shinyAppAuth0(ui = ui, server = server)


