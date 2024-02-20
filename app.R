#Application that creates the rainfall landing
#Code by: Ricardo Pommer Muñoz
#Updated on 12/26/23

#&&&&&&&&&&&&&&&&&&
#SETUP ----
#&&&&&&&&&&&&&&&&&&

#Required packages
#library(colorspace)
#library(gt)
library(DT)
#library(data.table)
library(shiny)
library(stringr)
library(ggplot2)
#library(auth0)
library(keyring)
library(shinymanager)
library(gtools)
library(shinylogs)
library(dplyr)


#Server side stuff ----
#For auth0
#options(shiny.port = 3838)
#keyring_unlock(keyring = "system", password = "secret")

#UI ----
# Define UI
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
    
#Design the authentication page----
ui <- secure_app(ui, enable_admin = T,
                 # add image on top ?
                 tags_top = 
                   tags$div(
                     tags$h2("Portal de Lluvias Personalizado", style = "font-family: Arial; align:center"),
                     #tags$img(
                     #   src = "https://advertisingweek.com/wp-content/uploads/2022/11/mindset-concept-watering-plants-with-big-brain-growth-mindset-concept.jpg_s1024x1024wisk20czP9Uq_aL7U7zSCYuBhaMlF6s8u3BUquvSO20MFDZxTA-1170x600.jpg", width = 100
                     #)
                   ),
                 # add information on bottom ?
                 tags_bottom = 
                   tags$div(tags$p(
                     "Si tiene preguntas, por favor contactar via WhatsApp al +17328443418 o al", tags$a(
                       href = "mailto:rap2194@columbia.edu?Subject=Portal de Lluvias Personal",
                       target="_top", "administrador"
                     ),
                     style = "font-family: Arial;"
                   )
                   ),
                 # change auth ui background ?
                 # https://developer.mozilla.org/fr/docs/Web/CSS/background
                 # background  = "linear-gradient(rgba(0, 127, 255, 0.5), rgba(144, 238, 144, 0.5)),
                  #     url('https://media.istockphoto.com/id/867412692/vector/health-icon.jpg?s=612x612&w=0&k=20&c=qqxN-fIU8m0xW_U76vnFzTJvSJ79QIN_m8VHvLeDbrs=');background-size = '450px auto';", 
                 # set language ?
                 language = "es")

set_labels(
  language = "es",
  "Por favor ingrese con su número de participante (recibido por WhatsApp)" = "Bienvenido: por favor ingrese aqui"
)

#Server ----
# Define server logic required
server <- function(input, output, session) {
  
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
    # Get the authenticated username
    # if (is.null(auth$user())) {
    #   return(NULL)
    # }
    # logged_in_username <- auth$user()
    # 
    # # Find the user's vereda from the credentials data frame
    # vereda_for_user <- credentials %>%
    #   filter(username == logged_in_username) %>%
    #   .$vereda
    # 
    # # Return the vereda or a default value if not found
    # if (length(vereda_for_user) > 0) {
    #   vereda_for_user
    # } else {
    #   "Unknown"  # default value if vereda is not found
    # }
    
    dplyr::filter(credentials, user == auth$user) %>% 
      pull(vereda)
  })
  
  output$dynamicVeredaTitle <- renderText({
    paste0("Parecido de lluvia con vereda '", user_vereda(), "'")
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
    modified_data <- user_input()
    modified_data[info$row, info$col] <- info$value
    user_input(modified_data)
  })
  
  # Second table (yield_rain_data)
  output$yieldRainTable <- renderDT({
    # Add a blank column to the modified data named "Mi cafe"
    modified_data <- user_input()
    if (!"Mi cafe" %in% colnames(modified_data)) {
      modified_data$`Mi cafe` <- rep(NA, 42)
    }
    
    # Move "Mi cafe" to the second position
    cols_order <- c("Año", "Mi cafe", setdiff(names(modified_data), c("Año", "Mi cafe")))
    modified_data <- modified_data[, cols_order]
    
    datatable(modified_data, editable = list(target = 'cell', disable = list(columns = c(1, 3, 4))),
              options = list(
                dom = 'tp',
                pageLength = 10,
                language = list(paginate = list(`next` = "Siguiente",
                                                previous = "Anterior")),
                columnDefs = list(list(
                  targets = 2, 
                  render = DT::JS("
                                  function(data, type, row, meta) {
                                    if(type === 'display'){
                                      return '<select><option value=\"Seleccione\">Seleccione</option><option value=\"Normal\">Normal</option><option value=\"Malo\">Malo</option></select>';
                                    }
                                    return data;
                                  }
                                  ")
                ))
              )
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
#shinyAppAuth0(ui = ui, server = server)


