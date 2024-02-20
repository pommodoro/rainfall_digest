### creating postgresql

library(RPostgreSQL)

# Connect to your PostgreSQL database
db_con <- dbConnect(RPostgreSQL::PostgreSQL(), 
                    dbname = "cafe_yield", 
                    host = "localhost", 
                    port = 5432,
                    user = "admin", 
                    password = "coolpwd")


# Read your CSV data
common_data <- readxl::read_xlsx("data/IRI/yield_rain_javier.xlsx", sheet = 1)
colnames(common_data) <- c("year", "cauca", "tecnicafe", "bogota", "santiago")

# Define the range of years
years <- 1981:2022

# Assume you have a list of user IDs. If not, you'll need to create this list based on your application's users.
user_ids <- c(1111, 2222, 3333)  # Replace with actual user IDs

# SQL template for inserting data
sql_template <- "INSERT INTO mi_cafe_data (user_id, year, mi_cafe, cauca, tecnicafe, bogota, santiago) VALUES ($1, $2, 'Seleccione', $3, $4, $5, $6)"

# Loop over each user ID and year, and insert the data into the database
for (user_id in user_ids) {
  for (year in years) {
    # Extract the row from common_data corresponding to the year
    row <- common_data[common_data$year == year, ]
    
    # Check if the row is not empty before attempting to insert
    if (nrow(row) == 1) {
      # Use dbExecute for INSERT statements
      dbExecute(db_con, sql_template, list(user_id, year, row$cauca, row$tecnicafe, row$bogota, row$santiago))
    } else {
      message("Data for year ", year, " not found.")
    }
  }
}

# Close the database connection when done
dbDisconnect(db_con)
