# Load the RSQLite Library
library(RSQLite)
library(ggplot2)

# Load the mtcars as an R data frame put the row names as a column, and print the header.
mtcars$car_names <- rownames(mtcars)
rownames(mtcars) <- c()
head(mtcars)

# Create a connection to our new database, CarsDB.db
# you can check that the .db file has been created on your working directory
conn <- dbConnect(RSQLite::SQLite(), "CarsDB.db")

# Write the mtcars dataset into a table named mtcars_data
dbWriteTable(conn, "cars_data", mtcars)

# List all the tables available in the database
dbListTables(conn)

# Create toy data frames
car <- c('Camaro', 'California', 'Mustang', 'Explorer')
make <- c('Chevrolet','Ferrari','Ford','Ford')
df1 <- data.frame(car,make)
car <- c('Corolla', 'Lancer', 'Sportage', 'XE')
make <- c('Toyota','Mitsubishi','Kia','Jaguar')
df2 <- data.frame(car,make)

# Add them to a list
dfList <- list(df1,df2)

# Write a table by appending the data frames inside the list
for(k in 1:length(dfList)){
  dbWriteTable(conn,"Cars_and_Makes", dfList[[k]], append = TRUE)
}

# List all the Tables
dbListTables(conn)

# Let us now make sure that all the data is in the new table
dbGetQuery(conn, "SELECT * FROM Cars_and_Makes")

# Gather the first 10 rows in the cars_data table
dbGetQuery(conn, "SELECT * FROM cars_data LIMIT 10")

# Get the car names and horsepower of the cars with 8 cylinders
dbGetQuery(conn,"SELECT car_names, hp, cyl FROM cars_data WHERE cyl = 8")

# Close the database connection to CarsDB
dbDisconnect(conn)