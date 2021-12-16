library(httr)
library(jsonlite)
library(tidyverse)

# address of the server that the request needs to be sent to.
url <- "http://api.open-notify.org/astros.json"
response <- GET(url)

data <- rawToChar(response$content) %>% fromJSON()
names(data)

