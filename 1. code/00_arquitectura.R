## Gonzalo Oyanedel Vial
## June 2021

library(DBI)
library(RSQLite)
library(openxlsx)

futbol <- dbConnect(RSQLite::SQLite(), dbname = "futbol_not_soccer/database.sqlite")

tables <- dbListTables(futbol); tables

# res <- dbSendQuery(futbol, "SELECT * from Match")
# data <- fetch(res, n = 15)

for (i in 1:length(tables)){
    
    table <- tables[i]

    data <- fetch(dbSendQuery(futbol,paste0("SELECT * from ", table)))

    write.xlsx(data, paste0(table, ".xlsx"))

}

