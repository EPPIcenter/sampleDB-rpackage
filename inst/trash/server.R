library(shiny)
library(sampleDB)

#source("~/eppicenter/library/R/tests/sqlite_sampleDB_test.R")
function(input, output, session) {
    output$hellp_world <- renderPrint({
        
        sampleDB::CheckTable("location")
        
        # conn <- dbConnect(RSQLite::SQLite(), "~/eppicenter/library/R/files/example_19-Oct-21.sample_db.sqlite")
        # tables.list <- list()
        # for (table in dbListTables(conn)){
        #     tables.list[[table]] <- dbGetQuery(conn, paste("SELECT * FROM", table)) %>% tibble()
        # }
        # dbDisconnect(conn)
        # print(tables.list)
    })
    output$plot <- renderPlot({
        plot(cars, type=input$plotType)
    })
    
    output$summary <- renderPrint({
        summary(cars)
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(cars)
    })
}
