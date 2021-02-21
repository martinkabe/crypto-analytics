sapply(c("tidyverse", "rvest", "R6", "dplyr", "prophet", "RSQLS"), require, character.only = TRUE)

CleanHtmlData <- R6::R6Class("CleanHtmlData",
  private = list(
    ..path_to_file = as.character(),
    ..file_name = as.character(),
    ..data = data.frame()
  ),
  public = list(
    initialize = function(path_to_file, file_name) {
      private$..path_to_file = path_to_file
      private$..file_name = file_name
      raw_table = readLines(paste0(path_to_file, file_name))
      private$..data = rvest::minimal_html(raw_table) %>%
        html_node("table") %>%
        html_table() %>% mutate(Date = as.Date(Date, format = "%B %d, %Y"))
      private$..data[2:ncol(private$..data)] = apply(private$..data[2:ncol(private$..data)], 2, function(x) {
        as.numeric(gsub("[\\$,]", "", x))
      }) %>% as.data.frame()
      colnames(private$..data) =  str_remove_all(colnames(private$..data), "[\\* ]")
    },
    
    getCryptoName = function() {
      return (tools::file_path_sans_ext(private$..file_name))
    },
    
    getCryptoData = function() {
      return(private$..data)
    },
    
    getDataInfo = function(data) {
      cat("\n----------------------------------------------------\n")
      cat("\trows: ", nrow(data), "\tcols: ", ncol(data), "\n")
      cat("\n")
      print(head(data, 3))
      cat("\n\t...\t...\t...\n\n")
      print(tail(data, 3))
      cat("\n")
    },
    
    getProphetData = function() {
      prophet_data <- private$..data
      prophet_data = prophet_data %>% select(Date, Close) %>% rename(ds=Date, y=Close)
      return(prophet_data)
    },
    
    drawDyplotProphet = function(periods) {
      data_prophet <- self$getProphetData()
      model <- prophet(data_prophet)
      future <- make_future_dataframe(model, periods = periods)
      
      forecast <- predict(model, future)
      dyplot.prophet(model, forecast, 
                     main=paste0("Time Series for ", 
                                 self$getCryptoName(), " [", 
                                 min(data_prophet$ds), "::", 
                                 max(data_prophet$ds), "]"))
    },
    
    updateData = function(connString) {
      db_dates <- pull_data(connectionString = connString, 
                            sqltask = paste0("select Date from dbo.cryptocurrency where CryptoName = '", 
                                                                    self$getCryptoName(), "'"), 
                            showprogress = FALSE) %>% .$Date
      
      missing_dates <- self$getCryptoData()[!self$getCryptoData()$Date %in% db_dates,]
      
      if (nrow(missing_dates) > 0) {
        dates_to_update <- paste0(missing_dates$Date, collapse = ", ")
        cat("\nDates to be updated: ", dates_to_update, "\n")
        push_data(connString, missing_dates %>% mutate(CryptoName=self$getCryptoName()), 
                  "dbo.cryptocurrency", showprogress = TRUE, append = TRUE)
      } else {
        cat("\nAll up to date!\n")
      }
    }
  )
)
