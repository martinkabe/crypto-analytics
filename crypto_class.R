sapply(c("tidyverse", "rvest", "R6", "dplyr", "prophet", "RSQLS", "bit64"), require, character.only = TRUE)

CleanHtmlData <- R6::R6Class("CleanHtmlData",
                             
  private = list(
    ..path_to_file = as.character(),
    ..file_name = as.character(),
    ..data = data.frame(),
    ..table_name = as.character(),
    ..prophet_crypto_name = as.character()
  ),
  
  public = list(
    initialize = function(path_to_file = NULL,
                          file_name = NULL,
                          tableName) {
      private$..path_to_file = path_to_file
      private$..file_name = file_name
      private$..table_name = tableName
      if (!is.null(path_to_file)) {
        raw_table = readLines(paste0(path_to_file, file_name))
        private$..data = rvest::minimal_html(raw_table) %>%
          html_node("table") %>%
          html_table() %>% mutate(Date = as.Date(Date, format = "%B %d, %Y"))
        private$..data[2:ncol(private$..data)] = apply(private$..data[2:ncol(private$..data)], 2, function(x) {
          as.numeric(gsub("[\\$,]", "", x))
        }) %>% as.data.frame()
        colnames(private$..data) =  str_remove_all(colnames(private$..data), "[\\* ]")
      }
    },
    
    getCryptoName = function() {
      if (is.null(private$..file_name)) {
        cat("\n... all crypto names considered ...\n")
      } else {
        return (tools::file_path_sans_ext(private$..file_name))
      }
    },
    
    getCryptoData = function() {
      if (is.null(private$..path_to_file)) {
        cat("\n... general object ...\n")
      } else {
        return (private$..data)
      }
    },
    
    getCryptoDataFromDB = function(connString, 
                                   dateFrom = NULL, 
                                   dateTo = NULL,
                                   cryptos = NULL) {
      sql_task = paste0("SELECT * FROM ", private$..table_name)
      
      if (!is.null(private$..path_to_file)) {
        crypto_name = paste0("'", self$getCryptoName(), "'")
        crypto_name_task = paste0(" CryptoName = ", crypto_name)
      } else {
        if (!is.null(cryptos)) {
          crypto_name = paste0("'", cryptos, "'", collapse = ", ")
          crypto_name_task = paste0(" CryptoName in (", crypto_name, ")")
        } else {
          crypto_name = "ALL"
          crypto_name_task = NULL
        }
      }
      
      if (is.null(dateFrom) & is.null(dateTo)) {
        # return all data
        cat("\n---------- Returning all the data for ", crypto_name, " ----------\n")
        sql_task = ifelse(is.null(crypto_name_task),
                          sql_task,
                          paste0(sql_task, " WHERE", crypto_name_task))
      } else if (!is.null(dateFrom) & is.null(dateTo)) {
        # return data from until the end
        cat("\n---------- Returning data from ", dateFrom, " till the end for ", crypto_name, " ----------\n")
        sql_task = ifelse(is.null(crypto_name_task),
                          paste0(sql_task, " WHERE Date >= '", dateFrom, "'"),
                          paste0(sql_task, " WHERE Date >= '", dateFrom, "' AND", crypto_name_task))
      } else if (is.null(dateFrom) & !is.null(dateTo)) {
        # return all data until some date
        cat("\n---------- Returning all data until ", dateTo, " for ", crypto_name, " ----------\n")
        sql_task = ifelse(is.null(crypto_name_task),
                          paste0(sql_task, " WHERE Date <= '", dateTo, "'"),
                          paste0(sql_task, " WHERE Date <= '", dateTo, "' AND", crypto_name_task))
      } else {
        # return date range
        cat("\n---------- Returning all data from ", dateFrom, " to ", dateTo, " for ", crypto_name, " ----------\n")
        sql_task = ifelse(is.null(crypto_name_task),
                          paste0(sql_task, " WHERE Date BETWEEN '", dateFrom, "' AND '", dateTo,"'"),
                          paste0(sql_task, " WHERE Date BETWEEN '", dateFrom, "' AND '", dateTo,"' AND", crypto_name_task))
      }
      
      cat("\n", sql_task, "\n")
      sql_task = paste0(sql_task, " ORDER BY Date DESC")
      
      return (pull_data(connectionString = connString, 
                            sqltask = sql_task, 
                            showprogress = FALSE))
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
    
    getProphetData = function(data = NULL, cryptoName = NULL) {
      if (is.null(private$..path_to_file)) {
        private$..prophet_crypto_name = cryptoName
        return (data %>% filter(CryptoName == cryptoName) %>% select(Date, Close) %>% rename(ds=Date, y=Close))
      } else {
        private$..prophet_crypto_name = self$getCryptoName()
        return (private$..data %>% select(Date, Close) %>% rename(ds=Date, y=Close))
      }
    },
    
    drawDyplotProphet = function(data_prophet, periods) {
      cryptoName = private$..prophet_crypto_name
      model <- prophet(data_prophet)
      future <- make_future_dataframe(model, periods = periods)
      
      forecast <- predict(model, future)
      dyplot.prophet(model, forecast, 
                     main=paste0("Time Series for ", 
                                 cryptoName, " [", 
                                 min(data_prophet$ds), "::", 
                                 max(data_prophet$ds), "]"))
    },
    
    updateData = function(connString) {
      if (is.null(private$..path_to_file)) {
        cat("\n... general object, this method cannot be used ...\n")
      } else {
        db_dates <- pull_data(connectionString = connString, 
                              sqltask = paste0("SELECT Date FROM ", private$..table_name, " WHERE CryptoName = '", self$getCryptoName(), "'"), 
                              showprogress = FALSE) %>% .$Date
        
        missing_dates <- self$getCryptoData()[!self$getCryptoData()$Date %in% db_dates,]
        
        if (nrow(missing_dates) > 0) {
          dates_to_update <- paste0(missing_dates$Date, collapse = ", ")
          cat("\nDates to be updated: ", dates_to_update, "\n")
          push_data(connString, missing_dates %>% mutate(CryptoName=self$getCryptoName()), 
                    private$..table_name, 
                    showprogress = TRUE, 
                    append = TRUE)
        } else {
          cat("\nAll up to date!\n")
        }
      }
    }
  )
)
