# Load Packages -----------------------------------------------------------
sapply(c("ggplot2", "PerformanceAnalytics"), require, character.only = TRUE)
# devtools::install_github("martinkabe/RSQLS", force = TRUE)

# Load Properties ---------------------------------------------------------
source('./crypto_class.R')
path_to_file <- "./data/"
table_name <- "dbo.cryptocurrency"
cs <- set_connString(datasource = Sys.getenv()[["Crypto_DataSource"]], 
                     database = "CryptoDB", 
                     usr = Sys.getenv()[["Crypto_User"]],
                     pwd = Sys.getenv()[["Crypto_Pwd"]])

# RSQLS::get_DB_info(cs)

# Analytics ---------------------------------------------------------------
# https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20150101&end=20210213
ethereum <- CleanHtmlData$new(path_to_file, "ethereum.csv", table_name)
bitcoin <- CleanHtmlData$new(path_to_file, "bitcoin.csv", table_name)
cardano <- CleanHtmlData$new(path_to_file, "cardano.csv", table_name)
polkadot <- CleanHtmlData$new(path_to_file, "polkadot.csv", table_name)

## test for general object
general_obj <- CleanHtmlData$new(file_name = "ethereum.csv", tableName = table_name)
general_obj$getCryptoName()
general_obj$getCryptoData()

crypto_data <- general_obj$getCryptoDataFromDB(connString = cs, 
                                               dateFrom = "2021-01-01", 
                                               cryptos = c("bitcoin", "ethereum"))

general_obj$getDataInfo(crypto_data)
general_obj$drawDyplotProphet(data_prophet = general_obj$getProphetData(data = crypto_data,
                                                                        cryptoName = "bitcoin"), 
                              periods = 365)

general_obj$updateData(connString = cs)

# Update the data in DB ---------------------------------------------------
### update the data
ethereum$updateData(connString = cs)
bitcoin$updateData(connString = cs)
cardano$updateData(connString = cs)
polkadot$updateData(connString = cs)

ethereum$drawDyplotProphet(data_prophet = ethereum$getProphetData(), periods = 365)
bitcoin$drawDyplotProphet(periods = 365)
cardano$drawDyplotProphet(periods = 365)
polkadot$drawDyplotProphet(periods = 365)

### get historical data from db
ethereum_data <- ethereum$getCryptoDataFromDB(connString = cs, dateFrom = "2021-01-01")
ethereum_data <- ethereum$getCryptoDataFromDB(connString = cs, dateTo = "2021-01-01")
ethereum_data <- ethereum$getCryptoDataFromDB(connString = cs, dateFrom = "2020-01-01", dateTo = "2020-12-31")

# Plot some graphs --------------------------------------------------------
ggplot() + 
  geom_line(data = ethereum$getCryptoData(), aes(x = Date, y = log10(Close), colour="red"), size = 1) +
  geom_line(data = bitcoin$getCryptoData(), aes(x = Date, y = log10(Close), colour="blue"), size = 1) +
  geom_line(data = cardano$getCryptoData(), aes(x = Date, y = log10(Close), colour="green"), size = 1) +
  xlab('Date') +
  ylab('log10(ClosePrice)') +
  ggtitle("Ethereum, Bitcoin, Cardano") +
  scale_color_discrete(name = "Cryptocurrency:", labels = c("Ethereum", "Cardano", "Bitcoin"))

onlyClosePrice <- bitcoin$getCryptoData() %>% 
  inner_join(ethereum$getCryptoData(), by="Date") %>%
  inner_join(., cardano$getCryptoData(), by="Date") %>%
  select(Date, starts_with("Close")) %>%
  rename("Bitcoin"="Close.x", "Ethereum"="Close.y", "Cardano"="Close")

## plot correlation chart
chart.Correlation(onlyClosePrice[2:ncol(onlyClosePrice)], histogram=TRUE, pch=19)

ggplot() + 
  geom_line(data = onlyClosePrice, aes(x = Date, y = log10(Bitcoin), colour="red"), size = 1) +
  geom_line(data = onlyClosePrice, aes(x = Date, y = log10(Ethereum), colour="blue"), size = 1) +
  geom_line(data = onlyClosePrice, aes(x = Date, y = log10(Cardano), colour="green"), size = 1) +
  xlab('Date') +
  ylab('log10(ClosePrice)') +
  ggtitle("Ethereum, Bitcoin, Cardano for common Dates") +
  scale_color_discrete(name = "Cryptocurrency:", labels = c("Bitcoin", "Cardano", "Ethereum"))


## plot time series from db
general_obj <- CleanHtmlData$new(tableName = table_name)
all_data <- general_obj$getCryptoDataFromDB(connString = cs)

ggplot(all_data, aes(x = Date, y = log10(Close))) + 
  geom_line(aes(color = CryptoName), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FF0000", "#00FFFF")) +
  theme_minimal()

general_obj$drawDyplotProphet(data_prophet = general_obj$getProphetData(data = all_data, cryptoName = "bitcoin"), periods = 365)
general_obj$drawDyplotProphet(data_prophet = general_obj$getProphetData(data = all_data, cryptoName = "cardano"), periods = 365)
general_obj$drawDyplotProphet(data_prophet = general_obj$getProphetData(data = all_data, cryptoName = "ethereum"), periods = 365)
general_obj$drawDyplotProphet(data_prophet = general_obj$getProphetData(data = all_data, cryptoName = "polkadot"), periods = 365)
