
# Load Packages -----------------------------------------------------------
sapply(c("ggplot2", "PerformanceAnalytics", "RSQLS"), require, character.only = TRUE)

# Load Properties ---------------------------------------------------------
source('./crypto_class.R')
path_to_file <- "./data/"

# https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20150101&end=20210213

# Analytics ---------------------------------------------------------------
ethereum <- CleanHtmlData$new(path_to_file, "ethereum.csv")
bitcoin <- CleanHtmlData$new(path_to_file, "bitcoin.csv")
cardano <- CleanHtmlData$new(path_to_file, "cardano.csv")
polkadot <- CleanHtmlData$new(path_to_file, "polkadot.csv")


# Update the data in DB ---------------------------------------------------
# devtools::install_github("martinkabe/RSQLS", force = TRUE)
cs <- set_connString(datasource = Sys.getenv()[["Crypto_DataSource"]], 
                     database = "CryptoDB", 
                     usr = Sys.getenv()[["Crypto_User"]],
                     pwd = Sys.getenv()[["Crypto_Pwd"]])
# RSQLS::get_DB_info(cs)

### update the data
ethereum$updateData(connString = cs)
bitcoin$updateData(connString = cs)
cardano$updateData(connString = cs)
polkadot$updateData(connString = cs)

ethereum$drawDyplotProphet(periods = 365)
bitcoin$drawDyplotProphet(periods = 365)
cardano$drawDyplotProphet(periods = 365)
polkadot$drawDyplotProphet(periods = 365)

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


chart.Correlation(onlyClosePrice[2:ncol(onlyClosePrice)], histogram=TRUE, pch=19)

ggplot() + 
  geom_line(data = onlyClosePrice, aes(x = Date, y = log10(Bitcoin), colour="red"), size = 1) +
  geom_line(data = onlyClosePrice, aes(x = Date, y = log10(Ethereum), colour="blue"), size = 1) +
  geom_line(data = onlyClosePrice, aes(x = Date, y = log10(Cardano), colour="green"), size = 1) +
  xlab('Date') +
  ylab('log10(ClosePrice)') +
  ggtitle("Ethereum, Bitcoin, Cardano for common Dates") +
  scale_color_discrete(name = "Cryptocurrency:", labels = c("Bitcoin", "Cardano", "Ethereum"))

