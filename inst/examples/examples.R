
library(dygraphs)
library(datasets)

# lung deaths
lungDeaths <- cbind(mdeaths, fdeaths)
colnames(lungDeaths) <- c("Male", "Female")

dygraph(lungDeaths) %>%  
  dyAxis("x", label = "Month") %>%
  dyAxis("y", label = "Deaths", drawGrid = FALSE) %>%
  dySeries("Male") %>%
  dySeries("Female") %>% 
  dyRoller(rollPeriod = 10) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

dygraph(discoveries, main = "Important Discoveries", ylab = "Discoveires") %>%
  dyAxis("x", pixelsPerLabel = 60) %>%
  dySeries("V1", label = "Discoveries") %>%
  dyRangeSelector() %>%
  dyOptions(fillGraph = TRUE, strokeWidth = 2, css = "inst/examples//styles.css")

temperature <- ts(
  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
  frequency = 12, start = c(1980, 1))
rainfall <- ts(
  data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4),
  frequency = 12, start = c(1980, 1))
weather <- cbind(rainfall, temperature)

dygraph(weather) %>%
  dyAxis('y2', independentTicks = TRUE) %>%
  dySeries('temperature') %>%
  dySeries('rainfall', axis = 'y2')


library(quantmod)
# getSymbols(c("BCOV", "MSFT"), from = "2012-01-01")
# BCOV$BCOV.Open <- NULL
# BCOV$BCOV.Volume <- NULL
# BCOV$BCOV.Adjusted <- NULL
# MSFT$MSFT.Open <- NULL
# MSFT$MSFT.Volume <- NULL
# MSFT$MSFT.Adjusted <- NULL
# 
# stocks <- cbind(BCOV, MSFT)

dygraph(stocks, main = "Stocks", xlab = "time", ylab = "price") %>% 
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
  dySeries("MSFT.Close", label = "MSFT") %>%
  dySeries(c("BCOV.Low", "BCOV.Close", "BCOV.High"), label = "BCOV")



