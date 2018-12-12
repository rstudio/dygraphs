#' dyUpdate allows in-situ replacement of the rendered timeseries in dygraph within shiny, rather than
#' rerendering the widget on each data change.  This can be used for "real-time" update changes in
#' sampling, etc.
#'  
#' @param session shiny session
#' @param id graph identifier (refers to the dygraph elementId parameter)
#' @param data the latest data set to be rendered (must have the same number of columns as the original data set)
#'   
#' @note
#' See the \href{https://rstudio.github.io/dygraphs/}{online documentation} for
#' additional details and examples.
#' 
#' @examples 
#' require(shiny)
#' require(dygraphs)
#'
#' app = function () {
#'    newdata <- function(n = 1000) {
#'       vclose <- cumsum(rnorm(n,sd=0.25))
#'       vlow <- vclose - abs(rnorm(n,sd=0.25))
#'       vhigh <- vclose + abs(rnorm(n,sd=0.25))
#'       vopen <- c(vlow[1], vclose[1:(NROW(vclose)-1)])
#'       times <- as.POSIXct((1:n)*5, origin='2018-1-1 00:00:00', tz='UTC')
#'       data.frame(open=vopen, high=vhigh, low=vlow, close=vclose, row.names = times)
#'    }
#'    graph = function() {
#'       bars <- newdata()
#'       v1 <- dygraph(bars, height=650, width='100%', elementId='graph1') %>% 
#'          dyCandlestick() %>% 
#'          dyOptions(labelsUTC = TRUE)
#'       htmltools::browsable(v1)
#'    }
#'
#'    ui <- fluidPage(
#'       sidebarLayout(sidebarPanel(actionButton("button", "regenerate")),
#'       mainPanel(graph())))
#'        
#'    events <- function (input, output, session) {
#'       observeEvent(input$button, {
#'          bars <- newdata()
#'          dyUpdate (session, 'graph1', bars)
#'       })
#'    }
#'
#'    shinyApp(ui = ui, server = events, options=list(port=5432, host="127.0.0.1"))
#' }
#'
#' app()
#' 
#' @export
dyUpdate <- function(session, id, data) {
    if (!xts::is.xts(data))
        data <- xts::as.xts(data)

    times <- as.POSIXct(time(data), origin='2010-1-1 00:00:00', tz='UTC')
    data <- zoo::coredata(data)

    # create matrix of time and data columns (time in epoch time seconds)
    mat <- cbind(as.numeric(times), data)

    # send data and target graph ID to browser
    session$sendCustomMessage(type='dygraph:newdata', list(id=id, data=mat))
}
