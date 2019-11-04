#' Trend Extraction by Moving Averages
#'
#' Extracts and plots the trend for a time series.
#' @param x A time series
#' @param order Order of moving average smoother. Defaults to 5.
#' @param plot Whether to plot the trend or not. Defaults to TRUE.
#' @param detrend Whether to remove the calculated trend from the time series or not. TRUE needed to use with ts_season(). Defaults to TRUE.
#' @param detrendPlot Whether to plot the detrended series. Defaults to FALSE.
#' @details The moving average smoother averages the nearest order periods of each observation.
#' @seealso ma()
#' @return If either plot or detrendPlot a list containing the series and the plots, otherwise a list containing the trend and the order of the moving average.
#' @author David Alberto Mateos Montes de Oca
#' @keywords ts, timeseries, trend
#' @export
#' @examples
#'  x <- AirPassengers
#'  ts_trend(x)


ts_trend <- function(x, order = 5, plot = T, detrend = T, detrendPlot = F, type = "additive"){

  if(class(x) != "ts") stop("ts class object needed")
  if(detrend == F & detrendPlot == T) stop("Set detrend to TRUE in order to plot it")

  ts_t <- ma(x, order = order, centre = T)

  a <- paste(start(x)[1], start(x)[2],1, sep = "/") %>% ymd()
  b <- paste(end(x)[1], end(x)[2],1, sep = "/") %>% ymd()

  ifelse(sum(start(x)) == 2,
         fecha <- 1:length(x),
         fecha <- seq.Date(from = a, to = b, length.out = length(x)))

  if(detrend == T){
    ifelse(type == "additive", ts_d <- (x-ts_t), ts_d <- (x/ts_t))
  }

  if(plot == T & detrendPlot == F & detrend == F){
    gg_t <- data.frame(fecha = fecha,
                     serie = as.numeric(as.vector(x)),
                     tendencia = ts_t) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = serie))+
      geom_point(aes(x = fecha, y = serie))+
      geom_line(aes(x = fecha, y = ts_t))+
      theme_minimal()+
      labs(x = "", y = "")
    return(list(trend.series = ts_t,
                trend.plot = gg_t,
                order.f = order))
  }
  if(plot == T & detrendPlot == T){
    gg_t <- data.frame(fecha = fecha,
                       serie = as.numeric(as.vector(x)),
                       tendencia = ts_t) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = serie))+
      geom_point(aes(x = fecha, y = serie))+
      geom_line(aes(x = fecha, y = ts_t))+
      theme_minimal()+
      labs(x = "", y = "")

    gg_d <- data.frame(fecha = fecha,
                       serie_d = ts_d) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = serie_d))+
      geom_point(aes(x = fecha, y = serie_d))+
      theme_minimal()+
      labs(x = "", y = "")

    return(list(trend.series = ts_t,
                trend.plot = gg_t,
                detrended.series = ts_d,
                detrended.plot = gg_d,
                order.f = order))
  }
  if(plot == T & detrendPlot == F & detrend == T){
    gg_t <- data.frame(fecha = fecha,
                       serie = as.numeric(as.vector(x)),
                       tendencia = ts_t) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = serie))+
      geom_point(aes(x = fecha, y = serie))+
      geom_line(aes(x = fecha, y = ts_t))+
      theme_minimal()+
      labs(x = "", y = "")
    return(list(trend.series = ts_t,
                trend.plot = gg_t,
                detrended.series = ts_d,
                order.f = order))
  }
  else{
    return(list(trend.series = ts_t,
                order.f = order))
  }
}
