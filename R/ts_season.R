#' Seasonality Extraction function
#'
#' Extracts and plot the seasonality component of a time series
#' @param x A list returned by ts_trend().
#' @param plot Whether to plot or not the seasonal component. Defaults to TRUE.
#' @param random Whether to calculate random component or not
#' @seealso decompose().
#' @author David Alberto Mateos Montes de Oca.
#' @keywords ts, timeseries, seasonality.
#' @export
#' @examples
#'  s <- AirPassengers
#'  x <- ts_trend(s)
#'  ts_season(x)


ts_season <- function(x, plot = TRUE, random = TRUE) {
  if(class(x) != "list") stop("List returned by ts_trend function needed")
  if(!"detrended.series" %in% names(x)) stop("Detrended series not found")

  a <- (paste(start(x$trend.series)[1], 1, 1, sep = "/") %>% ymd())+(start(x$trend.series)[2]-1)
  b <- (paste(end(x$trend.series)[1], 1, 1, sep = "/") %>% ymd())+(end(x$trend.series)[2]-1)

  ifelse(sum(start(x$trend.series)) == 2,
         fecha <- 1:length(x$trend.series),
         fecha <- seq.Date(from = a, to = b, length.out = length(x$trend.series)))

  m <- t(matrix(data = x$detrended.series, nrow = x$order))
  seas <- colMeans(m, na.rm = TRUE)
  seas <- rep_len(seas, length.out = length(x$detrended.series))

  if(plot == TRUE & random == FALSE){

    gg_s <- data.frame(fecha = fecha,
               seasonality = seas) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = seasonality))+
      theme_minimal()+
      labs(x = "", y = "")

    return(list(season.series = seas,
                season.plot = gg_s))
  }
  if(plot == TRUE & random == TRUE){

    gg_s <- data.frame(fecha = fecha,
                       seasonality = seas) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = seasonality))+
      theme_minimal()+
      labs(x = "", y = "")

    random <- (x$detrended.series - seas)

    return(list(season.series = seas,
                season.plot = gg_s,
                random.series = random))

  }
  if(plot == FALSE & random == TRUE){

    random <- (x$detrended.series - seas)

    return(list(season.series = seas,
                random.series = random))

  }else{
    return(seas)
  }

}
