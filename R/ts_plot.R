#' Time Series beautiful plots
#'
#' This function creates a plot for a time series object using different engines. Corresponding libraries will be loaded.
#' @param x A time series.
#' @param interactive Set to FALSE for base R and ggplot2 plots and TRUE for plotly and highcharter. Defaults to TRUE.
#' @param engine library to be used to create the plot, can be any of "base" "ggplot", "plotly" and "highcharter". Defaults to "ggplot".
#' @author David Alberto Mateos Montes de Oca.
#' @keywords plot, timeseries, ts.
#' @export
#' @examples
#' x <- AirPassengers
#' ts_plot(x)


ts_plot <- function(x, interactive = F, engine ="ggplot"){

  if(class(x) != "ts") stop("ts class object needed")

  # require(ggplot2, quietly = TRUE)
  # require(plotly, quietly = TRUE)
  # require(highcharter, quietly = TRUE)
  # require(dygraphs, quietly = TRUE)

  a <- paste(start(x)[1], start(x)[2],1, sep = "/") %>% ymd()
  b <- paste(end(x)[1], end(x)[2],1, sep = "/") %>% ymd()

  ifelse(sum(start(x)) == 2,
         fecha <- 1:length(x),
         fecha <- seq.Date(from = a, to = b, length.out = length(x)))

  if(interactive == TRUE & engine %in% c("base", "ggplot")) stop("base R and ggplot2 plots cannot be interactive \n please turn off interactive parameter when using those engines")
  if(interactive == FALSE & engine %in% c("plotly", "highcharter")) stop("plotly and highcharter are interactive plots \n please turn on interactive parameter when using those engines")

  if(engine == "base"){
    plot(x)
  }
  if(engine == "ggplot"){
    gg <- data.frame(fecha = fecha, serie = as.numeric(as.vector(x))) %>%
      ggplot()+
      geom_line(aes(x = fecha, y = serie))+
      geom_smooth(aes(x = fecha, y = serie), se = F)+
      geom_point(aes(x = fecha, y = serie))+
      theme_minimal()+
      labs(x = "", y = "")

    return(gg)
  }
  if(engine == "plotly"){
    ptly <- data.frame(fecha = fecha, serie = as.numeric(as.vector(x))) %>%
      plot_ly(x = ~fecha, y = ~serie, mode = "lines")

    return(ptly)
  }
  if(engine == "highcharter"){
    hc <- data.frame(fecha = fecha, serie = as.numeric(as.vector(x))) %>%
      hchart(type = "line", hcaes(x = fecha, y = serie))

    return(hc)
  }
}
