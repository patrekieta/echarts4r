e_marginDensity <- function(e, data, x, y, tl, group, show_axis = FALSE, ...){
  
  if(tl){
    
  }
  
  if(!missing(group)){
    dens1 <- data |>
      split(data[, group]) |>
      lapply(function(df) {
        d <- density(df[, x, drop = TRUE])
        data.frame(
          x = d[["x"]],
          y = d[["y"]],
          group = df[,group, drop = TRUE][1]
        )
      })
    dens2 <- data |>
      split(data[, group]) |>
      lapply(function(df) {
        d <- density(df[, y, drop = TRUE])
        data.frame(
          x = d[["y"]],
          y = d[["x"]],
          group = df[,group, drop = TRUE][1]
        )
      })
    
    
  } else {
    d <- density(data[,x,drop = TRUE])
    dens1 <- list(all = data.frame( x = d[["x"]],y = d[["y"]]))
    d <- density(data[,y,drop = TRUE])
    dens2 <- list(all = data.frame( x = d[["y"]],y = d[["x"]]))
  }
  
  chart1 <- e_charts() |> e_hide_grid_lines()
  chart2 <- e_charts() |> e_hide_grid_lines()
  
  
  chart1$x$opts$xAxis <- list(
    list(type = "value")
  )
  chart1$x$opts$yAxis <- list(
    list(type = "value")
  )
  chart2$x$opts$xAxis <- list(
    list(type = "value")
  )
  chart2$x$opts$yAxis <- list(
    list(type = "value")
  )
  
  
  chart1$x$opts$series <- lapply(names(dens1), function(g) {
    d <- dens1[[g]]
    d <- d[order(d[["x"]]), ]
    list(
      name = g,
      type = "line",
      data = lapply(seq_len(nrow(d)), function(i) list(d[["x"]][i], d[["y"]][i])),
      showSymbol = FALSE,
      smooth = TRUE,
      areaStyle = list(opacity = 0.5)
    )
  })
  chart2$x$opts$series <- lapply(names(dens2), function(g) {
    d <- dens2[[g]]
    d <- d[order(d[["y"]]), ]  # sort by y so line draws monotonically along y-axis
    list(
      name = g,
      type = "line",
      data = lapply(seq_len(nrow(d)), function(i) list(d[["x"]][i], d[["y"]][i])),
      showSymbol = FALSE,
      smooth = TRUE,
      areaStyle = list(opacity = 0.5)
    )
  })
  
  if(show_axis){
    chart1 <- chart1 |>
      e_x_axis(position = "top", axisLine = list(onZero = FALSE))
    chart2 <- chart2 |> 
      e_y_axis(position = "right", axisLine = list(onZero = FALSE))
  } else {
    chart1 <- chart1 |>
      e_x_axis(show = FALSE)|>
      e_y_axis(show = FALSE)
    chart2 <- chart2 |>
      e_x_axis(show = FALSE)|>
      e_y_axis(show = FALSE)
  }
  
  return(list(chart1, chart2))
}