

e_marginDensity <- function(data, x, y, tl, group, show_axis = FALSE, ...){
  
  # Calculate densities for tl
  if(!missing(tl)) {
    dens1 <- data |>
      split(data[, tl]) |>
      lapply(function(df) {
        d <- density(df[, x, drop = TRUE])
        data.frame(
          x = d[["x"]],
          y = d[["y"]]
        )
      })
    dens2 <- data |>
      split(data[, tl]) |>
      lapply(function(df) {
        d <- density(df[, y, drop = TRUE])
        data.frame(
          x = d[["y"]],
          y = d[["x"]]
        )
      })
  } else {
    d <- density(data[,x,drop = TRUE])
    dens1 <- list(all = data.frame( x = d[["x"]],y = d[["y"]]))
    d <- density(data[,y,drop = TRUE])
    dens2 <- list(all = data.frame( x = d[["y"]],y = d[["x"]]))
  }
  
  # Calculate Densities for grouped data
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
  
  # Generate Base chart
  if(!missing(tl)){
    
    chart1 <- data |> group_by(.data[[tl]]) |> 
      e_charts(timeline = TRUE) |> 
      e_hide_grid_lines() |>
      e_x_axis(type = "value") |>
      e_y_axis(type = "value")
    
    chart2 <- chart1
    
  } else {
    
    chart1 <- e_charts() |> 
      e_x_axis(type = "value") |>
      e_y_axis(type = "value") 
    
    chart2 <- chart1
  }
  
  
  
  
  # Generate series data 
  if(!missing(tl)){
    
    for(i in 1:length(chart1$x$opts$options)){
      g <- names(dens1)[i]
      d <- dens1[[g]]
      d <- d[order(d[["x"]]), ]
      chart1$x$opts$options[[i]]$series <- list(
        name = g,
        type = "line",
        data = lapply(seq_len(nrow(d)), function(j) list(d[["x"]][j], d[["y"]][j])),
        showSymbol = FALSE,
        smooth = TRUE,
        clip = TRUE,
        areaStyle = list(opacity = 0.5)
      )
      
      
      d <- dens2[[g]]
      d <- d[order(d[["y"]]), ]
      
      # remove rendering artifact in areastyle for flipped line graph 
      x_offset <- -max(d[["x"]]) * 0.01
      d <- rbind(
        data.frame(x = x_offset, y = d[["y"]][1]),
        d,
        data.frame(x = x_offset, y = d[["y"]][nrow(d)])
      )
      chart2$x$opts$options[[i]]$series <- list(
          name = g,
          type = "line",
          data = lapply(seq_len(nrow(d)), function(j) list(d[["x"]][j], d[["y"]][j])),
          showSymbol = FALSE,
          smooth = TRUE,
          clip = TRUE,
          areaStyle = list(opacity = 0.5)
        )
          
    }
    
    
    
    
  } else {
    
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
      d <- d[order(d[["y"]]), ]
      list(
        name = g,
        type = "line",
        data = lapply(seq_len(nrow(d)), function(i) list(d[["x"]][i], d[["y"]][i])),
        showSymbol = FALSE,
        smooth = TRUE,
        areaStyle = list(opacity = 0.5)
      )
    })
    
  }  
  
  # Remove axes if needed 
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