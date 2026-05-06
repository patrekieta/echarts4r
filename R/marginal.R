

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
    
    chart1$x$opts$baseOption$timeline$show = FALSE
    
  } else {
    
    chart1 <- e_charts() |> 
      e_hide_grid_lines() |>
      e_x_axis(type = "value") |>
      e_y_axis(type = "value")
      
  }
  
  chart2 <- chart1
  
  
  
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
      
      # remove rendering artifact in areastyle for flipped line graph 
      x_offset <- -max(d[["x"]]) * 0.01
      d <- rbind(
        data.frame(x = x_offset, y = d[["y"]][1], group = d[["group"]][1]),
        d,
        data.frame(x = x_offset, y = d[["y"]][nrow(d)], group = d[["group"]][1])
      )
      
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
      e_x_axis(position = "top", axisLine = list(onZero = FALSE)) |>
      e_y_axis(min = 0)
    chart2 <- chart2 |> 
      e_x_axis(min = 0) |>
      e_y_axis(position = "right", axisLine = list(onZero = FALSE))
  } else {
    chart1 <- chart1 |>
      e_x_axis(show = FALSE)|>
      e_y_axis(show = FALSE, min = 0)
    chart2 <- chart2 |>
      e_x_axis(show = FALSE, min = 0)|>
      e_y_axis(show = FALSE)
  }
  
  return(list(chart1, chart2))
}






e_marginHistogram <- function(data, x, y, tl, group, show_axis = FALSE, ...){
  
  # Calculate histograms for tl
  if(!missing(tl)) {
    hist1 <- data |>
      split(data[, tl]) |>
      lapply(function(df) {
        h <- hist(df[, x, drop = TRUE], plot = FALSE)
        list(
          mids = h$mids,
          counts = h$counts,
          breaks = h$breaks
        )
      })
    hist2 <- data |>
      split(data[, tl]) |>
      lapply(function(df) {
        h <- hist(df[, y, drop = TRUE], plot = FALSE)
        list(
          mids = h$mids,
          counts = h$counts,
          breaks = h$breaks
        )
      })
  } else {
    h <- hist(data[, x, drop = TRUE], plot = FALSE)
    hist1 <- list(all = list(mids = h$mids, counts = h$counts, breaks = h$breaks))
    h <- hist(data[, y, drop = TRUE], plot = FALSE)
    hist2 <- list(all = list(mids = h$mids, counts = h$counts, breaks = h$breaks))
  }
  
  # Calculate Histograms for grouped data
  if(!missing(group)){
    hist1 <- data |>
      split(data[, group]) |>
      lapply(function(df) {
        h <- hist(df[, x, drop = TRUE], plot = FALSE)
        list(
          mids = h$mids,
          counts = h$counts,
          breaks = h$breaks,
          group = df[, group, drop = TRUE][1]
        )
      })
    hist2 <- data |>
      split(data[, group]) |>
      lapply(function(df) {
        h <- hist(df[, y, drop = TRUE], plot = FALSE)
        list(
          mids = h$mids,
          counts = h$counts,
          breaks = h$breaks,
          group = df[, group, drop = TRUE][1]
        )
      })
  } else {
    h <- hist(data[, x, drop = TRUE], plot = FALSE)
    hist1 <- list(all = list(mids = h$mids, counts = h$counts, breaks = h$breaks))
    h <- hist(data[, y, drop = TRUE], plot = FALSE)
    hist2 <- list(all = list(mids = h$mids, counts = h$counts, breaks = h$breaks))
  }
  
  # Generate Base chart
  if(!missing(tl)){
    chart1 <- data |> group_by(.data[[tl]]) |> 
      e_charts(timeline = TRUE) |>
      e_hide_grid_lines() |>
      e_x_axis(type = "value") |>
      e_y_axis(type = "value")
    
    chart1$x$opts$baseOption$timeline$show <- FALSE
  } else {
    chart1 <- e_charts() |> 
      e_hide_grid_lines() |>
      e_x_axis(type = "category") |>
      e_y_axis(type = "value")
  }
  
  chart2 <- chart1 
  
  # Helper to build flipped histogram axis configuration
  build_flipped_axes <- function(h) {
    bin_width <- diff(h$breaks)[1]
    list(
      yAxis = list(
        list(
          type = "value",
          min = min(h$breaks) - bin_width / 2,
          max = max(h$breaks) + bin_width / 2
        ),
        list(
          type = "category",
          data = format(h$mids, nsmall = 2),
          boundaryGap = TRUE
        )
      ),
      xAxis = list(list(type = "value", show = FALSE))
    )
  }
  
  # Generate series and axis data
  if(!missing(tl)){
    
    for(i in seq_along(chart1$x$opts$options)){
      g <- names(hist1)[i]
      
      h <- hist1[[g]]
      chart1$x$opts$options[[i]]$series <- list(list(
        name = g,
        type = "bar",
        data = lapply(seq_along(h$mids), function(j) list(h$mids[j], h$counts[j])),
        barWidth = "99%",
        barCategoryGap = "0%"
      ))
      
      h <- hist2[[g]]
      axes <- build_flipped_axes(h)
      chart2$x$opts$options[[i]]$series <- list(list(
        name = g,
        type = "bar",
        data = as.list(h$counts),
        yAxisIndex = 1,
        barWidth = "99%",
        barCategoryGap = "0%"
      ))
      chart2$x$opts$options[[i]]$xAxis <- axes$xAxis
      chart2$x$opts$options[[i]]$yAxis <- axes$yAxis
    }
    
  } else {

    chart1$x$opts$series <- lapply(names(hist1), function(g) {
      h <- hist1[[g]]
      list(
        name = g,
        type = "bar",
        data = lapply(seq_along(h$mids), function(j) list(h$mids[j], h$counts[j])),
        barWidth = "99%",
        barCategoryGap = "0%"
      )
    })
    
    # Right histogram - flipped horizontal bars
    h_ref <- hist2[[1]]
    axes <- build_flipped_axes(h_ref)
    chart2$x$opts$series <- lapply(names(hist2), function(g) {
      h <- hist2[[g]]
      list(
        name = g,
        type = "bar",
        data = as.list(h$counts),
        yAxisIndex = 1,
        barWidth = "99%",
        barCategoryGap = "0%"
      )
    })
    chart2$x$opts$xAxis <- axes$xAxis
    chart2$x$opts$yAxis <- axes$yAxis
  }
  
  # Apply axis visibility settings
  if(show_axis){
    chart1 <- chart1 |>
      e_x_axis(position = "top", axisLine = list(onZero = FALSE))|>
      e_hide_grid_lines()
    chart2 <- chart2 |>
      e_x_axis(show = TRUE) |>
      e_y_axis(position = "right", axisLine = list(onZero = FALSE)) |>
      e_hide_grid_lines()
  } else {
    chart1 <- chart1 |>
      e_x_axis(show = FALSE) |>
      e_y_axis(show = FALSE, min = 0)
    chart2 <- chart2 |>
      e_x_axis(show = FALSE, min = 0) |>
      e_y_axis(show = FALSE)
  }
  
  return(list(chart1, chart2))
}
