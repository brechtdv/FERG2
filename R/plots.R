### FERG2 / plot functions

## world map

plot_world <-
function(x, iso3 = "ISO3", data = "DATA", col.pal = "Reds", cols = NULL,
  legend.labs = NULL, legend.title = NULL, legend.ncol = 3) {
  # check arguments
  if (!(iso3 %in% names(x)))
    stop(sprintf("Input 'x' requires '%s' variable.", iso3))
  if (!(data %in% names(x)))
    stop(sprintf("Input 'x' requires '%s' variable.", data))

  # merge data
  map1$DATA <- x[[data]][match(map1$ISO_3_CODE, x[[iso3]])]

  # settings
  col_na <- rgb(210, 210, 210, max = 255)
  col_bg <- rgb(224, 232, 255, max = 255)
  col_br <- rgb(110, 110, 110, max = 255)
  col_lk <- rgb(190, 210, 255, max = 255)

  # define colors
  if (is.numeric(x[[data]])) {
    breaks <- pretty(map1$DATA)
    cat <- cut(map1$DATA, breaks)
    if (length(col.pal) == 1) {
      col <- RColorBrewer::brewer.pal(length(levels(cat)), col.pal)
    } else {
      col <- col.pal
    }
    cols <- col[cat]
    
    if (is.null(legend.labs))
      legend.labs <- levels(cat)

  } else if (is.factor(x[[data]])) {
    if (length(col.pal) == 1) {
      col <- RColorBrewer::brewer.pal(length(levels(cat)), col.pal)
    } else {
      col <- col.pal
    }
    cols <- col[map1$DATA]

    if (is.null(legend.labs))
      legend.labs <- levels(map1$DATA)
  }
  cols[is.na(cols)] <- col_na

  # setup plot
  par(mar = c(0, 0, 0, 0))
  plot(st_geometry(map1), col = cols, bg = col_bg, border = col_br, lwd = .75)
  plot(st_geometry(map3), add = T, lwd = .75,
     col = c(col_na, col_bg, col_na),
     border = c(col_br, col_lk, col_br))
  plot(st_geometry(map2), add = T, lwd = .75,
       col = "white", lty = 1)
  plot(st_geometry(map2), add = T, lwd = .75,
       col = rgb(110, 110, 110, max = 255),
       lty = c(3, 3, 1, 3, 3, 3, 3, 3, 3, 3))

  # add legend
  legend("bottomleft",
    title = legend.title,
    box.col = NA, bg = NA, 
    legend = c(legend.labs, "Not applicable"),
    fill = c(col, col_na),
    cex = .9, y.intersp = .9, x.intersp = 0.5, text.width = 20,
    ncol = legend.ncol)
}
