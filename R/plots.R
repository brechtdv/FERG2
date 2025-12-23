### FERG2 / plot functions

## world map

plot_world <-
function(x, iso3 = "ISO3", data = "DATA", col.pal = "Reds",
  col.pal.inv = FALSE, cols = NULL, col.br = "#6E6E6E", 
  legend.dig.lab = 3L, legend.labs = NULL, legend.title = NULL,
  legend.ncol = 3, title.adj = 0.5, text.width = 20,
  integer.breaks = FALSE, breaks = NULL, diseasefree = NULL,
  diseasefree.title = "Disease-free", na.countries = NULL)  {
  # check arguments
  if (!(iso3 %in% names(x)))
    stop(sprintf("Input 'x' requires '%s' variable.", iso3))
  if (!(data %in% names(x)))
    stop(sprintf("Input 'x' requires '%s' variable.", data))

  # merge data
  map1$DATA <- x[[data]][match(map1$ISO_3_CODE, x[[iso3]])]

  # overwrite NA countries
  map1$DATA[match(na.countries, map1$ISO_3_CODE)] <- NA

  # settings
  col_na <- rgb(210, 210, 210, max = 255)
  col_bg <- rgb(224, 232, 255, max = 255)
  col_lk <- rgb(190, 210, 255, max = 255)
  col_br <- col.br

  # define colors
  if (is.numeric(x[[data]])) {
    if (is.null(breaks)) {
      if (integer.breaks) {
        breaks <- unique(floor(pretty(map1$DATA)))
        cat <-
          cut(map1$DATA, breaks, right = FALSE, include.lowest = TRUE,
              dig.lab = legend.dig.lab)
      } else {
        breaks <- pretty(map1$DATA)
        cat <-
          cut(map1$DATA, breaks, right = FALSE, include.lowest = TRUE,
              dig.lab = legend.dig.lab)
      }
    } else {
      cat <-
        cut(map1$DATA, breaks, right = FALSE, include.lowest = TRUE,
            dig.lab = legend.dig.lab)
    }
    
    # Add disease-free countries if asked
    if(!is.null(diseasefree)){
      map1$DISEASEFREE <-
        diseasefree[["DISEASEFREE"]][
          match(map1$ISO_3_CODE, diseasefree[["COUNTRY"]])]
      levels(cat) <- c(levels(cat), diseasefree.title)
      cat[map1$DISEASEFREE == 0] <- diseasefree.title
      if (length(col.pal) == 1) {
        col <- RColorBrewer::brewer.pal(nlevels(cat)-1, col.pal)
        if (col.pal.inv) col <- rev(col)
        col <- c(col, "white")
      } else {
        col <- col.pal
      }
    } else {
      if (length(col.pal) == 1) {
        col <- RColorBrewer::brewer.pal(nlevels(cat), col.pal)
        if (col.pal.inv) col <- rev(col)
      } else {
        col <- col.pal
      }
    }
    
    cols <- col[cat]
    
    if (is.null(legend.labs)) {
      legend.labs <- levels(cat)
	  legend.labs <- gsub(",", ", ", legend.labs, fixed = TRUE)
    }

  } else {
    map1$DATA <- as.factor(map1$DATA)
    if (length(col.pal) == 1) {
      col <- RColorBrewer::brewer.pal(nlevels(map1$DATA), col.pal)
      if (col.pal.inv) col <- rev(col)
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
    cex = 0.9,
    y.intersp = 0.9,
    x.intersp = 0.5,
    text.width = text.width,
    ncol = legend.ncol,
    title.adj = title.adj)
  
  # return breaks
  return(breaks)
}

## number of data points per country

plot_world_data <-
function(x, legend.ncol = 1, text.width=20, title.adj=0.5, ...) {
  world <- countries
  world <- merge(all = TRUE, world, data.frame(xtabs(~ISO3, x)))
  world$Freq[is.na(world$Freq)] <- 0

  max_freq <- max(world$Freq, na.rm = TRUE)
  if (max_freq < 10) {
    world$Freq <-
      factor(world$Freq, levels = seq(0, max_freq))
    col.pal <- RColorBrewer::brewer.pal(max_freq, "Greens")
    col.pal <- c("white", col.pal)
    col.pal <- col.pal[seq(max_freq+1)]
    plot_world(
      world, iso3 = "ISO3", data = "Freq",
      col.pal = col.pal, legend.ncol = ifelse(max_freq<=5, 1, 2), ...)

  } else {
    breaks <- pretty(world$Freq[world$Freq != 0]) 
    if (breaks[1] == 0) 
      breaks[1] <- 1
    breaks <- c(0, breaks) 
    col.pal <- RColorBrewer::brewer.pal(length(breaks) - 2, "Greens") 
    col.pal <- c("white", col.pal) 
    legend.labs <- c(levels(cut(world$Freq, breaks, right = F, 
                                include.lowest = T))) 
    legend.labs[1] <- "0" 
    plot_world(world, iso3 = "ISO3", data = "Freq", col.pal = col.pal,
               text.width = text.width, legend.labs = legend.labs, 
               breaks = breaks, title.adj = title.adj, ...) #plot map
  }
}

## imputation map

plot_world_imputation <- function (x, sub = c("SUB2", "SUB1"), ...) {
  sub <- match.arg(sub)
  if (sub == "SUB2"){
    reg <- "REG2"
  } else {
    reg <- "REG1"
  }
  world <- countries
  world$col <- "firebrick2"
  has_reg <- unique(world[[reg]][world$ISO3 %in% x$ISO3])
  has_sub <- unique(world[[sub]][world$ISO3 %in% x$ISO3])
  world$col[world[[reg]] %in% has_reg] <- "darkorange"
  world$col[world[[sub]] %in% has_sub] <- "gold1"
  world$col[world$ISO3 %in% x$ISO3] <- "darkolivegreen3"
  world$col <- factor(world$col, levels = c("darkolivegreen3", "gold1", 
                                            "darkorange","firebrick2"))
  plot_world(world, 
             iso3 = "ISO3", 
             data = "col", 
             col.pal = c("darkolivegreen3", "gold1", "darkorange","firebrick2"), 
             legend.ncol = 1, 
             legend.labs = c("Data in country",
                             "Data in subregion", 
                             "Data in region",
                             "No data in region"),...)
}

## plot FERG1 subregions

plot_world_sub1 <-
function() {
  col.pal <-
    c(RColorBrewer::brewer.pal(4, "Oranges")[3:4],
      RColorBrewer::brewer.pal(4, "Reds")[-1],
      RColorBrewer::brewer.pal(4, "Greys")[3:4],
      RColorBrewer::brewer.pal(4, "Blues")[-1],
      RColorBrewer::brewer.pal(4, "Greens")[3:4],
      RColorBrewer::brewer.pal(4, "Purples")[3:4])
  plot_world(countries, "ISO3", "SUB1", col.pal = col.pal)
}

## plot FERG2 subregions

plot_world_sub2 <-
function() {
  col.pal <-
    c(RColorBrewer::brewer.pal(4, "Oranges")[-1],
      RColorBrewer::brewer.pal(4, "Reds")[-1],
      RColorBrewer::brewer.pal(4, "Greys")[-1],
      RColorBrewer::brewer.pal(4, "Blues")[-1],
      RColorBrewer::brewer.pal(4, "Greens")[3:4],
      RColorBrewer::brewer.pal(4, "Purples")[-1])
  plot_world(countries, "ISO3", "SUB2", col.pal = col.pal)
}

## plot data availability

plot_data <-
function(x, by = c("REG2", "REG1", "SUB2", "SUB1", "COUNTRY"), range = NULL) {
  # check arguments
  by <- match.arg(by)

  # set variables
  x$YEAR <- round(x$YEAR)
  x$LOCATION <- x[[by]]

  # set range
  if (is.null(range)) range <- range(x$YEAR)
  x <- subset(x, YEAR >= min(range) & YEAR <= max(range))

  # count entries by year and location
  count <- xtabs(~YEAR+LOCATION, x)
  count <- as.data.frame(count, stringsAsFactors = FALSE)

  # find all locations
  all_locations <-
  switch(by,
         "REG2" = unique(countries$REG2),
         "REG1" = unique(countries$REG1),
         "SUB2" = unique(countries$SUB2),
         "SUB1" = unique(countries$SUB1),
         "COUNTRY" = countries$ISO3)

  # expand dataframe to make complete
  count <- merge(all = TRUE,
    count,
    expand.grid(YEAR = seq(min(range), max(range)),
                LOCATION = all_locations))
  count$Freq[count$Freq == 0] <- NA

  # plot
  n_breaks <- min(5, max(count$Freq, na.rm = TRUE))
  ggplot(count, aes(x = YEAR, y = LOCATION)) +
    geom_tile(
      aes(fill = Freq),
      linewidth = 0.5, color = "#cccccc") +
    scale_fill_distiller(
      "Count",
      direction = 1,
      na.value = NA,
      breaks = scales::breaks_extended(n = n_breaks)) +
    scale_x_discrete(NULL) +
    scale_y_discrete(NULL, limits = rev(sort(all_locations))) +
    theme_classic() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}
