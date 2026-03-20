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

## world map WHO style
plot_world_who <- function(x, iso3 = "COUNTRY", data = "DATA", col.pal = "RdYlBu", col.pal.inv = FALSE,
                           title = NULL, legend.dig.lab = 3L, legend.title = NULL,
                           integer.breaks = FALSE, breaks = NULL, disclaimer = NULL,
                           disclaimer.pal = "Greys", na.countries = NULL){
  
  # WHO admin data
  who_adm0 <- whomapper::pull_sfs(adm_level = 0, query_server = TRUE) 
  sfs_map <- who_adm0$adm0
  sfs_map <- subset(sfs_map, iso_3_code != "XKX")
  
  # General checks
  if (!(iso3 %in% names(x))) 
    stop(sprintf("Input 'x' requires '%s' variable.", iso3))
  if (!(data %in% names(x))) 
    stop(sprintf("Input 'x' requires '%s' variable.", data))
  
  # Add data to geometry
  sfs_map$DATA <- x[[data]][match(sfs_map$iso_3_code, x[[iso3]])]
  
  # Remove countries where data doesn't need to be shown
  sfs_map$DATA[match(na.countries, sfs_map$iso_3_code)] <- NA
  
  # Create breaks
  if (is.numeric(x[[data]])) {
    if (is.null(breaks)) { 
      if (integer.breaks) { # if integer breaks needed
        breaks <- unique(floor(pretty(sfs_map$DATA)))
        sfs_map$cat <- cut(sfs_map$DATA, breaks, right = FALSE, 
                           include.lowest = TRUE, dig.lab = legend.dig.lab)
      } else {
        breaks <- pretty(sfs_map$DATA)
        sfs_map$cat <- cut(sfs_map$DATA, breaks, right = FALSE, 
                           include.lowest = TRUE, dig.lab = legend.dig.lab)
      }
    } else {
      sfs_map$cat <- cut(sfs_map$DATA, breaks, right = FALSE, include.lowest = TRUE, 
                         dig.lab = legend.dig.lab)
    }
  }
  breaks_cat <- levels(sfs_map$cat)
  
  # Add break with disclaimer if needed
  if (!is.null(disclaimer)) {
    sfs_map$disclaimer <- disclaimer[["DISCLAIMER"]][match(sfs_map$iso_3_code, disclaimer[["COUNTRY"]])]
    sfs_map$cat <- ifelse(!is.na(sfs_map$disclaimer),
                          sfs_map$disclaimer, 
                          as.character(sfs_map$cat))
    sfs_map$cat <- as.factor(sfs_map$cat)
    
    disclaimer_cat <- unique(subset(disclaimer, !is.na(DISCLAIMER))$DISCLAIMER)
  } else {
    disclaimer_cat <- NULL
  }
  
  # geometry without data gets shown as NA
  sfs_map$cat <- ifelse(is.na(sfs_map$cat),
                        "Data not available",
                        as.character(sfs_map$cat))
  cats <- c(breaks_cat, disclaimer_cat, "Data not available")
  
  # Fix factors
  real_cat <- unique(sfs_map$cat)
  sfs_map$cat <- factor(sfs_map$cat, levels = cats)
  
  # Define colors
  # Define palette for data and reverse if wanted
  if (col.pal %in% rownames(brewer.pal.info)){
    breaks_cols <- brewer.pal(length(breaks_cat), col.pal)
  } else {
    breaks_cols <- viridis(length(breaks_cat), option = "D")
  }
  if (col.pal.inv) {
    breaks_cols <- rev(breaks_cols)
  }
  names(breaks_cols) <- breaks_cat
  
  # Define palette for disclaimers
  if (!is.null(disclaimer)) {
    disclaimer_cat  <- unique(subset(disclaimer, !is.na(DISCLAIMER))$DISCLAIMER)
    if(disclaimer.pal == "Greys"){
      disclaimer_cols <- brewer.pal(8,disclaimer.pal)[-c(3,4)] # The third and fourth color of the palette looks similar to the NA color of WHO so should be removed
    } else {
      disclaimer_cols <- brewer.pal(8,disclaimer.pal)
    }
    disclaimer_cols <- disclaimer_cols[1:length(disclaimer_cat)] 
  } else {
    disclaimer_cols <- NULL
  }
  names(disclaimer_cols) <- disclaimer_cat
  
  # Define colors for data not available
  dna_cat <- c("Data not available")
  dna_cols <- c("#F0F0F0")
  names(dna_cols) <- dna_cat
  
  # Add not applicable to legend
  na_cat <- c("Not applicable")
  na_cols <- c("#cccccc")
  names(na_cols) <- na_cat
  
  # Combine colors and add color for addiitonal category
  cols <- c(breaks_cols, disclaimer_cols, dna_cols, na_cols)
  
  ## Plot
  ggplot() +
    geom_sf_who_poly(data = sfs_map, aes(fill = cat), show.legend = TRUE) +
    scale_fill_manual(values = cols,
                      breaks = names(cols),
                      limits = names(cols),
                      guide = guide_legend(reverse = FALSE),
                      drop = FALSE) +
    labs(title = title, 
         fill = legend.title) +
    who_map_pipeline(na_scale = FALSE, 
                     no_data_scale = FALSE, 
                     logo_location = "bottomright",
                     no_annotation = TRUE,
                     background_col = "#E0E8FF")
  
}

## helper: plot area of world in box
plot_area <- function(sfs_map, area, cols, area_name = NULL){
  # create box around area
  bbox <- st_bbox(subset(sfs_map, FOCUS == area))
  pad <- 2
  # Plot
  plot <- ggplot() +
    geom_sf(data = sfs_map, aes(fill = cat), show.legend = TRUE) +
    scale_fill_manual(values = cols,
                      breaks = names(cols),
                      limits = names(cols),
                      guide = guide_legend(reverse = FALSE, ncol = 3),
                      drop = FALSE) +
    who_map_pipeline(na_scale       = FALSE, 
                     no_data_scale  = FALSE, 
                     logo_location  = "bottomright",
                     no_annotation  = TRUE,
                     background_col = "#E0E8FF") +
    coord_sf(xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
             ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad),
             expand = FALSE) +
    theme(legend.position="none",
          panel.background = element_rect(fill = "#E0E8FF"),
          plot.background  = element_rect(fill = "#E0E8FF"),
          panel.grid       = element_blank(),
          # panel.border     = element_rect(color = "white", fill = NA, linewidth = 2),
          panel.border     = element_blank(),
          axis.title.x     = element_blank(),
          axis.title.y     = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_blank(),
          axis.ticks       = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  # if needed add name of area
  if (!is.null(area_name)) {
    plot <- plot +
      annotate("text",
               x        = bbox["xmax"] + pad * 0.75,
               y        = bbox["ymax"] + pad * 0.75,
               label    = area_name,
               hjust    = 1, vjust = 1,
               size     = 4, fontface = "bold", color = "black")
  }
  
  
  return(plot)
}

## helper: plot one country in box
plot_country <- function(sfs_map, country, cols, country_name = NULL, island_group = FALSE){
  # Subset data
  sub_data <- subset(sfs_map, iso_3_code == country)
  # If island group to big, keep largest island
  if(island_group){
    sub_data <- st_make_valid(sub_data)
    sub_data <- st_cast(sub_data, "POLYGON")
    sub_data <- sub_data[which.max(st_area(sub_data)), ]
  }
  # create squared box around island(s)
  bbox     <- st_bbox(sub_data)
  x_mid   <- (bbox["xmax"] + bbox["xmin"]) / 2
  y_mid   <- (bbox["ymax"] + bbox["ymin"]) / 2
  half    <- max(bbox["xmax"] - bbox["xmin"],
                 bbox["ymax"] - bbox["ymin"]) / 2
  half <- half * 1.2
  # Plot
  plot <- ggplot() +
    geom_sf(data = sub_data, aes(fill = cat), show.legend = TRUE) +
    scale_fill_manual(values = cols,
                      breaks = names(cols),
                      limits = names(cols),
                      guide  = guide_legend(reverse = FALSE, ncol = 3),
                      drop   = FALSE) +
    who_map_pipeline(na_scale       = FALSE, 
                     no_data_scale  = FALSE, 
                     logo_location  = "bottomright",
                     no_annotation  = TRUE,
                     background_col = "#E0E8FF") +
    coord_sf(xlim = c(x_mid - half, x_mid + half),
             ylim = c(y_mid - half, y_mid + half),
             expand = FALSE) +
    theme(legend.position  = "none",
          panel.background = element_rect(fill = "#E0E8FF"),
          plot.background  = element_rect(fill = "#E0E8FF"),
          # panel.border     = element_rect(color = "white", fill = NA, linewidth = 1),
          panel.border     = element_blank(),
          panel.grid       = element_blank(),
          axis.title.x     = element_blank(),
          axis.title.y     = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_blank(),
          axis.ticks       = element_blank(),
          plot.margin      = unit(c(0, 0, 0, 0), "cm"))
  
  # if needed add name of country
  if (!is.null(country_name)) {
    plot <- plot +
      annotate("text",
               x     = x_mid + half * 0.95,
               y     = y_mid + half * 0.95,
               label    = country_name,
               hjust    = 1, vjust = 1,
               size     = 4, fontface = "bold", color = "black")
  }
  
  return(plot)
}

## helper: draw white border around area
draw_border <- function(x, y, w, h) {
  draw_grob(
    grid::rectGrob(
      x      = x + w/2, 
      y      = y + h/2,
      width  = w, 
      height = h,
      gp     = grid::gpar(fill = NA, col = "white", lwd = 2)
    )
  )
}

## world map WHO style with zooms

plot_world_who_zoom <- function(x, iso3 = "COUNTRY", data = "DATA", col.pal = "RdYlBu", col.pal.inv = FALSE,
                                title = NULL, legend.dig.lab = 3L, legend.title = NULL,
                                integer.breaks = FALSE, breaks = NULL, disclaimer = NULL,
                                disclaimer.pal = "Greys", na.countries = NULL){
  # WHO admin data
  who_adm0 <- whomapper::pull_sfs(adm_level = 0, query_server = TRUE) 
  sfs_map  <- who_adm0$adm0
  
  # General checks
  if (!(iso3 %in% names(x))) 
    stop(sprintf("Input 'x' requires '%s' variable.", iso3))
  if (!(data %in% names(x))) 
    stop(sprintf("Input 'x' requires '%s' variable.", data))
  
  # Add data to geometry
  sfs_map$DATA <- x[[data]][match(sfs_map$iso_3_code, x[[iso3]])]
  
  # Remove countries where data doesn't need to be shown
  sfs_map$DATA[match(na.countries, sfs_map$iso_3_code)] <- NA
  
  # Create breaks
  if (is.numeric(x[[data]])) {
    if (is.null(breaks)) { 
      if (integer.breaks) { # if integer breaks needed
        breaks <- unique(floor(pretty(sfs_map$DATA)))
        sfs_map$cat <- cut(sfs_map$DATA, breaks, right = FALSE, 
                           include.lowest = TRUE, dig.lab = legend.dig.lab)
      } else {
        breaks <- pretty(sfs_map$DATA)
        sfs_map$cat <- cut(sfs_map$DATA, breaks, right = FALSE, 
                           include.lowest = TRUE, dig.lab = legend.dig.lab)
      }
    } else {
      sfs_map$cat <- cut(sfs_map$DATA, breaks, right = FALSE, include.lowest = TRUE, 
                         dig.lab = legend.dig.lab)
    }
  }
  breaks_cat <- levels(sfs_map$cat)
  
  # Add break with disclaimer if needed
  if (!is.null(disclaimer)) {
    sfs_map$disclaimer <- disclaimer[["DISCLAIMER"]][match(sfs_map$iso_3_code, disclaimer[["COUNTRY"]])]
    sfs_map$cat        <- ifelse(!is.na(sfs_map$disclaimer),
                                 sfs_map$disclaimer, 
                                 as.character(sfs_map$cat))
    sfs_map$cat        <- as.factor(sfs_map$cat)
    
    disclaimer_cat     <- unique(subset(disclaimer, !is.na(DISCLAIMER))$DISCLAIMER)
  } else {
    disclaimer_cat     <- NULL
  }
  
  # geometry without data gets shown as NA
  sfs_map$cat <- ifelse(is.na(sfs_map$cat),
                        "Data not available",
                        as.character(sfs_map$cat))
  cats        <- c(breaks_cat, disclaimer_cat, "Data not available")
  
  # Fix factors
  real_cat    <- unique(sfs_map$cat)
  sfs_map$cat <- factor(sfs_map$cat, levels = cats)
  
  # Define colors
  # Define palette for data and reverse if wanted
  if (col.pal %in% rownames(brewer.pal.info)){
    breaks_cols <- brewer.pal(length(breaks_cat), col.pal)
  } else {
    breaks_cols <- viridis(length(breaks_cat), option = "D")
  }
  
  if (col.pal.inv) {
    breaks_cols <- rev(breaks_cols)
  }
  names(breaks_cols) <- breaks_cat
  
  # Define palette for disclaimers
  if (!is.null(disclaimer)) {
    disclaimer_cat  <- unique(subset(disclaimer, !is.na(DISCLAIMER))$DISCLAIMER)
    if(disclaimer.pal == "Greys"){
      disclaimer_cols <- brewer.pal(8,disclaimer.pal)[-c(3,4)] # The third and fourth color of the palette looks similar to the NA color of WHO so should be removed
    } else {
      disclaimer_cols <- brewer.pal(8,disclaimer.pal)
    }
    disclaimer_cols <- disclaimer_cols[1:length(disclaimer_cat)] 
  } else {
    disclaimer_cols <- NULL
  }
  names(disclaimer_cols) <- disclaimer_cat
  
  # Define colors for data not available
  dna_cat         <- c("Data not available")
  dna_cols        <- c("#F0F0F0")
  names(dna_cols) <- dna_cat
  
  # Add not applicable to legend
  na_cat         <- c("Not applicable")
  na_cols        <- c("#cccccc")
  names(na_cols) <- na_cat
  
  # Combine colors and add color for additional category
  cols <- c(breaks_cols, disclaimer_cols, dna_cols, na_cols)
  
  # Define countries in area that need a zoom
  sfs_map <- sfs_map %>% 
    mutate(FOCUS =  case_when(
      iso_3_code %in% c("ATG", "BHS", "BRB", "DMA", "DOM", "HTI", "JAM", "KNA", "LCA", "VCT", "TTO") ~ "CARIBBEAN",
      iso_3_code %in% c("ALB", "AUT", "GRC", "ISR", "JOR","MDA", "SVK") ~ "EUROPE",
      .default = NA))
  
  # Plot world and areas
  p_area <- list()
  p_area$WORLD <- ggplot() +
    geom_sf_who_poly(data = sfs_map, aes(fill = cat), show.legend = TRUE, colour = "black") +
    scale_fill_manual(values = cols,
                      breaks = names(cols),
                      limits = names(cols),
                      guide  = guide_legend(reverse = FALSE, ncol = 1),
                      drop   = FALSE) +
    labs(title = title, 
         fill = legend.title) + 
    who_map_pipeline(na_scale       = FALSE, 
                     no_data_scale  = FALSE, 
                     logo_location  = "bottomright",
                     no_annotation  = TRUE,
                     background_col = "#E0E8FF") +
    theme(legend.background     = element_rect(fill = "#E0E8FF", color = NA),
          legend.box.background = element_rect(fill = "#E0E8FF", color = NA),
          legend.key            = element_rect(fill = "#E0E8FF", color = NA),
          legend.position       = c(0,0.12),
          legend.justification  = c("left", "bottom"),
          legend.box.just       = "left",
          legend.margin         = margin(6, 6, 6, 6),
          legend.title          = element_text(size = 14),
          legend.text           = element_text(size = 12),
          legend.key.size       = unit(0.3, "cm"),
          legend.spacing.y      = unit(0.1, "cm"),
          legend.spacing.x      = unit(0.2, "cm"),
          panel.border          = element_blank(),
          panel.background      = element_rect(fill = "#E0E8FF", color = NA),
          plot.background       = element_rect(fill = "#E0E8FF", color = NA),
          panel.spacing         = unit(0, "cm"),
          plot.margin           = unit(c(0, 0, 0, 0), "cm"),
          panel.grid            = element_blank(),
          axis.text             = element_blank(),
          axis.ticks            = element_blank(),
          axis.title            = element_blank(),
          strip.background      = element_blank(),
          strip.text            = element_blank(),
          panel.spacing.x       = unit(0, "cm"),
          panel.spacing.y       = unit(0, "cm"))
  
  p_area$CARIBBEAN <- plot_area(sfs_map, "CARIBBEAN", cols, "Caribbean")
  p_area$EUROPE    <- plot_area(sfs_map, "EUROPE", cols)
  
  # Plot islands
  p_islands     <- list()
  p_islands$STP <- plot_country(sfs_map, "STP", cols, country_name = "STP") 
  p_islands$COM <- plot_country(sfs_map, "COM", cols, country_name = "COM") 
  p_islands$MDV <- plot_country(sfs_map, "MDV", cols, country_name = "MDV", island_group = TRUE) 
  p_islands$MUS <- plot_country(sfs_map, "MUS", cols, country_name = "MUS", island_group = TRUE) 
  p_islands$CPV <- plot_country(sfs_map, "CPV", cols, country_name = "CPV") 
  p_islands$SYC <- plot_country(sfs_map, "SYC", cols, country_name = "SYC", island_group = TRUE) 
  p_islands$PLW <- plot_country(sfs_map, "PLW", cols, country_name = "PLW", island_group = TRUE) 
  p_islands$TON <- plot_country(sfs_map, "TON", cols, country_name = "TON", island_group = TRUE)
  p_islands$WSM <- plot_country(sfs_map, "WSM", cols, country_name = "WSM")
  p_islands$NIU <- plot_country(sfs_map, "NIU", cols, country_name = "NIU")
  p_islands$COK <- plot_country(sfs_map, "COK", cols, country_name = "COK", island_group = TRUE)
  p_islands$KIR <- plot_country(sfs_map, "KIR", cols, country_name = "KIR", island_group = TRUE)
  p_islands$FJI <- plot_country(sfs_map, "FJI", cols, country_name = "FJI", island_group = TRUE)
  p_islands$MHL <- plot_country(sfs_map, "MHL", cols, country_name = "MHL", island_group = TRUE)
  p_islands$FSM <- plot_country(sfs_map, "FSM", cols, country_name = "FSM", island_group = TRUE)
  p_islands$NRU <- plot_country(sfs_map, "NRU", cols, country_name = "NRU")
  p_islands$SLB <- plot_country(sfs_map, "SLB", cols, country_name = "SLB")
  p_islands$TUV <- plot_country(sfs_map, "TUV", cols, country_name = "TUV", island_group = TRUE)
  p_islands$VUT <- plot_country(sfs_map, "VUT", cols, country_name = "VUT")
  p_islands$MLT <- plot_country(sfs_map, "MLT", cols, country_name = "MLT")
  p_islands$SGP <- plot_country(sfs_map, "SGP", cols, country_name = "SGP")
  
  # Three-row island layout
  row1 <- c("MUS", "SYC", "COM", "CPV", "STP", "MDV", "MLT")
  row2 <- c("COK", "NIU", "NRU", "SGP", "FJI", "MHL", "PLW")
  row3 <- c("TON", "TUV", "FSM", "KIR", "SLB", "VUT", "WSM")
  
  # Define widths of areas and islands
  island_cols  <- max(length(row1), length(row2), length(row3))
  area_width   <- 3.5
  island_width <- 1
  total_width  <- (3 * area_width) + (island_cols * island_width)
  
  # Define height zooms
  zoom_height   <- 0.32
  main_height   <- 1 - zoom_height
  island_height <- zoom_height / 3
  
  # placement on xaxis
  x_offset        <- 0.1
  europe_x        <- 0
  europe_width    <- area_width / total_width
  caribbean_x     <- europe_width
  caribbean_width <- area_width / total_width
  island_x_start  <- (2 * area_width) / total_width
  island_w_f      <- island_width / total_width
  
  # Gather world, areas and islands
  # Main plot
  final_plot <- ggdraw() +
    draw_plot(p_area$WORLD,     
              x = 0,             
              y = zoom_height, 
              width = 1, 
              height = main_height) +
    draw_plot(p_area$EUROPE,    
              x = x_offset,    
              y = 0.07, 
              width = europe_width,    
              height = zoom_height) +
    draw_plot(p_area$CARIBBEAN, 
              x = x_offset + europe_width, 
              y = 0.07, 
              width = caribbean_width, 
              height = zoom_height)
  
  # Add first row islands
  for (i in seq_along(row1)) {
    final_plot <- final_plot +
      draw_plot(p_islands[[row1[i]]],
                x      = x_offset + island_x_start + (i - 1) * island_w_f,
                y      = 2*island_height + 0.07 ,
                width  = island_w_f,
                height = island_height)
  }
  
  # Add second row islands
  for (i in seq_along(row2)) {
    final_plot <- final_plot +
      draw_plot(p_islands[[row2[i]]],
                x      = x_offset + island_x_start + (i - 1) * island_w_f,
                y      = island_height + 0.07 ,
                width  = island_w_f,
                height = island_height)
  }
  
  # Add third row islands
  for (i in seq_along(row3)) {
    final_plot <- final_plot +
      draw_plot(p_islands[[row3[i]]],
                x      = x_offset + island_x_start + (i - 1) * island_w_f,
                y      = 0.07 ,
                width  = island_w_f,
                height = island_height)
  }
  
  # Add general background to plot
  final_plot <- final_plot +
    theme(plot.background = element_rect(fill = "#E0E8FF", color = NA))
  
  # Add white border to areas
  final_plot <- final_plot +
    draw_border(x_offset,              
                0.07, 
                europe_width,    
                zoom_height) +
    draw_border(x_offset + europe_width,   
                0.07, 
                caribbean_width, 
                zoom_height)
  
  # Borders for island row 1
  for (i in seq_along(row1)) {
    final_plot <- final_plot +
      draw_border(x      = x_offset + island_x_start + (i - 1) * island_w_f,
                  y      = 2 * island_height + 0.07,
                  w      = island_w_f,
                  h      = island_height)
  }
  
  # Borders for island row 2
  for (i in seq_along(row2)) {
    final_plot <- final_plot +
      draw_border(x      = x_offset + island_x_start + (i - 1) * island_w_f,
                  y      = island_height + 0.07,
                  w      = island_w_f,
                  h      = island_height)
  }
  
  # Borders for island row 3
  for (i in seq_along(row3)) {
    final_plot <- final_plot +
      draw_border(x      = x_offset + island_x_start + (i - 1) * island_w_f,
                  y      = 0.07,
                  w      = island_w_f,
                  h      = island_height)
  }
  
  return(final_plot)
}

