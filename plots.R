p_chrom <- function(x, type = c("sum", "max"), facet = TRUE) {
  type <- match.arg(type)
  if (type == "sum") {
    d <- x[, .(Intensity = sum(i)), by = .(file, rt)]
  } else {
    d <- x[, .(Intensity = max(i)), by = .(file, rt)]
  }
  setnames(d, old = c("rt", "file"), new = c("Retention Time", "File"))
  p <- ggplot(d, aes(x = `Retention Time`, y = Intensity)) +
    theme_bw()
  if (facet) {
    p <- p +
      geom_line() +
      facet_wrap(~ File) +
      theme(legend.position = "none")
  } else {
    p <- p +
      geom_line(aes(col = File))
  }
  p
}

p_xic <- function(x, mzrange, rtrange) {
  x <- x[mz >= mzrange[1] & mz <= mzrange[2]]
  x <- x[rt >= rtrange[1] & rt <= rtrange[2]]
  setnames(x, old = c("rt", "i", "file"),
           new = c("Retention Time", "Intensity", "File"))
  ggplot(x, aes(x = `Retention Time`, y = Intensity, col = Intensity)) +
    geom_point() +
    facet_wrap(~ File) +
    scale_color_viridis_c() +
    theme_bw()
}

p_mass <- function(x, file, scan, yaxis) {
  d <- x[file == file & rt == scan]
  if (yaxis == "Relative Abundance") {
    s <- sum(d$i)
    d[, i := 100 * (i / s)]
    ## setnames(d, old = c("mz", "i"),
    ##          new = c("m/z", "Relative Abundance"))
    ytitle <- c("Relative Abundance (%)")

  } else {
    ## setnames(d, old = c("mz", "i"),
    ##          new = c("m/z", "Intensity"))
    ytitle <- c("Intensity")
  }
  ggplot(d, aes(x = mz, ymin = 0, ymax = i)) +
    geom_linerange() +
    theme_bw() +
    xlab("m/z") +
    ylab(ytitle) +
    theme(legend.position = "none")
  ## ggplot(d, aes(x = `m/z`, ymin = 0, ymax = Intensity)) +
  ##   geom_linerange() +
  ##   theme_bw() +
  ##   theme(legend.position = "none")
}


p_feature_bar <- function(x, title) {
  x[, Area := log2(Area)]
  x[, label := sprintf("%.2f", Area)]
  title <- NULL
  if (nrow(x) > 1) {
    area_sd <- sd(x$Area)
    if (is.na(area_sd)) {
      area_sd <- "N/A"
      area_rsd <- "N/A"
    } else{
      area_rsd <- 100 * area_sd / mean(x$Area)
      area_sd <- sprintf("%.2f", area_sd)
      area_rsd <- sprintf("%.2f", area_rsd)
      title <- paste0("SD: ", area_sd, ", ",
                      "RSD: ", area_rsd, "%")
    }
  }
  ggplot(x, aes(x = File, y = Area, fill = File)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
    theme_bw() +
    labs(title = title) +
    ylab("Log2 Peak Area") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45))
}

p_peak_density <- function(x, mzrange, rtrange, rt_offset) {
  x <- x[mz >= mzrange[1] & mz <= mzrange[2]]
  xlim <- c(max(0, (rtrange[1] - rt_offset)),
            rtrange[2] + rt_offset)
  x <- x[rt >= xlim[1] & rt <= xlim[2]]
  maxo <- max(x[rt >= rtrange[1] & rt <= rtrange[2]]$i)
  d_rect <- data.frame(
    xmin = rtrange[1], xmax = rtrange[2], ymin = 0, ymax = maxo
  )
  setnames(x, old = c("rt", "i", "file"),
           new = c("Retention Time", "Intensity", "File"))
  ggplot(x) +
  geom_rect(data = d_rect,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = alpha("gray90", 0.3),
            col = alpha("red", 0.3)
            ) +
    geom_point(aes(x = `Retention Time`, y = Intensity, col = Intensity)) +
    facet_wrap(~ File) +
    scale_color_viridis_c() +
    scale_x_continuous(limits = xlim) +
    theme_bw() +
    theme(legend.position = "none")
}
