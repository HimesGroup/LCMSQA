get_multp_nrow <- function(p_list) {
  n_plots <- length(p_list)
  n_cols <- ceiling(sqrt(n_plots))
  ceiling(n_plots / n_cols)
}

p_tic <- function(x, type = c("sum", "max"), facet = TRUE) {
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

p_massspec <- function(x, file, scan, yaxis) {
  d <- x[file == file & rt == scan]
  setnames(d, old = c("mz", "i"), new = c("m/z", "Intensity"))
  if (yaxis == "Relative Abundance") {
    ## s <- sum(d$i)
    s <- max(d$Intensity)
    d[, Intensity := 100 * (Intensity / s)]
    ytitle <- c("Relative Abundance (%)")

  } else {
    ytitle <- c("Intensity")
  }
  ggplot(d, aes(x = `m/z`, ymin = 0, ymax = Intensity)) +
    geom_linerange() +
    theme_bw() +
    ylab(ytitle) +
    theme(legend.position = "none")
}

p_xic <- function(x, mz_lim, rt_lim, int_lim, blank = FALSE) {
  p_top <- ggplot(x, aes(x = `Retention Time`, y = Intensity, col = Intensity))
  p_bottom <- ggplot(x, aes(x = `Retention Time`, y = `m/z`, col = Intensity))
  if (!blank) {
    p_top <- p_top + geom_point()
    p_bottom <- p_bottom + geom_point()
  } else {
    p_top <- p_top + geom_blank()
    p_bottom <- p_bottom + geom_blank()
  }
  p_top <- p_top +
    facet_wrap(~ File) +
    scale_x_continuous(limits = rt_lim) +
    scale_y_continuous(limits = int_lim) +
    scale_color_viridis_c(limits = int_lim) +
    theme_bw()
  p_bottom <- p_bottom +
    scale_x_continuous(limits = rt_lim) +
    scale_y_continuous(limits = mz_lim) +
    scale_color_viridis_c(limits = int_lim) +
    theme_bw()
  subplot(
    layout(
      ggplotly(p_top),
      yaxis = list(title = list(text = "Intensity", font = list(size = 14)))
    ),
    layout(
      ggplotly(p_bottom),
      yaxis = list(title = list(text = "m/z", font = list(size = 14)))
    ),
    nrows = 2, shareX = TRUE, titleY = TRUE, heights = c(0.6, 0.4)
  )
}

p_xic_list <- function(x, mzrange, rtrange, fname) {
  x <- x[mz >= mzrange[1] & mz <= mzrange[2]]
  x <- x[rt >= rtrange[1] & rt <= rtrange[2]]
  if (!nrow(x)) {
    ## create dummy data when no data for all selected files
    x <- data.table(
      mz = -99, rt = NA_integer_, i = NA_integer_, file = fname
    )
    int_lim = c(0, 100)
  } else {
    maxo <- max(x$i)
    int_lim <- c(0, 1.1 * maxo)
  }
  setnames(x, old = c("mz", "rt", "i", "file"),
           new = c("m/z", "Retention Time", "Intensity", "File"))
  p_list <- lapply(fname, function(i) {
    xs <- x[File == i]
    ## create dummy data when no data for a subset of selected files
    if (nrow(xs) == 0) {
      xs <- x[1, ]
      xs$File <- i
      p_xic(xs, mz_lim = mzrange, rt_lim = rtrange,
            int_lim = int_lim, blank = TRUE)
    ## no data for all files
    } else if (any(xs$mz < 0)) {
      p_xic(xs, mz_lim = mzrange, rt_lim = rtrange,
            int_lim = int_lim, blank = TRUE)
    } else {
      p_xic(xs, mz_lim = mzrange, rt_lim = rtrange,
            int_lim = int_lim, blank = FALSE)
    }
  })
  n_rows <- get_multp_nrow(p_list)
  suppressWarnings(
    layout(
      ## specifying height in layout is now deprecated
      ## need a better solution
      subplot(p_list, nrows = n_rows, shareX = TRUE, titleY = TRUE, margin = 0.05),
      height = 350 * n_rows
    )
  )
}

p_feature_area <- function(x, title) {
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

p_peak <- function(x, mzrange, rtrange, rt_offset) {
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

p_peak_list <- function(x, peak_info) {
  rtrange <- c(min(peak_info$rtmin), max(peak_info$rtmax))
  p_list <- list()
  for (i in seq_along(peak_info$fname)) {
    idx <- which(x$file == peak_info$fname[i])
    xs <- x[idx, ]
    p_list[[i]] <- p_peak(
      xs,
      mzrange = c(peak_info$mzmin[i], peak_info$mzmax[i]),
      rtrange = rtrange,
      ## rt_offset = 3 * input$bw
      rt_offset = 20
    )
  }
  n_plots <- length(p_list)
  n_cols <- ceiling(sqrt(n_plots))
  n_rows <- ceiling(n_plots / n_cols)
  subplot(p_list, nrows = n_rows, margin = c(0.03, 0.03, 0.07, 0.07))
}
