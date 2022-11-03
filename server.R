
server <- function(input, output, session) {
  ## output$files <- renderTable(input$upload)

  flist <- reactive({
    req(input$upload)
    input$upload
  })

  v <- reactiveValues(fname = NULL, raw = NULL, raw_sub = NULL, fdata = NULL,
                      manual = FALSE, xic = NULL, massspec = NULL,
                      scan_choices = NULL, res = NULL, peak = NULL,
                      ui_nopeak = FALSE, feature = NULL, peak_sub = NULL)


  ## observeEvent(req(input$manual), {
  ##   v$manual <- input$manual
  ## })

  mz_manual <- reactive({
    if (isTruthy(input$manual)) {
      TRUE
    } else {
      FALSE
    }
  })

  mandatory_fields <- reactive({
    ## if (v$manual) {
    if (mz_manual()) {
      mandatory_fields <- mandatory_fields_manual
    } else {
      mandatory_fields <- mandatory_fields_preset
    }
  })

  comp <- reactive({
    input$compound
  })

  mzr <- reactive({
    ## if (v$manual) {
    if (mz_manual()) {
      c(as.numeric(input$xic_mz_min), as.numeric(input$xic_mz_max))
    } else {
      req(input$xic_mz_window)
      get_compound_mzrange(comp(), compound_dat, as.numeric(input$xic_mz_window))
    }
  })

  rtr <- reactive({
    c(as.numeric(input$xic_rt_min), as.numeric(input$xic_rt_max))
  })

  observe({
    ## check if all mandatory fields have a value
    mandatory_filled <-
      vapply(mandatory_fields(),
             function(x) {
               !is.null(input[[x]]) && input[[x]] != "" &&
                 !is.na(suppressWarnings(as.numeric(input[[x]])))
             },
             logical(1))
    mandatory_filled <- all(mandatory_filled)
    if (mandatory_filled && (mzr()[2] <= mzr()[1])) {
      mandatory_filled <- FALSE
    }
    if (mandatory_filled && (rtr()[2] <= rtr()[1])) {
      mandatory_filled <- FALSE
    }
    ## enable/disable the submit button
    shinyjs::toggleState(id = "xic_plot", condition = mandatory_filled)
    shinyjs::toggleState(id = "peak_peaking", condition = mandatory_filled)
  })

  ## observe({
  ##   ## check if all mandatory fields have a value
  ##   mandatory_filled <-
  ##     vapply(mandatory_fields,
  ##            function(x) {
  ##              !is.null(input[[x]]) && input[[x]] != "" &&
  ##                !is.na(suppressWarnings(as.numeric(input[[x]])))
  ##            },
  ##            logical(1))
  ##   mandatory_filled <- all(mandatory_filled)
  ##   if (mandatory_filled && (as.numeric(input$xic_mz_max) <= as.numeric(input$xic_mz_min))) {
  ##     mandatory_filled <- FALSE
  ##   }
  ##   if (mandatory_filled && (as.numeric(input$xic_rt_max) <= as.numeric(input$xic_rt_min))) {
  ##     mandatory_filled <- FALSE
  ##   }
  ##   ## enable/disable the submit button
  ##   shinyjs::toggleState(id = "xic_plot", condition = mandatory_filled)
  ##   shinyjs::toggleState(id = "peak_peaking", condition = mandatory_filled)
  ## })

  observeEvent(flist(), {
    withProgress(message = "Reading Data...", value = 0, {
      v$fname <- file_path_sans_ext(flist()$name)
      v$raw <- readMSData(
        flist()$datapath,
        pdata = new(
          "NAnnotatedDataFrame",
          data.frame(idx = seq_len(nrow(input$upload)),
                     fname = factor(v$fname, levels = v$fname))
        ),
        msLevel. = 1, centroided. = TRUE, mode = "onDisk"
      )
      n <- nrow(flist())
      dl <- list()
      for (i in 1:n) {
        dl[[i]] <- get_df(filterFile(v$raw, i))
        incProgress(1/n, detail = paste0("File ", i))
      }
      v$fdata <- as.data.table(do.call(rbind, dl))
      v$fdata[, file := factor(file, levels = v$fname)]
    })
    output$peakpicking <- renderUI(
      tagList(
        ## pickerInput(
        ##   "fileselect", "File List", choices = flist()$name,
        ##   selected = flist()$name,
        ##   multiple = TRUE, options = list(`actions-box` = TRUE)
        ## ),
        peakpicking_ui()
      )
    )
    output$tabs <- renderUI(
      maintabs_ui(v$fdata)
    )
    ## updateCheckboxInput(session, "manual", value = FALSE)
    ## reset("xic_plot")
  })

  observeEvent(v$fdata, {
    updateTabsetPanel(session, "tabs", selected = "Total Ion Current")
    output$tic <- renderPlotly({
      if (!input$bpc) {
        if (!input$collapse) {
          ggplotly(p_chrom(v$fdata, type = "sum", facet = TRUE))
        } else {
          ggplotly(p_chrom(v$fdata, type = "sum", facet = FALSE))
        }
      } else {
        if (!input$collapse) {
          ggplotly(p_chrom(v$fdata, type = "max", facet = TRUE))
        } else {
          ggplotly(p_chrom(v$fdata, type = "max", facet = FALSE))
        }
      }
    })
    observeEvent(input$xic_plot, {
      updateTabsetPanel(session, "tabs", selected = "Extracted Ion Chromatogram")
      v$xic <- p_xic(
        v$fdata,
        mzrange = mzr(),
        rtrange = rtr()
      )
    })
    output$xic <- renderPlotly({
      if (!is.null(v$xic)) {
        tryCatch(
          ggplotly(v$xic),
          error = function(e) {
            showNotification(
              ui = "No data points are available!",
              duration = 5, type = "error"
            )
            NULL
          }
        )
      }
    })
  })

  observeEvent({
    input$fileselect
    input$ms_int_cut
  }, {
    v$scan_choices <- unique(v$fdata[file == input$fileselect & i >= input$ms_int_cut]$rt)
    updateSelectizeInput(session, "scan", choices = v$scan_choices,
                         selected = v$scan_choices[1], server = TRUE)
  })

  observeEvent({
    input$scan
    input$yaxis
  }, {
    v$massspec <- p_mass(v$fdata, file = input$fileselect, scan = input$scan,
                         yaxis = input$yaxis)
    output$massspec <- renderPlotly({
      if (!is.null(v$massspec)) {
        tryCatch(
          ggplotly(v$massspec),
          error = function(e) NULL
        )
      }
    })
  })

  observeEvent(input$peak_peaking, {
    if (v$ui_nopeak) {
      removeNotification(id = "nopeak")
      v$ui_nopeak <- FALSE
    }
    integrate_method <- ifelse(input$integrate == "Mexican Hat", 1L, 2L)
    fitgauss_method <- ifelse(input$gauss == "False", FALSE, TRUE)
    cpm <- CentWaveParam(
      ppm = input$ppm,
      peakwidth = input$peakwidth,
      snthresh = input$snthr,
      prefilter = c(input$pre_peak, input$pre_int),
      mzCenterFun = input$mzcenter,
      integrate = integrate_method,
      mzdiff = input$mzdiff,
      fitgauss = fitgauss_method,
      noise = input$noise
    )
    ## v$raw_sub <- filterMz(
    ##   v$raw, c(as.numeric(input$xic_mz_min), as.numeric(input$xic_mz_max))
    ## )
    ## v$raw_sub <- filterRt(
    ##   v$raw_sub, c(as.numeric(input$xic_rt_min), as.numeric(input$xic_rt_max))
    ## )
    v$raw_sub <- filterMz(v$raw, mzr())
    v$raw_sub <- filterRt(v$raw_sub, rtr())
    m <- findChromPeaks(v$raw_sub, param = cpm)
    v$peak <- chromPeaks(m)
    if (is.null(v$peak)) {
      showNotification(
        ui = paste0("No peaks detected in the specified region! ",
                    "Adjust peak picking parameters."),
        duration = NULL, type = "error", id = "nopeak"
      )
      v$ui_nopeak <- TRUE
    } else {
      updateTabsetPanel(session, "tabs", selected = "Peak Picking")
      pdp <- PeakDensityParam(
        sampleGroups = rep(1, nrow(input$upload)),
        bw = input$bw,
        minFraction = input$minfrac,
        binSize = input$binsize,
        maxFeatures = 100
      )
      v$res <- groupChromPeaks(m, pdp)
      if (nrow(featureDefinitions(v$res)) == 0) {
        showNotification(
          ui = paste0("No Features detected in the specified region! ",
                      "Adjust peak picking parameters."),
          duration = NULL, type = "error", id = "nopeak"
        )
        v$ui_nopeak <- TRUE
      } else {
        fdef <- as.data.table(featureDefinitions(v$res), keep.rownames = "feature")
        fval <- as.data.table(featureValues(v$res), keep.rownames = "feature")
        colnames(fval)[-1] <- as.character(pData(v$raw_sub)$fname)
        mz_cols <- c("mzmed", "mzmin", "mzmax")
        rt_cols <- c("rtmed", "rtmin", "rtmax")
        ## keep_cols <- c("feature", mz_cols, rt_cols)
        ## v$feature <- merge(fdef[, ..keep_cols], fval, sort = FALSE)
        v$feature <- merge(fdef, fval, sort = FALSE)
        feature_tbl <- v$feature[, .(mzmed, mzmin, mzmax, rtmed, rtmin, rtmax)]
        feature_tbl[, (mz_cols) := lapply(.SD, function(x) sprintf("%.4f", x)),
                    .SDcols = mz_cols]
        feature_tbl[, (rt_cols) := lapply(.SD, function(x) sprintf("%.3f", x)),
                    .SDcols = rt_cols]
        setnames(
          feature_tbl,
          new = c("m/z apex median", "m/z apex min", "m/z apex max",
                  "time apex median", "time apex min", "time apex max"))
        output$feature_tbl <- renderDT(
          ## datatable(feature_tbl[, c(1, 4)], selection = "single")
          datatable(
            feature_tbl[, c(1, 4)],
            selection = list(mode = "single", selected = 1, target = "row")
          )
        )
      }
    }
  })

  observeEvent(input$feature_tbl_rows_selected, {
    idx <- input$feature_tbl_rows_selected
    output$feature_fig <- renderUI(tagList(
      withSpinner(plotlyOutput("feature_bar"))
    ))
    ## Bar plot for feature intensities
    dw <- v$feature[idx, -c(1:11)]
    dl <- melt(dw, measure.vars = colnames(dw),
               variable.name = "File", value.name = "Area")
    dl[, File := factor(File, levels = v$fname)]
    output$feature_bar <- renderPlotly({
      ggplotly(p_feature_bar(dl, title))
    })

    output$peak_tbl <- renderUI(tagList(
      h5("Peaks for the Selected Feature", style = "color:orange"),
      br(),
      DTOutput("feature_peak_map")
      ## br(),
      ## actionButton("peak_show", "Show Peaks")
    ))
    peaklist <- as.data.table(v$peak)[v$feature$peakidx[[idx]], ]
    v$peak_sub <- merge(peaklist, pData(v$raw), by.x = "sample", by.y = "idx",
                        sort = FALSE)
    peak_tbl <- copy(v$peak_sub)
    mz_cols <- c("mz", "mzmin", "mzmax")
    rt_cols <- c("rt", "rtmin", "rtmax")
    peak_tbl[, (mz_cols) := lapply(.SD, function(x) sprintf("%.4f", x)),
             .SDcols = mz_cols]
    peak_tbl[, (rt_cols) := lapply(.SD, function(x) sprintf("%.3f", x)),
             .SDcols = rt_cols]
    peak_tbl[, area := sprintf("%.2f", log2(into))]
    keep_cols <- c("fname", mz_cols, rt_cols, "area")
    peak_tbl <- peak_tbl[, ..keep_cols]
    setnames(peak_tbl, new = c("file", "m/z", "m/z min", "m/z max",
                               "time", "time min", "time max", "log2 area"))
    output$feature_peak_map <- renderDT(datatable(peak_tbl, selection = "none"))

    ## Peak display
    output$peak_fig <- renderUI(tagList(
      br(),
      withSpinner(plotlyOutput("peak_chrom_fig"))
    ))
    peak_info <- v$peak_sub[, .(fname, mzmin, mzmax, rtmin, rtmax)]
    if (anyDuplicated(peak_info$fname)) {
      peak_info <- peak_info[, .(mzmin = min(mzmin), mzmax = max(mzmax),
                                 rtmin = min(rtmin), rtmax = max(rtmax)),
                             by = .(fname)]
    }
    full_rt_range <- c(min(peak_info$rtmin), max(peak_info$rtmax))
    p_peaklist <- list()
    for (i in seq_len(nrow(peak_info))) {
      idx <- which(v$fdata$file == peak_info$fname[i])
      p_peaklist[[i]] <- p_peak_density(
        v$fdata[idx, ],
        mzrange = c(peak_info$mzmin[i], peak_info$mzmax[i]),
        rtrange = full_rt_range,
        ## rt_offset = 3 * input$bw
        rt_offset = 20
      )
    }
    n_plots <- length(p_peaklist)
    n_cols <- ceiling(sqrt(n_plots))
    n_rows <- ceiling(n_plots/n_cols)
    output$peak_chrom_fig <- renderPlotly(
      subplot(p_peaklist, nrows = n_rows, margin = c(0.03, 0.03, 0.07, 0.07))
    )

  })

  ## observeEvent(input$peak_show, {
  ##   output$peak_fig <- renderUI(tagList(
  ##     br(),
  ##     withSpinner(plotlyOutput("peak_chrom_fig"))
  ##   ))
  ##   peak_info <- v$peak_sub[, .(fname, mzmin, mzmax, rtmin, rtmax)]
  ##   if (anyDuplicated(peak_info$fname)) {
  ##     peak_info <- peak_info[, .(mzmin = min(mzmin), mzmax = max(mzmax),
  ##                                rtmin = min(rtmin), rtmax = max(rtmax)),
  ##                            by = .(fname)]
  ##   }
  ##   full_rt_range <- c(min(peak_info$rtmin), max(peak_info$rtmax))
  ##   p_peaklist <- list()
  ##   for (i in seq_len(nrow(peak_info))) {
  ##     idx <- which(v$fdata$file == peak_info$fname[i])
  ##     p_peaklist[[i]] <- p_peak_density(
  ##       v$fdata[idx, ],
  ##       mzrange = c(peak_info$mzmin[i], peak_info$mzmax[i]),
  ##       rtrange = full_rt_range,
  ##       ## rt_offset = 3 * input$bw
  ##       rt_offset = 20
  ##     )
  ##   }
  ##   n_plots <- length(p_peaklist)
  ##   n_cols <- ceiling(sqrt(n_plots))
  ##   n_rows <- ceiling(n_plots/n_cols)
  ##   output$peak_chrom_fig <- renderPlotly(
  ##     subplot(p_peaklist, nrows = n_rows, margin = c(0.03, 0.03, 0.07, 0.07))
  ##   )
  ## })
#################################################################################
}
