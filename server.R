server <- function(input, output, session) {

  ##############################################################################
  ## Validate files have spectra
  ##############################################################################
  flist <- reactive({
    req(input$upload)
    tryCatch({
      has_spectra(input$upload$datapath) ## spectra validation
      input$upload
    }, error = function(e) {
      ## Show notification for invalid files
      showNotification(paste0(
        "Input must be valid mass-spectrometry data files ",
        "in open format (mzML, mzData, mzXML, and netCDF). ",
        "Please re-upload new files."
      ), duration = 5, type = "error", closeButton = FALSE)
      Sys.sleep(6)
      session$reload() ## reload session
    })
  })

  ##############################################################################
  ## Extend input for intermediate states
  ##############################################################################
  v <- reactiveValues(fname = NULL, raw = NULL, fdata = NULL,
                      xic = NULL, massspec = NULL,
                      scan_choices = NULL, peak = NULL,
                      ui_nopeak = FALSE, feature = NULL)

  ##############################################################################
  ## Conditional UI for m/z specification
  ##############################################################################
  mz_manual <- reactive({
    if (isTruthy(input$manual)) {
      TRUE
    } else {
      FALSE
    }
  })

  mandatory_fields <- reactive({
    if (mz_manual()) {
      mandatory_fields_manual
    } else {
      mandatory_fields_preset
    }
  })

  comp <- reactive(input$compound)

  mzr <- reactive({
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
    mandatory_filled <- vapply(
      mandatory_fields(),
      function(x) {
        !is.null(input[[x]]) && input[[x]] != "" &&
          !is.na(suppressWarnings(as.numeric(input[[x]])))
      },
      logical(1)
    )
    mandatory_filled <- all(mandatory_filled)
    if (mandatory_filled && (mzr()[2] <= mzr()[1])) {
      mandatory_filled <- FALSE
    }
    if (mandatory_filled && (rtr()[2] <= rtr()[1])) {
      mandatory_filled <- FALSE
    }
    ## enable/disable the submit button
    shinyjs::toggleState(id = "plot_xic", condition = mandatory_filled)
    shinyjs::toggleState(id = "detect_feature", condition = mandatory_filled)
  })

  ##############################################################################
  ## Conditional default values for feature detection by machine type
  ##############################################################################
  machine_r <- reactive({
    if (is.null(input$machine)) {
      "UPLC / Q-Exactive"
    } else {
      input$machine
    }
  })

  ppm_r <- reactive(get_machine_val(machine_r(), machines, machines_ppm))
  peakwidth_r <- reactive(get_machine_val(machine_r(), machines, machines_peakwidth))
  snthr_r <- reactive(get_machine_val(machine_r(), machines, machines_snthr))
  mzdiff_r <- reactive(get_machine_val(machine_r(), machines, machines_mzdiff))
  noise_r <- reactive(get_machine_val(machine_r(), machines, machines_noise))
  pre_peak_r <- reactive(get_machine_val(machine_r(), machines, machines_pre_peak))
  pre_int_r <- reactive(get_machine_val(machine_r(), machines, machines_pre_int))
  bw_r <- reactive(get_machine_val(machine_r(), machines, machines_bw))
  ## minfrac_r <- reactive(get_machine_val(machine_r(), machines, machines_minfrac))
  binsize_r <- reactive(get_machine_val(machine_r(), machines, machines_binsize))

  observe({
    updateNumericInput(session, "ppm", value = ppm_r())
    updateSliderInput(session, "peakwidth", value = peakwidth_r())
    updateNumericInput(session, "snthr", value = snthr_r())
    updateNumericInput(session, "mzdiff", value = mzdiff_r())
    updateNumericInput(session, "noise", value = noise_r())
    updateNumericInput(session, "pre_peak", value = pre_peak_r())
    updateNumericInput(session, "pre_int", value = pre_int_r())
    updateNumericInput(session, "bw", value = bw_r())
    ## updateNumericInput(session, "minfrac", value = minfrac_r())
    updateNumericInput(session, "binsize", value = binsize_r())
  })

  ##############################################################################
  ## Read LC/MS data onto R via XCMS
  ##############################################################################
  observeEvent(flist(), {
    v$fname <- file_path_sans_ext(flist()$name)
    withProgress(message = "Reading Data...", value = 0, {
      ## v$fname <- file_path_sans_ext(flist()$name)
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
    output$featuredetection <- renderUI(
      tagList(
        featuredetection_ui()
      )
    )
    output$tabs <- renderUI(
      maintabs_ui(v$fdata)
    )
  })

  ##############################################################################
  ## TIC plot
  ##############################################################################
  observeEvent(v$fdata, {
    updateTabsetPanel(session, "tabs", selected = "Total Ion Current")
    observeEvent(input$tic_files, {
      xs <- v$fdata[file %in% input$tic_files]
      if (nrow(xs)) {
        output$tic <- renderPlotly({
          type <- if (input$bpc) "max" else "sum"
          facet <- if (input$collapse) FALSE else TRUE
          ggplotly(p_chrom(xs, type = type, facet = facet))
        })
      }
    })
  })

  ##############################################################################
  ## XIC plot
  ##############################################################################
  observeEvent(v$fdata, {
    output$xic <- renderPlotly(NULL)
    observeEvent(input$plot_xic, {
      updateTabsetPanel(session, "tabs", selected = "Extracted Ion Chromatogram")
      updatePickerInput(
        session, "xic_files",
        selected = unique(as.character(v$fdata$file))
      )
      observeEvent(input$xic_files, {
        v$xic <- p_xic_list(
          v$fdata[file %in% input$xic_files],
          mzrange = mzr(),
          rtrange = rtr(),
          fname = v$fname
        )
        if (!is.null(v$xic)) {
          output$xic <- renderPlotly({
            v$xic
            ## tryCatch(
            ##   v$xic,
            ##   error = function(e) {
            ##     showNotification(
            ##       ui = "No data points are available!",
            ##       duration = 5, type = "error"
            ##     )
            ##     NULL
            ##   }
            ## )
          })
        } else {
          showNotification(
            ui = "No data points are available!",
            duration = 5, type = "error"
          )
        }
      })
    })
  })

  ##############################################################################
  ## Mass spectrum plot
  ##############################################################################
  observeEvent({
    input$massspec_file
    input$ms_int_cut
  }, {
    v$scan_choices <- unique(
      v$fdata[file == input$massspec_file & i >= input$ms_int_cut]$rt
    )
    updateSelectizeInput(session, "scan", choices = v$scan_choices,
                         selected = v$scan_choices[1], server = TRUE)
  })

  observeEvent({
    input$scan
    input$yaxis
  }, {
    if (!is.null(input$scan) && input$scan != "") {
      v$massspec <- p_massspec(
        v$fdata, file = input$massspec_file,
        scan = input$scan, yaxis = input$yaxis
      )
      output$massspec <- renderPlotly({
        if (!is.null(v$massspec)) {
          tryCatch(
            ggplotly(v$massspec),
            error = function(e) NULL
          )
        }
      })
    }
  })

  ##############################################################################
  ## Feature detection
  ##############################################################################
  observeEvent(input$detect_feature, {
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
    raw_sub <- filterMz(
      v$raw, c(mzr()[1] - 5, mzr()[2] + 5) ## extend m/z window
    )
    if (any(is.finite(rtr()))) {
      raw_sub <- filterRt(
        raw_sub, c(rtr()[1] - 20, rtr()[2] + 20) ## extend RT window
      )
    }
    m <- findChromPeaks(raw_sub, param = cpm)
    m <- filterMz(m, mzr())
    if (any(is.finite(rtr()))) {
      m <- filterRt(m, rtr())
    }
    v$peak <- chromPeaks(m)
    if (is.null(v$peak)) {
      showNotification(
        ui = paste0("No peaks detected in the specified region! ",
                    "Adjust peak picking parameters."),
        duration = NULL, type = "error", id = "nopeak"
      )
      v$ui_nopeak <- TRUE
    } else {
      updateTabsetPanel(session, "tabs", selected = "Feature Detection")
      pdp <- PeakDensityParam(
        sampleGroups = rep(1, nrow(input$upload)),
        bw = input$bw,
        minFraction = input$minfrac,
        binSize = input$binsize,
        maxFeatures = 100
      )
      res <- groupChromPeaks(m, pdp)
      if (nrow(featureDefinitions(res)) == 0) {
        showNotification(
          ui = paste0("No Features detected in the specified region! ",
                      "Adjust peak grouping parameters."),
          duration = NULL, type = "error", id = "nopeak"
        )
        v$ui_nopeak <- TRUE
      } else {
        fdef <- as.data.table(featureDefinitions(res), keep.rownames = "feature")
        fval <- as.data.table(featureValues(res), keep.rownames = "feature")
        colnames(fval)[-1] <- as.character(pData(raw_sub)$fname)
        mz_cols <- c("mzmed", "mzmin", "mzmax")
        rt_cols <- c("rtmed", "rtmin", "rtmax")
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

  ##############################################################################
  ## Feature and peak mapping and peak visualization
  ##############################################################################
  observeEvent(input$feature_tbl_rows_selected, {
    idx <- input$feature_tbl_rows_selected

    ## Bar plot for feature intensities
    output$feature_fig <- renderUI(tagList(
      withSpinner(plotlyOutput("feature_bar"))
    ))
    dw <- v$feature[idx, -c(1:11)]
    dl <- melt(dw, measure.vars = colnames(dw),
               variable.name = "File", value.name = "Area")
    dl[, File := factor(File, levels = v$fname)]
    output$feature_bar <- renderPlotly({
      ggplotly(p_feature_area(dl, title))
    })

    ## Feature-peak table
    output$peak_tbl <- renderUI(tagList(
      h5("Peaks for the Selected Feature", style = "color:orange"),
      br(),
      DTOutput("feature_peak_map")
    ))
    peaklist <- as.data.table(v$peak)[v$feature$peakidx[[idx]], ]
    peak_sub <- merge(peaklist, pData(v$raw), by.x = "sample", by.y = "idx",
                      sort = FALSE)
    peak_tbl <- copy(peak_sub)
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
    peak_info <- peak_sub[, .(fname, mzmin, mzmax, rtmin, rtmax)]
    if (anyDuplicated(peak_info$fname)) {
      peak_info <- peak_info[, .(mzmin = min(mzmin), mzmax = max(mzmax),
                                 rtmin = min(rtmin), rtmax = max(rtmax)),
                             by = .(fname)]
    }
    output$peak_chrom_fig <- renderPlotly(p_peak_list(v$fdata, peak_info))
  })

#################################################################################
}
