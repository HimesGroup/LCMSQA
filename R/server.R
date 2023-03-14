server <- function(input, output, session) {
  ## Set file size limit
  options(shiny.maxRequestSize = 1000 * (1024**2))

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
  v <- reactiveValues(fname = NULL, raw = NULL, compound_dat = NULL,
                      fdata = NULL, p_mass_chrom = NULL, mass_dat = NULL,
                      peak = NULL, ui_nopeak = FALSE, feature = NULL)

  ##############################################################################
  ## Conditional UI for m/z specification
  ##############################################################################
  mz_manual <- reactive({
    if (isTruthy(input$manual) ||
        (!is.null(input$standard_skip) & isTruthy(input$standard_skip))) {
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
      req(input$xic_mz_val)
      req(input$xic_mz_err)
      get_mzrange(as.numeric(input$xic_mz_val), as.numeric(input$xic_mz_err))
    } else {
      req(input$xic_mz_window)
      get_compound_mzrange(comp(), v$compound_dat, as.numeric(input$xic_mz_window))
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
    toggleState(id = "plot_xic", condition = mandatory_filled)
    toggleState(id = "detect_feature", condition = mandatory_filled)
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
    updateNumericInput(session, "binsize", value = binsize_r())
  })

  ##############################################################################
  ## Read LC/MS data onto R via XCMS
  ##############################################################################
  observeEvent(flist(), {
    v$fname <- tools::file_path_sans_ext(flist()$name)
    withProgress(message = "Reading Data...", value = 0, {
      v$raw <- readMSData(
        flist()$datapath,
        pdata = new(
          "NAnnotatedDataFrame",
          data.frame(idx = seq_len(nrow(input$upload)),
                     fname = factor(v$fname, levels = v$fname))
        ),
        msLevel. = 1, mode = "onDisk"
      )
      n <- nrow(flist())
      dl <- list()
      for (i in 1:n) {
        dl[[i]] <- get_df(filterFile(v$raw, i))
        incProgress(1/n, detail = paste0("File ", i))
      }
      v$fdata <- rbindlist(dl)
      v$fdata[, file := factor(file, levels = v$fname)]
    })
    output$standard <- renderUI(
      tagList(
        bs_embed_popover(
          fileInput("standard_info", "Provide internal standard information",
                    multiple = FALSE, accept = list(".csv")),
          paste0(
            "Input must be a csv file with the following columns: ",
            "compound, adduct, mode (positive or negative), and mz ",
            "(e.g., Lactate, [M+H]+, positive, 94.04903)"
          )
        ),
        div(style = "margin-top: -20px"),
        checkboxInput("standard_skip", "Skip and specify m/z manually")
      )
    )
    observeEvent({
      input$standard_info
      input$standard_skip
    }, {
      if (isTruthy(input$standard_info) || input$standard_skip) {
        if (is.null(input$standard_info)) {
          hide("upload")
          hide("standard_info")
          hide("standard_skip")
          output$featuredetection <- renderUI(
            tagList(
              featuredetection_ui(v$compound_dat, input$standard_skip)
            )
          )
          output$tabs <- renderUI(
            maintabs_ui(v$fdata, input$standard_skip)
          )
        } else {
          v$compound_dat <- fread(input$standard_info$datapath, sep = ",")
          if (has_all_columns(v$compound_dat)) {
            v$compound_dat[, id := paste(compound, adduct, sep = " ")]
            hide("upload")
            hide("standard_info")
            hide("standard_skip")
            output$featuredetection <- renderUI(
              tagList(
                featuredetection_ui(v$compound_dat)
              )
            )
            output$tabs <- renderUI(
              maintabs_ui(v$fdata)
            )
            d <- v$compound_dat[, -c("id")]
            setnames(d, old = "mz", new = "m/z")
            setcolorder(d, c("compound", "adduct", "mode", "m/z"))
            output$standard_tbl <- renderDT(
              datatable(
                d,
                selection = "none",
                extensions = "Buttons",
                options = list(
                  dom = "Bfrtip", buttons = c('copy', 'csv', 'excel')
                )
              )
            )
          } else {
            showNotification(
              paste0("Input must have the following columns: ",
                     "compound, adduct, mode, and mz. ",
                     "Please re-upload a valid file."),
              duration = 5, type = "error", closeButton = FALSE
            )
          }
        }
      }
    })
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
          p_tic(xs, type = type, facet = facet)
        })
      }
    })
  })

  ##############################################################################
  ## XIC plot
  ##############################################################################
  observeEvent(v$fdata, {
    output$xic <- renderPlotly(NULL) ## hide spinner when no request for XIC
    observeEvent(input$plot_xic, {
      updateTabsetPanel(session, "tabs", selected = "Extracted Ion Chromatogram")
      updatePickerInput(
        session, "xic_files",
        selected = unique(as.character(v$fdata$file))
      )
      observeEvent(input$xic_files, {
        rtrange <- rtr()
        if (is.infinite(rtrange[1])) {
          rtrange[1] <- 0
        }
        if (is.infinite(rtrange[2])) {
          rtrange[2] <- max(v$fdata$rt) + 20
        }
        ## Assign figure and render separately so that figure only respond to
        ## XIC button (don't know why?)
        xic <- p_xic_list(
          v$fdata[file %in% input$xic_files],
          mzrange = mzr(),
          rtrange = rtrange,
          fname = input$xic_files
        )
        output$xic <- renderPlotly(xic)
      })
    })
  })

  ##############################################################################
  ## Mass spectrum plot
  ##############################################################################
  observeEvent(input$massspec_file, {
    output$massspec <- renderPlotly(NULL) ## hide spinner
    rt_max <- ceiling(max(v$fdata[file == input$massspec_file]$rt) / 10) * 10
    output$masschrom_slider <- renderUI(tagList(
      br(),
      sliderInput(
        "masschrom_rt", "Retention Time Range",
        min = 0, max = rt_max, value = c(0, rt_max), step = 30
      )
    ))
    v$mass_dat <- v$fdata[file == input$massspec_file]
    if (!is.null(v$mass_dat)) {
      output$masschrom <- renderPlotly({
        v$p_mass_chrom <- tryCatch(
          p_mass_chrom(v$mass_dat, rtrange = input$masschrom_rt),
          warning = function(w) NULL,
          error = function(e) NULL
        )
      })
    }
    observeEvent({
      req(v$p_mass_chrom)
      event_data("plotly_click", source = "mass_chrom")
      v$mass_dat
    }, {
      d <- event_data("plotly_click", source = "mass_chrom")
      output$massspec <- renderPlotly({
        tryCatch(
          p_massspec(
            v$mass_dat,
            scan = d$x[1], yaxis = input$yaxis
          ),
          warning = function(w) NULL,
          error = function(e) NULL
        )
      })
    })
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
    showModal(modalDialog("Detecting features...", footer = NULL, size = "l"))
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
          datatable(
            feature_tbl[, c(1, 4)],
            selection = list(mode = "single", selected = 1, target = "row"),
            extensions = "Buttons",
            options = list(
              dom = "Bfrtip", buttons = c('copy', 'csv', 'excel')
            )
          )
        )
      }
    }
    removeModal()
  })

  ##############################################################################
  ## Feature and peak mapping and peak visualization
  ##############################################################################
  observeEvent(input$feature_tbl_rows_selected, {
    idx <- input$feature_tbl_rows_selected

    ## Bar plot for feature intensities
    output$feature_fig <- renderUI(tagList(
      pickerInput(
        "feature_files", "Files",
        choices = unique(as.character(v$fdata$file)),
        selected = unique(as.character(v$fdata$file)),
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      splitLayout(
        checkboxInput("log2", "Log2 Scale"),
        checkboxInput("show_val", "Show Values")
      ),
      withSpinner(plotlyOutput("feature_bar"))
    ))
    observeEvent(input$feature_files, {
      dw <- v$feature[idx, -c(1:11)]
      dl <- melt(dw, measure.vars = colnames(dw),
                 variable.name = "File", value.name = "maxo")
      dl[, File := factor(File, levels = v$fname)]
      output$feature_bar <- renderPlotly({
        p_feature_area(
          dl[File %in% input$feature_files],
          title,
          input$log2,
          input$show_val
        )
      })
    })

    ## Feature-peak table
    output$peak_tbl <- renderUI(tagList(
      h5("Peaks for the Selected Feature", style = "color:orange"),
      br(),
      DTOutput("feature_peak_map")
    ))
    peaklist <- as.data.table(v$peak)[v$feature$peakidx[[idx]], ]
    peak_sub <- merge(peaklist, pData(v$raw), by.x = "sample",
                      by.y = "idx", sort = FALSE)
    peak_tbl <- copy(peak_sub)
    mz_cols <- c("mz", "mzmin", "mzmax")
    rt_cols <- c("rt", "rtmin", "rtmax")
    peak_tbl[, (mz_cols) := lapply(.SD, function(x) sprintf("%.4f", x)),
             .SDcols = mz_cols]
    peak_tbl[, (rt_cols) := lapply(.SD, function(x) sprintf("%.3f", x)),
             .SDcols = rt_cols]
    peak_tbl[, area := sprintf("%.2f", into)]
    keep_cols <- c("fname", mz_cols, rt_cols, "area")
    peak_tbl <- peak_tbl[, ..keep_cols]
    setnames(peak_tbl, new = c("file", "m/z", "m/z min", "m/z max",
                               "time", "time min", "time max", "area"))
    output$feature_peak_map <- renderDT(
      datatable(
        peak_tbl,
        selection = "none",
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip", buttons = c('copy', 'csv', 'excel')
        )
      )
    )

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
