featuredetection_ui <- function(compound_dat, standard_skip = FALSE) {
  fluidRow(
    column(
      12,
      h3("Feature Detection"),
      helpText("Parameters for LC/MS feature detection"),
      hr(),
      ),
    column(
      12,
      h5("1. m/z and retention time of interest", style = "color:orange"),
      if (!standard_skip) {
        checkboxInput("manual", "set m/z manually", value = FALSE)
      }
    ),
    if (standard_skip) {
      column(
        6,
        wellPanel(
          "m/z  (\u00b1 ppm)",
          fluidRow(
            column(
              9,
              textInput("xic_mz_val", "", value = NULL, placeholder = "m/z")
            ),
            column(
              3,
              textInput("xic_mz_err", "", value = 10, placeholder = "ppm")
            )
          )
        )
      )
    } else {
      column(
        6,
        conditionalPanel(
          condition = "input.manual == 1",
          wellPanel(
            "m/z  (\u00b1 ppm)",
            fluidRow(
              column(
                9,
                textInput("xic_mz_val", "", value = NULL, placeholder = "m/z")
              ),
              column(
                3,
                textInput("xic_mz_err", "", value = 10, placeholder = "ppm")
              )
            )
          ),
          br()
        ),
        conditionalPanel(
          condition = "input.manual == 0",
          wellPanel(
            "compound (\u00b1 ppm)",
            fluidRow(
              column(
                9,
                selectizeInput(
                  "compound", "",
                  choices = list(
                    `Positive Mode` = compound_dat[mode == "positive"]$id,
                    `Negative Mode` = compound_dat[mode == "negative"]$id
                  ),
                  selected = compound_dat$id[1]
                )
              ),
              column(
                3,
                textInput("xic_mz_window", "", value = 10, placeholder = "ppm")
              )
            ),
            style = "padding-bottom: 8px"
          ),
          br()
        )
      )
    },
    column(
      6,
      wellPanel(
        "time",
        splitLayout(
          textInput("xic_rt_min", "", value = 0, placeholder = "min"),
          textInput("xic_rt_max", "", value = Inf, placeholder = "max")
        )
      ),
      br()
    ),
    column(
      12, align = "right",
      actionButton("plot_xic", "Generate XIC")
    ),
    column(
      12,
      h5("2. Peak Picking", style = "color:orange"),
      helpText("Chromatographic peak detection using the",
               tags$a("centWave",
                      href="https://sneumann.github.io/xcms/reference/findChromPeaks-centWave.html",
                      target="_blank"),
               "method"),
      br(),
      br()
    ),
    column(
      12, align = "left",
      selectizeInput("machine", "machine preset", choices = machines)
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("ppm", "ppm", value = 5),
        "Maximal tolerated m/z deviation in consecutive scans in ppm"
      )
    ),
    column(
      8,
      bs_embed_tooltip(
        sliderInput("peakwidth", "peak width", min = 0, max = 100, step = 1,
                    value = c(5, 20)),
        "Expected peak width in chromatographic space"
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("snthr", "signal/noise cut", value = 5),
        "Signal to noise ratio cutoff"
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("mzdiff", "m/z diff", value = 0.01),
        paste0(
          "Minium difference in m/z dimension required for peaks ",
          "with overlapping retention times"
        )
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("noise", "noise", value = 0),
        paste0(
          "Minimum intensity required for centroids to be considered ",
          "in the first analysis step of the centWave"
        )
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("pre_peak", "prefilter: peaks", 3)
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("pre_int", "preilter: intensity", 100),
        "Minimum intensity to keep mass traces for the ROI detection of centWave"
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        selectInput("gauss", "Gaussian fit", choices = c("False", "True"),
                    selected = "False"),
        "Whether or not a Gaussian should be fitted to each peak"
      )
    ),
    column(
      6,
      bs_embed_tooltip(
        selectizeInput("mzcenter", "m/z center",
                       choices = c("wMean", "mean", "apex",
                                   "wMeanApex3", "meanApex3")),
        "Function to calculate the m/z center of the chromatographic peak"
      )
    ),
    column(
      6,
      bs_embed_tooltip(
        selectizeInput("integrate", "integration",
                       choices = c("Mexican Hat", "Real")),
        "Integration method"
      )
    ),
    column(
      12,
      h5("3. Peak Grouping", style = "color:orange"),
      helpText("Chromatographic peak grouping using the ",
               tags$a("peak density",
                      href="https://sneumann.github.io/xcms/reference/do_groupChromPeaks_density.html",
                      target="_blank"),
               "method"),
      br(),
      br()
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("bw", "bandwidth", value = 5),
        "Standard deviation of the smoothing kernel for peak grouping"
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("minfrac", "min fraction", value = 0.5),
        paste0("Minimum faction of samples in which the peak have ",
               "to be present to be considered as a peak group")
      )
    ),
    column(
      4,
      bs_embed_tooltip(
        numericInput("binsize", "bin size", value = 0.025),
        "Size of the overlapping slices in m/z dimension"
      )
    ),
    column(
      12, align = "right",
      actionButton("detect_feature", "Detect Features")
    )
  )
}
