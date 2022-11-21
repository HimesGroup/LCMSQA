maintabs_ui <- function(fdata) {
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Total Ion Current",
      br(),
      pickerInput(
        "tic_files", "Files",
        choices = unique(as.character(fdata$file)),
        selected = unique(as.character(fdata$file)),
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      splitLayout(
        checkboxInput("collapse", "Collapse"),
        checkboxInput("bpc", "Show Base Peak Chromatogram"),
        cellWidths = 300,
        cellArgs = list(style = "padding-left: 10px")
      ),
      withSpinner(plotlyOutput("tic"))
    ),
    tabPanel(
      "Mass Spectrum",
      br(),
      fluidRow(
        selectizeInput(
          "massspec_file", "File", choices = unique(as.character(fdata$file)),
          selected = unique(as.character(fdata$file))[1]
        ),
        numericInput("ms_int_cut", "Intensity Threshold", value = 10000)
      ),
      fluidRow(
        selectizeInput("scan", "Scan Time", choices = NULL),
        selectizeInput("yaxis", "Y-axis",
                       choices = c("Relative Abundance", "Absolute Intensity"),
                       selected = "Relative Abundance")
      ),
      withSpinner(plotlyOutput("massspec"))
    ),
    tabPanel(
      "Extracted Ion Chromatogram",
      br(),
      pickerInput(
        "xic_files", "Files",
        choices = unique(as.character(fdata$file)),
        selected = unique(as.character(fdata$file)),
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      withSpinner(plotlyOutput("xic"))
    ),
    tabPanel(
      "Feature Detection",
      br(),
      h4("Feature Definitions"),
      br(),
      splitLayout(
        DTOutput("feature_tbl"),
        uiOutput("feature_fig"),
        cellArgs = list(style = "padding: 10px")
      ),
      uiOutput("peak_tbl"),
      uiOutput("peak_fig")
    )
  )
}

