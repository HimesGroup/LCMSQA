ui <- fluidPage(
  ## Enable js
  useShinyjs(),
  ## Enable tooltip
  use_bs_tooltip(),
  ## Set theme
  theme = bs_theme(
    bootswatch = "minty",
    heating_font = font_google("Roboto"),
    base_font = font_google("Roboto"),
    code_font = font_google("JetBrains Mono")
    ),
  titlePanel("LC/MS Quality Assessment"),
  ## File upload
  sidebarLayout(
    sidebarPanel(
      bs_embed_popover(
        fileInput(
          inputId = "upload",
          label = "Choose LC/MS files",
          multiple = TRUE
        ),
        paste0(
          "Input must be valid mass-spectrometry data files ",
          "in open format (mzML, mzData, mzXML, and netCDF)"
        )
      ),
      br(),
      uiOutput("standard"),
      uiOutput("featuredetection"),
      width = 4
    ),
    mainPanel(
      uiOutput("tabs")
    )
  )
)
