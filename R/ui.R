ui <- fluidPage(
  ## Enable js
  useShinyjs(),
  ## Enable tooltip
  use_bs_tooltip(),
  ## Set theme
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
