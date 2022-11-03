ui <- fluidPage(
  ## Enable js
  shinyjs::useShinyjs(),
  ## Enable tooltip
  bsplus::use_bs_tooltip(),
  ## Set theme
  theme = bs_theme(
    bootswatch = "minty",
    heating_font = font_google("Roboto"),
    base_font = font_google("Roboto"),
    code_font = font_google("JetBrains Mono")
    ),
  titlePanel("LC/MS QA"),
  ## File upload
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "upload",
        label = "Choose LC/MS files",
        multiple = TRUE
      ),
      textOutput("text1"),
      uiOutput("peakpicking"),
      width = 4
    ),
    mainPanel(
      uiOutput("tabs")
    )
  )
)
