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
  ## tags$head(
  ##        tags$style(
  ##               HTML("#shiny-notification-panel {
  ##                 top: 0;
  ##                 bottom: unset;
  ##                 left: 0;
  ##                 right: 0;
  ##                 margin-left: auto;
  ##                 margin-right: auto;
  ##                 width: 50%; max-width: 450px;}"
  ##               )
  ##             )
  ##      ),
  titlePanel("LC/MS Quality Assessment"),
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
