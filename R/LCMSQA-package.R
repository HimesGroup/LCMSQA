##' LC/MS Quality Assessment
##'
##' @description
##'
##' The 'LCMSQA' package is designed to assess the quality of liquid
##' chromatography/mass spectrometry (LC/MS) experiment using a user-friendly
##' web application built with the R package 'shiny'. It utilizes the R package
##' 'xcms' workflow for data import, visualization, and quality check of LC/MS
##' experiments.
##'
##' The application consists of four main tabs:
##'
##' - Total Ion Chromatogram (and Base Peak Chromatogram)
##' - Extracted Ion Chromatogram (XIC)
##' - Mass Spectrum
##' - Metabolic Feature Detection
##'
##' Please check the vignette for the details (Run \code{vignette("LCMSQA",
##' package = "LCMSQA")}).
##'
##' @details
##' The application needs the following inputs:
##'
##' - (required) mass-spectrometry data files of quality control (QC) samples in
##' open formats: AIA/ANDI NetCDF, mzXML, mzData and mzML.
##'
##' - (optional) internal standard information in a CSV format with the columns:
##'   + compound: the name of compound
##'   + adduct: adduct type (e.g., \[M+H\]+)
##'   + mode: must be either "positive" or "negative"
##'   + mz: a known mass-to-charge ratio (m/z) value
##'
##' In the application UI, a user can tune the following parameters:
##'
##' - Set m/z and retention time of interest
##'   + compound (or m/z) with a ppm tolerance
##'   + retention time in second (min, max)
##' - Peak picking using the centWave method (see xcms::CentWaveParam)
##'   + ppm: the maximal tolerated m/z deviation in consecutive scans in ppm for
##'   the initial region of interest (ROI) definition
##'   + peak width: the expected approximate peak width in chromatographic space
##'   + signal/noise cut: the signal to noise ratio cutoff
##'   + m/z diff: the minimum difference in m/z dimension required for peaks
##'   with overlapping retention times
##'   + noise: a minimum intensity required for centroids to be considered in
##'   the first analysis step
##'   + prefilter (>= peaks, >= intensity): the prefilter step for the first
##'   analysis step (ROI detection)
##'   + Gaussian fit: whether or not a Gaussian should be fitted to each peak
##'   + m/z center: the function to calculate the m/z center of the
##'   chromatographic peaks
##'   + integration: whether or not peak limits are found through descent on the
##'   Mexican Hat filtered data
##' - Peak grouping using the peak density method (see xcms::PeakDensityParam)
##'   + bandwidth: the bandwidth (standard deviation of the smoothing kernel) to
##'   be used
##'   + min fraction: the minimum fraction of samples in which the peaks has to
##'   be detected to define a peak group
##'   + bin size: the size of overlapping slices in m/z dimension
##'
##' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel
##'   mainPanel uiOutput br fileInput h3 helpText hr conditionalPanel wellPanel
##'   selectizeInput actionButton h5 numericInput tabsetPanel tabPanel
##'   splitLayout fluidRow h4 reactive req showNotification reactiveValues
##'   isTruthy observe updateNumericInput updateSliderInput observeEvent
##'   withProgress incProgress renderUI div checkboxInput updateTabsetPanel
##'   updateSelectizeInput removeNotification showModal modalDialog removeModal
##'   column textInput sliderInput selectInput tags tagList
##' @importFrom shinyWidgets pickerInput updatePickerInput
##' @importFrom shinycssloaders withSpinner
##' @importFrom DT DTOutput renderDT datatable
##' @importFrom shinyjs useShinyjs toggleState hide
##' @importFrom data.table fread data.table as.data.table rbindlist setnames
##'   setcolorder melt copy :=
##' @importFrom bsplus use_bs_tooltip bs_embed_popover bs_embed_tooltip
##' @importFrom ggplot2 ggplot aes theme theme_bw geom_line facet_wrap
##'   geom_linerange ylab geom_point scale_x_continuous scale_y_continuous
##'   scale_color_viridis_c geom_blank geom_col element_blank element_text
##'   geom_rect labs alpha geom_text position_stack
##' @importFrom plotly plotlyOutput ggplotly subplot layout renderPlotly
##'   event_register event_data
##' @importFrom methods is new
"_PACKAGE"

