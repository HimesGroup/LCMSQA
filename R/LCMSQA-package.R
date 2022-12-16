##' @keywords internal
##' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel
##'   mainPanel uiOutput br fileInput h3 helpText hr conditionalPanel wellPanel
##'   selectizeInput actionButton h5 numericInput tabsetPanel tabPanel
##'   splitLayout fluidRow h4 reactive req showNotification reactiveValues
##'   isTruthy observe updateNumericInput updateSliderInput observeEvent
##'   withProgress incProgress renderUI div checkboxInput updateTabsetPanel
##'   updateSelectizeInput removeNotification showModal modalDialog removeModal
##' @importFrom shinyWidgets pickerInput updatePickerInput
##' @importFrom shinycssloaders withSpinner
##' @importFrom DT DTOutput renderDT datatable
##' @importFrom shinyjs useShinyjs toggleState hide
##' @importFrom data.table fread data.table as.data.table rbindlist setnames
##'   setcolorder melt copy :=
##' @importFrom bslib bs_theme font_google
##' @importFrom bsplus use_bs_tooltip bs_embed_popover bs_embed_tooltip
##' @importFrom ggplot2 ggplot aes theme theme_bw geom_line facet_wrap
##'   geom_linerange ylab geom_point scale_x_continuous scale_y_continuous
##'   scale_color_viridis_c geom_blank geom_col element_blank element_text
##'   geom_rect labs alpha geom_text position_stack
##' @importFrom plotly plotlyOutput ggplotly subplot layout renderPlotly
##' @importFrom xcms filterFile filterMz filterRt CentWaveParam
##'   findChromPeaks chromPeaks PeakDensityParam groupChromPeaks
##'   featureDefinitions featureValues
"_PACKAGE"
