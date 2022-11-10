library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(bsplus)
library(bslib)
library(DT)
library(data.table)
library(xcms)
library(ggplot2)
library(plotly)
library(viridisLite)
library(tools)
options(shiny.maxRequestSize = 1000 * (1024**2))
options(digits = 12)
source("plots.R")
source("feature_detection.R")
source("maintabs.R")

mandatory_fields_preset <- c(
  "xic_mz_window", "xic_rt_min", "xic_rt_max"
)

mandatory_fields_manual <- c(
  "xic_mz_min", "xic_mz_max", "xic_rt_min", "xic_rt_max"
)

has_spectra <- function(x) {
  all(MSnbase::hasSpectra(x))
}

test_mandatory <- function(input, mandatory_fields) {
  all(
    vapply(mandatory_fields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != "" &&
               !is.na(suppressWarnings(as.numeric(input[[x]])))
           },
           logical(1))
  )
}

get_df <- function(x) {
  ## `x` is supposed to be a single file MSnExp object
  is(x, "MSnExp")
  d <- as.data.frame(x)
  d$file <- pData(x)$fname
  d
}

get_mzrange <- function(mz, ppm = 30) {
  delta <- ppm * mz / 1e6
  c(mz - delta, mz + delta)
}

get_compound_mzrange <- function(compound, compound_dat, ppm) {
  compound_idx <- which(compound_dat$id == compound)
  mz <- compound_dat$mz[compound_idx]
  get_mzrange(mz, ppm)
}

compounds <- c(
  "Lactate", "Sodium Pyruvate", "Aspartic Acid", "Glutamic Acid",
  "KG", "HG", "Succinic Acid", "Citric Acid", "AcetylCoA-d2"
)

compound_dat <- data.table(
  id = c(paste0(compounds, " [M+H]+"),
         paste0(compounds, " [M-H]-")),
  mode = c(rep("positive", length(compounds)),
           rep("negative", length(compounds))),
  mz = c(94.04903, 92.0333, 139.0552, 154.0742,
         152.06018, 154.06122, 123.0473, 199.0544, 812.1397,
         92.0334, 90.0189, 137.0395, 152.0585,
         150.031, 152.0466, 121.0316, 197.0387, 810.1246)
)

machines <- c(
  "UPLC / Q-Exactive", "UPLC / Orbitrap",
  "HPLC / Waters TOF", "HPLC / Ion Trap"
)

machines_ppm <- list(5, 2.5, 30, 50)
machines_peakwidth <- list(c(5, 20), c(5, 20), c(10, 60), c(10, 60))
machines_snrth <- list(4, 10, 6, 6)
machines_mzdiff <- list(0.01, 0.01, 0.01, 0.01)
machines_noise <- list(0, 1000, 0, 0)
machines_pre_peak <- list(3, 3, 3, 3)
machines_pre_int <- list(100, 5000, 100, 100)
machines_bw <- list(5, 5, 5, 5)
machines_minfrac <- list(0.5, 0.5, 0.5, 0.5)
machines_binsize <- list(0.025, 0.015, 0.025, 0.05)

get_machine_val <- function(machine, machine_list, values) {
  names(values) <- machine_list
  values[[machine]]
  ## do.call(switch, c(machine, as.list(values)))
}
