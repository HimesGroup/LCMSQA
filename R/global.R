################################################################################
## Validate mass-spectrometry files
################################################################################
has_spectra <- function(x) {
  all(MSnbase::hasSpectra(x))
}

################################################################################
## Validate standard info file
################################################################################
has_all_columns <- function(x) {
  df_name <- names(x)
  diff_cols <- setdiff(c("compound", "adduct", "mode", "mz"), df_name)
  if (length(diff_cols) == 0) TRUE else FALSE
}

################################################################################
## Mandatory fields to activate XIC plot and feature detection buttons
################################################################################
mandatory_fields_preset <- c(
  "xic_mz_window", "xic_rt_min", "xic_rt_max"
)

mandatory_fields_manual <- c(
  "xic_mz_val", "xic_mz_err", "xic_rt_min", "xic_rt_max"
)

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

################################################################################
## Retrieve data from the XCMS object
################################################################################
get_df <- function(x) {
  ## `x` is supposed to be a single file MSnExp object
  is(x, "MSnExp")
  d <- as.data.frame(x)
  d$file <- MSnbase::pData(x)$fname
  d
}

################################################################################
## Set m/z searching space from compound information
################################################################################
get_mzrange <- function(mz, ppm = 30) {
  delta <- ppm * mz / 1e6
  c(mz - delta, mz + delta)
}

get_compound_mzrange <- function(compound, compound_dat, ppm) {
  compound_idx <- which(compound_dat$id == compound)
  mz <- compound_dat$mz[compound_idx]
  get_mzrange(mz, ppm)
}


################################################################################
## Machine-specific feature detection parameters
################################################################################
machines <- c("UPLC / Orbitrap", "HPLC / Ion Trap")

machines_ppm <- list(5, 50)
machines_peakwidth <- list(c(5, 20),c(10, 60))
machines_snthr <- list(10, 6)
machines_mzdiff <- list(0.01, 0.01)
machines_noise <- list(1000, 0)
machines_pre_peak <- list(3, 3)
machines_pre_int <- list(5000, 100)
machines_bw <- list(5, 5)
machines_minfrac <- list(0.5, 0.5)
machines_binsize <- list(0.015, 0.05)

get_machine_val <- function(machine, machine_list, values) {
  names(values) <- machine_list
  values[[machine]]
  ## do.call(switch, c(machine, as.list(values)))
}


################################################################################
## Define global variables due to non-standard evaluation
################################################################################
utils::globalVariables(
         c(".", "..keep_cols", ".SD", "Area", "Chromatogram", "File",
           "Intensity", "Retention Time", "adduct", "area", "compound",
           "fname", "i", "id", "into", "label", "m/z", "maxo", "mz", "mz_label",
           "mzmax", "mzmin", "mzmed", "rt", "rtmax", "rtmin", "rtmed", "title",
           "xmax", "xmin", "ymax", "ymin")
       )


################################################################################
## BioC dependencies check
################################################################################
.verify_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required. Please install and try again.")
  }
}
