find_msconvert <- function() {
  tryCatch({
    system2(
      "where", args = c("/r", "C:\\Users", "msconvert"), stdout = TRUE
    )
  },
  warning = function(w) {
    showNotification(
      "Can't find MSConvert. Please install and try again!" ,
      duration = 5, type = "error", closeButton = FALSE
    )
    message("Can't find MSConvert. Please install and try again!")
  },
  error = function(w) {
    showNotification(
      "Can't find MSConvert. Please install and try again!" ,
      duration = 5, type = "error", closeButton = FALSE
    )
    message("Can't find MSConvert. Please install and try again!")
  })
}

filter_peakpicking <- '"peakPicking vendor msLevel=1-"'
filter_titlemaker <- paste0(
  '"titleMaker <RunId>.<ScanNumber>.<ScanNumber>.<ChargeState> File:',
  '"""^<SourcePath^>""", NativeID:"""^<Id^>""""'
)

run_msconvert <- function(msconvert, file, out_dir) {
  system2(
    msconvert,
    args = c(
      file, "-o", out_dir, "--zlib",
      "--filter", filter_peakpicking,
      "--filter", filter_titlemaker
    )
  )
}
