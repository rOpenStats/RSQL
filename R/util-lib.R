#' genLogger
#' @param r6.object a R6 object with a logger member
#' @import lgr
#' @author ken4rab
#' @export
genLogger <- function(r6.object) {
  lgr::get_logger(class(r6.object)[[1]])
}

#' getLogger
#' @param r6.object a R6 object with a logger member
#' @import lgr
#' @author ken4rab
#' @export
getLogger <- function(r6.object) {
  ret <- r6.object$logger
  if (is.null(ret)) {
    class <- class(r6.object)[[1]]
    stop(paste("Class", class, "don't seems to have a configured logger"))
  } else {
    ret.class <- class(ret)[[1]]
    if (ret.class == "logical") {
      stop(paste("Class", ret.class, "needs to initialize logger: self$logger <- genLogger(self)"))
    }
  }
  ret
}

#' loggerSetupFile
#' @param log.file filename to log on
#' @param default.threshold default threshold for logger
#' @import lgr
#' @author kenarab
#' @export
loggerSetupFile <- function(log.file, default.threshold = "info") {
  unlink(lgr$appenders$file$file)  # cleanup
  lgr::basic_config()
  lgr::get_logger("root")$add_appender(AppenderFile$new(log.file,
                                                        layout = LayoutFormat$new(
                                                          fmt = "%L [%t] %m %j",
                                                          timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
                                                          colors = NULL,
                                                          pad_levels = "right"
                                                        )
  ))
  lgr::threshold(default.threshold, lgr::get_logger("root"))
  lgr
}
