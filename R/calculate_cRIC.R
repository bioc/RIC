#' Estimate RNA binding activity
#'  
#' \code{calculate_cRIC} calculates log2 (RIC/WCL) changes, which are used as a
#' proxy of RNA binding activity.
#'  
#' @param WCL Dataframe
#' output from \code{\link{agregagte_singlepeptides}}
#' @param RIC Dataframe
#' output from \code{\link{agregagte_singlepeptides}}
#' @return A data frame with log2(RIC/WCL) values
#' @examples
#' if(interactive()){
#' cRIC <- calculate_cRIC(aggregatedWCL_batch,aggregatedRIC)
#' }
#' @export
calculate_cRIC <- function(WCL, RIC) {
  assertthat::assert_that(
    is.data.frame(WCL),
    is.data.frame(RIC)
  )

  tomerge_by <- c("ENSGid", "symbol", "Know_RBP")

  if (any(!tomerge_by %in% colnames(WCL))) {
    stop(paste0(
      "'",
      paste0(tomerge_by, collapse = "' and/or '"),
      "' not found in '",
      deparse(substitute(WCL)),
      "'."
    ),
    call. = FALSE
    )
  }

  if (any(!tomerge_by %in% colnames(RIC))) {
    stop(paste0(
      "'",
      paste0(tomerge_by, collapse = "' and/or '"),
      "' not found in '",
      deparse(substitute(RIC)),
      "'."
    ),
    call. = FALSE
    )
  }


  # check dimensions too

  proteins_int <- merge(WCL, RIC,
    by = c("ENSGid", "symbol", "Know_RBP")
  )
  # Do this by name.
  proteins_int[, 4:12] <- proteins_int[, 13:21] - proteins_int[, 4:12]
  proteins_int <- proteins_int[, 1:12]
}
