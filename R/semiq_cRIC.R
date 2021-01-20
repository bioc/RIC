#' Enables the analysis of proteins with intensity values in at least one
#' condition.
#' 
#' \code{semiq_cRIC} returns a tabular data with one additional summary column
#'  per sample that contains the number of detectable intensity values.
#' 
#' @param aggregated_peptides Dataframe 
#' output from \code{\link{agregate_singlepeptides}}
#' @param condition Character,
#' Condition to perform semi-quantitative analysis on 
#' @param relative Character,
#' Condition to perform semi-quantitative analysis against 
#' @param updown Integer,
#' Threshold for FC to keep semi-quantitative values
#' @return Dataframe with semi-quantitative values 
#' @examples
#' if(interactive()){
#'  semiq_cRIC(aggregatedRIC,condition = "hour4",relative = "mock" ,updown=2 )
#' }
#' @export
semiq_cRIC <-
  function(aggregated_peptides,
           condition,
           relative,
           updown) {
    if (is.integer(updown)) {
      updown <- as.numeric(updown)
    }
    assertthat::assert_that(
      is.data.frame(aggregated_peptides),
      is.character(condition),
      is.character(relative),
      is.numeric(updown)
    )


    Intensities <- as.matrix(aggregated_peptides[, grepl("hour", colnames(aggregated_peptides)) |
      grepl("mock", colnames(aggregated_peptides))])
    cond <- sapply(
      strsplit(colnames(Intensities), split = "_"),
      function(x) {
        x[1]
      }
    )

    semi_condition_relative <- apply(
      is.na(Intensities[, cond == relative]) -
        is.na(Intensities[, cond == condition]), 1,
      function(x) {
        sum(x)
      }
    )
    if (updown > 0) {
      selection <- aggregated_peptides[semi_condition_relative >= updown, ]
    } else {
      selection <- aggregated_peptides[semi_condition_relative <= updown, ]
    }
    selection <- selection[, c(1, 2, 3, 6, 9, 12, 5, 8, 11, 4, 7, 10)] # this to sort mock, hour4 and hour18 properly. This doesnt look very solid code!


    add_summary <- function(proteinset) {
      hour18_index <- grep("18", colnames(proteinset))
      hour4_index <- grep("4", colnames(proteinset))
      mock_index <- grep("mock", colnames(proteinset))
      proteinset <- cbind(proteinset,
        hour18_total = apply(
          proteinset, 1,
          function(x) {
            sum(!is.na(x[hour18_index]))
          }
        )
      )
      proteinset <- cbind(proteinset,
        hour4_total = apply(
          proteinset, 1,
          function(x) {
            sum(!is.na(x[hour4_index]))
          }
        )
      )
      proteinset <- cbind(proteinset,
        mock_total = apply(
          proteinset, 1,
          function(x) {
            sum(!is.na(x[mock_index]))
          }
        )
      )
      proteinset
    }

    selection <- add_summary(selection)
    return(selection)
  }


