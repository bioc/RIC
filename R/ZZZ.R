.onLoad <- function(libname = find.package("RBP"), pkgname = "RBP"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c( #globalVariables
        "ID", ".", "condition", "label",
        "name", "columns", "columns_positions",
        "CI.L", "CI.R", "P.Value", "variable", "temp",
        "bin","rowname","x","y","p.value"
      )
    )
  invisible()
}
