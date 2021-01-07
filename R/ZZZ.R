.onLoad <- function(libname = find.package("RIC"), pkgname = "RIC"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c( #globalVariables
        "ID", ".", "condition", "label",
        "name", "columns", "columns_positions",
        "CI.L", "CI.R", "P.Value", "variable", "temp",
        "bin","rowname","x","y","p.value","condition",
        "assay","na.omit","t.test","colData","plot"
      )
    )
  invisible()
}
