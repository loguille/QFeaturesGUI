#' @title A shiny app to import QFeatures objects.
#'
#' @description importQFeatures is a simple graphical interface to import bulk and single-cell proteomics data.
#' The app uses the \code{\link[QFeatures]{readQFeatures}} function from the QFeatures package to convert simple tables (single or multiple, CSV or TSV) to a QFeatures object.
#' The app allows users to convert tables to a QFeatures object.
#'
#' @param colData A data frame that contains the sample table.
#' @param assayData A data frame that contains the input table.
#' @param maxSize An integer that changes the shiny.maxRequestSize value, in MB.
#'
#' @return The "importQFeatures" Shiny app object.
#' @export
#' @importFrom shiny shinyApp runApp onStop
#'
#' @examples
#' library(QFeaturesGUI)
#'
#' data("sampleTable")
#' data("inputTable")
#' app <- importQFeatures(colData = sampleTable, assayData = inputTable, maxSize = 100)
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
importQFeatures <- function(colData = NULL, assayData = NULL, maxSize = 1000) {
    oldOptions <- options(shiny.maxRequestSize = maxSize * 1024^2)
    onStop(function() options(oldOptions))
    ui <- build_import_ui()
    server <- build_import_server(colData, assayData)
    shinyApp(ui = ui, server = server)
}
