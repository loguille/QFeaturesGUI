#' A UI module that return a box that contains the UI components
#'  to create an preview a QFeatures object
#'
#' @param id module id
#'
#' @return A shinydashboardPlus box object that contains the UI components to create an preview a QFeatures object
#' @rdname INTERNAL_box_readqfeatures_ui
#' @keywords internal
#'
#' @importFrom shiny tagList selectInput checkboxInput actionButton downloadButton NS
#' @importFrom shinydashboardPlus box
#' @importFrom shinydashboard infoBoxOutput
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs disabled hidden
#'
box_readqfeatures_ui <- function(id) {
    tagList(
        box(
            title = "QFeatures Converter",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            box(
                title = "Settings",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                id = NS(id, "parameters"),
                selectInput(
                    inputId = NS(id, "run_col"),
                    span(
                        bs3Tooltip(
                            trigger = "Run/Batch column :",
                            tooltipText = "For the multi-set case, the column assayData that contains\
                           the runs/batches.",
                            placement = "bottom"
                        )
                    ),
                    choices = NULL,
                    selected = "NULL"
                ),
                selectInput(
                    inputId = NS(id, "quant_cols"),
                    span(
                        bs3Tooltip(
                            trigger = "Quantitative column :",
                            tooltipText = "Only relevant without a colData table. The column(s) of the \
                            assayData that contain the quantitative data.",
                            placement = "bottom"
                        )
                    ),
                    choices = NULL,
                    multiple = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "removeEmptyCols"),
                    label = "Remove columns that contain only missing values",
                    value = FALSE
                ),
                checkboxInput(
                    inputId = NS(id, "logTransform"),
                    label = "Log transform data",
                    value = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "zero_as_NA"),
                    label = "Convert zeros to NA",
                    value = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "singlecell"),
                    label = "Single cell data",
                    value = FALSE
                ),
                disabled(
                    actionButton(
                        inputId = NS(id, "convert"),
                        "Convert to a QFeatures object",
                        class = "add-button no-bottom-margin",
                        width = "100%"
                    )
                )
            ),
            hidden(
                div(
                    id = NS(id, "qfeatures_preview_box"),
                    box(
                        title = "QFeatures Preview",
                        status = "primary",
                        width = 12,
                        solidHeader = FALSE,
                        collapsible = TRUE,
                        id = NS(id, "qfeatures_preview"),
                        infoBoxOutput(NS(id, "type_of_qfeatures")),
                        DT::dataTableOutput(NS(id, "qfeatures_dt"))
                    )
                )
            ),
            hidden(
                div(
                    id = NS(id, "selected_assay_preview_box"),
                    box(
                        title = "Selected Assay Preview",
                        status = "primary",
                        width = 12,
                        solidHeader = FALSE,
                        collapsible = TRUE,
                        id = NS(id, "assay_preview"),
                        DT::dataTableOutput(NS(id, "assay_table"))
                    )
                )
            ),
            hidden(
                div(
                    id = NS(id, "download_qfeatures_object"),
                    bs3Tooltip(
                        trigger = shiny::downloadButton(
                            outputId = NS(id, "downloadQFeatures"),
                            "Download QFeatures object",
                            class = "load-button",
                            style = "width: 100%;"
                        ),
                        tooltipText = "Download zip file containing QFeatures object, \
                           the script used to generate this object and the R sessionInfo\
                           containing package and version used for the script.",
                        placement = "top"
                    )
                )
            )
        )
    )
}
