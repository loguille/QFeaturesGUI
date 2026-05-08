#' Zero to NA tab (section) ui builder
#'
#' @param id The module id
#' @return A shiny tagList object that contains the Zero to NA tab UI components
#' @rdname INTERNAL_interface_module_zero_to_na_tab
#' @keywords internal
#'
#' @importFrom shiny NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools p tagList
#' @importFrom shinyBS bsTooltip
#'
interface_module_zero_to_na_tab <- function(id) {
    tagList(
        box(
            title = "Zero to NA",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            p(
                "Convert all zero intensities into missing values (NA) in the selected sets."
            ),
            uiOutput(NS(id, "zero_to_na_summary"))
        ),
        actionButton(
            NS(id, "export"),
            "Apply Zero to NA and save the processed sets",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "export"),
            title = paste(
                "Write the processed sets to the QFeatures object.",
                "This is needed to proceed to the next steps.",
                sep = " "
            ),
            trigger = "hover",
            placement = "top"
        )
    )
}
