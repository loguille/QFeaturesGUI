#' ui header builder
#'
#' @param title a string that refers to the title of the app
#'
#' @return the dashboardHeader object for the different apps
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shinydashboardPlus dashboardHeader
#'
header <- function(title) {
        dashboardHeader(
            title = title,
            dropdownMenuOutput("exception_menu"),
            tags$li(
                class = "dropdown",
                bs3Tooltip(
                    trigger = tags$a(
                        href = "https://uclouvain-cbio.github.io/QFeaturesGUI/",
                        target = "_blank",
                        rel = "noopener noreferrer",
                        style = "padding-top: 15px; padding-bottom: 15px;",
                        icon("arrow-up-right-from-square"),
                        "Documentation"
                    ),
                    tooltipText = "Open online documentation",
                    placement = "bottom"
                )
            )
        )
}
