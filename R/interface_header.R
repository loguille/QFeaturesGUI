#' ui header builder
#'
#' @param title a string that refers to the title of the app
#'
#' @return the dashboardHeader object for the different apps
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shinydashboard dropdownMenuOutput dropdownMenu
#' @importFrom shinydashboardPlus dashboardHeader notificationItem
#'
header <- function(title) {
    dashboardHeader(
        title = title,
        dropdownMenuOutput("exception_menu"),
        dropdownMenu(
          type = "notifications",
          badgeStatus = NULL,
          icon = icon("circle-question"),
          notificationItem(
            text = "Online Documentation",
            icon = icon("arrow-up-right-from-square"),
            status = "success",
            href = "https://uclouvain-cbio.github.io/QFeaturesGUI/")
        )
    )
}
