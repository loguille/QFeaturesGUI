#' Log transform tab (section) ui builder
#'
#' @param id The module id
#' @return A shiny tagList object that contains the Log Transform tab UI components
#' @rdname INTERNAL_interface_module_log_transform_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow column NS actionButton icon uiOutput numericInput textOutput selectInput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList br div tags
#'
interface_module_log_transform_tab <- function(id) {
    tagList(
        fluidRow(
            box(
                title = "Settings",
                status = "primary",
                width = 3,
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(
                    inputId = NS(id, "log_base"),
                    label = "Log base",
                    choices = c("log2", "log10", "ln"),
                    selected = "log2"
                ),
                numericInput(
                    inputId = NS(id, "pseudocount"),
                    label = "Pseudocount",
                    value = 0,
                    min = 0,
                    step = 1
                ),
                br(),
                actionButton(
                    inputId = NS(id, "apply_log_transform"),
                    label = "Apply log transform",
                    width = "100%",
                    class = "load-button"
                ),
                br(), br(),
                tags$h4("Plot options"),
                selectInput(
                    inputId = NS(id, "color"),
                    label = "Color by",
                    choices = c("NULL"),
                    selected = "NULL"
                )
            ),
            box(
                title = "Density Plots",
                status = "primary",
                width = 9,
                solidHeader = TRUE,
                collapsible = TRUE,
                fluidRow(
                    column(
                        6,
                        tags$h4("Pre-log transform"),
                        with_output_waiter(
                            plotlyOutput(outputId = NS(id, "density_plot_pre")),
                            html = waiter::spin_6(),
                            color = "transparent"
                        )
                    ),
                    column(
                        6,
                        tags$h4("Post-log transform"),
                        div(
                            style = "text-align: center; font-size: 16px; color: #777;",
                            textOutput(NS(id, "post_density_message"))
                        ),
                        with_output_waiter(
                            plotlyOutput(outputId = NS(id, "density_plot_post")),
                            html = waiter::spin_6(),
                            color = "transparent"
                        )
                    )
                )
            )
        ),
        bs3Tooltip(
            actionButton(
                NS(id, "export"),
                "Save the processed sets",
                icon("hand-pointer", class = "fa-solid"),
                width = "100%",
                class = "load-button"
            ),
            paste(
                "Write the processed sets to the QFeatures object.",
                "This is needed to proceed to the next steps.",
                sep = " "
            ),
            placement = "top"
        )
    )
}
