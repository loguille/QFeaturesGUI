#' Imputation tab (section) ui builder
#'
#' @param id The module id
#' @return A shiny tagList object that contains the imputation tab UI components
#' @rdname INTERNAL_interface_module_impute_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow column NS actionButton icon uiOutput textOutput selectInput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList br div tags
#' @importFrom shinyBS bsTooltip
#'
interface_module_impute_tab <- function(id) {
    tagList(
        fluidRow(
            box(
                title = "Settings",
                status = "primary",
                width = 3,
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(
                    inputId = NS(id, "method"),
                    label = "Imputation method",
                    choices = character(0)
                ),
                uiOutput(NS(id, "method_params")),
                br(),
                actionButton(
                    inputId = NS(id, "apply_impute"),
                    label = "Apply imputation",
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
                        tags$h4("Pre-imputation"),
                        with_output_waiter(
                            plotlyOutput(outputId = NS(id, "density_plot_pre")),
                            html = waiter::spin_6(),
                            color = "transparent"
                        )
                    ),
                    column(
                        6,
                        tags$h4("Post-imputation"),
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
        actionButton(
            NS(id, "export"),
            "Save the processed sets",
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
