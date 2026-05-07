#' Server logic for the Log Transform tab
#'
#' @param id The module id
#' @param step_number The step number
#' @param step_rv Reactive value tracking whether this step is saved
#' @param parent_rv Reactive value for the previous step completion
#'
#' @return The server logic for the Log Transform tab
#' @rdname INTERNAL_server_module_log_transform_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer reactive renderUI observeEvent req renderText eventReactive updateSelectInput
#'
server_module_log_transform_tab <- function(id, step_number, step_rv, parent_rv) {
    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")

        step_ready <- reactive({
            if (!is.null(parent_rv)) req(parent_rv() > 0L)
            TRUE
        })

        parent_assays <- reactive({
            req(step_ready())
            error_handler(
                page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = .qf$qfeatures,
                pattern = pattern
            )
        })

        clicked <- reactiveVal(FALSE)
        observeEvent(input$apply_log_transform, {
            clicked(TRUE)
        })

        output$post_density_message <- renderText({
            if (!clicked()) {
                "The post-log transform plot will be displayed once you apply log transform."
            } else {
                ""
            }
        })

        processed_assays <- eventReactive(input$apply_log_transform, {
            req(parent_assays())
            req(is.finite(input$log_base), input$log_base > 0, input$log_base != 1)
            req(is.finite(input$pseudocount), input$pseudocount >= 0)
            with_task_loader(
                caption = "Applying log transform and generating post-log transform densities",
                expr = {
                    error_handler(
                        log_transform_qfeatures,
                        component_name = "Log transform",
                        qfeatures = parent_assays(),
                        base = input$log_base,
                        pseudocount = input$pseudocount
                    )
                }
            )
        })

        selected_color <- reactive({
            req(input$color)
            if (identical(input$color, "NULL")) {
                return(NULL)
            }
            input$color
        })

        output$density_plot_pre <- renderPlotly({
            req(parent_assays())
            error_handler(
                density_by_sample_plotly,
                component_name = "Pre-log transform density plot",
                qfeatures = parent_assays(),
                color = selected_color(),
                title = "Pre-log transform density"
            )
        })

        output$density_plot_post <- renderPlotly({
            req(processed_assays())
            error_handler(
                density_by_sample_plotly,
                component_name = "Post-log transform density plot",
                qfeatures = processed_assays(),
                color = selected_color(),
                title = "Post-log transform density"
            )
        })

        observe({
            req(parent_assays())
            choices <- c("NULL", colnames(colData(parent_assays())))
            selected <- "NULL"
            if (!is.null(input$color) && input$color %in% choices) {
                selected <- input$color
            }
            updateSelectInput(session,
                "color",
                choices = choices,
                selected = selected
            )
        })

        observeEvent(input$export, {
            req(processed_assays())
            with_task_loader(
                caption = "Saving sets in QFeatures object",
                expr = {
                    error_handler(
                        add_assays_to_global_rv,
                        component_name = "Add assays to global_rv",
                        processed_qfeatures = processed_assays(),
                        step_number = step_number,
                        type = "log_transform"
                    )

                    global_rv$code_lines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(
                        qf = .qf$qfeatures,
                        step_number = step_number
                    )
                    global_rv$code_lines[[paste0("log_transform_", step_number)]] <- codeGeneratorLogTransform(
                        base = input$log_base,
                        pseudocount = input$pseudocount,
                        step_number = step_number
                    )
                    step_rv(step_rv() + 1L)
                }
            )
        },
        ignoreInit = TRUE
        )
    })
}
