#' Server logic for the imputation tab
#'
#' @param id The module id
#' @param step_number The step number
#' @param step_rv Reactive value tracking whether this step is saved
#' @param parent_rv Reactive value for the previous step completion
#'
#' @return The server logic for the imputation tab
#' @rdname INTERNAL_server_module_impute_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer reactive reactiveVal observe observeEvent req renderText eventReactive updateSelectInput renderUI selectInput tags
#' @importFrom htmltools tagList p
#'
server_module_impute_tab <- function(id, step_number, step_rv, parent_rv) {
    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")
        method_specs <- imputation_method_specs()

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

        method_choices <- reactive({
            methods <- names(method_specs)
            req(length(methods) > 0L)
            methods
        })

        observe({
            choices <- method_choices()
            selected <- input$method
            if (is.null(selected) || !(selected %in% choices)) {
                selected <- if ("knn" %in% choices) "knn" else choices[[1]]
            }

            updateSelectInput(
                session = session,
                inputId = "method",
                choices = choices,
                selected = selected
            )
        })

        current_method_spec <- reactive({
            req(input$method %in% names(method_specs))
            method_specs[[input$method]]
        })

        output$method_documentation <- renderUI({
            choices <- method_choices()
            selected_method <- input$method

            tagList(lapply(choices, function(method_name) {
                spec <- method_specs[[method_name]]
                is_selected <- identical(method_name, selected_method)
                tags$div(
                    class = "list-group-item",
                    style = paste(
                        "margin-bottom:8px;",
                        if (is_selected) "border-color:#3c8dbc; background-color:#f5fbff;" else ""
                    ),
                    tags$div(
                        style = "font-weight:600;",
                        paste0(
                            method_name,
                            if (is_selected) " (selected)" else ""
                        )
                    ),
                    p(
                        style = "margin:4px 0;",
                        spec$description
                    ),
                    if (!is.null(spec$default_parameters) &&
                        length(spec$default_parameters) > 0L &&
                        any(nzchar(spec$default_parameters))) {
                        tags$div(
                            style = "font-size:90%;",
                            tags$strong("Default parameters: "),
                            tags$code(paste(spec$default_parameters, collapse = ", "))
                        )
                    }
                )
            }))
        })

        impute_args <- reactive({
            req(input$method)
            req(input$method %in% method_choices())
            args <- current_method_spec()$call_args
            if (is.null(args)) {
                return(list())
            }
            args
        })

        clicked <- reactiveVal(FALSE)
        observeEvent(input$apply_impute, {
            clicked(TRUE)
        })

        output$post_density_message <- renderText({
            if (!clicked()) {
                "The post-imputation plot will be displayed once you apply imputation."
            } else {
                ""
            }
        })

        processed_state <- eventReactive(input$apply_impute, {
            req(parent_assays(), input$method)
            req(input$method %in% method_choices())
            current_assays <- parent_assays()
            current_method <- input$method
            current_args <- impute_args()
            with_task_loader(
                caption = "Applying imputation and generating post-imputation densities",
                expr = {
                    current_qfeatures <- do.call(
                        error_handler,
                        c(
                            list(
                                func = impute_qfeatures,
                                component_name = "Imputation",
                                object = current_assays,
                                impute_method = current_method
                            ),
                            current_args
                        )
                    )

                    if (is.null(current_qfeatures)) {
                        return(NULL)
                    }

                    list(
                        qfeatures = current_qfeatures,
                        method = current_method,
                        args = current_args
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
                component_name = "Pre-imputation density plot",
                qfeatures = parent_assays(),
                color = selected_color(),
                title = "Pre-imputation density"
            )
        })

        output$density_plot_post <- renderPlotly({
            req(processed_state())
            error_handler(
                density_by_sample_plotly,
                component_name = "Post-imputation density plot",
                qfeatures = processed_state()$qfeatures,
                color = selected_color(),
                title = "Post-imputation density"
            )
        })

        observe({
            req(parent_assays())
            choices <- c("NULL", colnames(colData(parent_assays())))
            selected <- "NULL"
            if (!is.null(input$color) && input$color %in% choices) {
                selected <- input$color
            }
            updateSelectInput(
                session = session,
                inputId = "color",
                choices = choices,
                selected = selected
            )
        })

        observeEvent(input$export, {
            req(processed_state())
            applied_state <- processed_state()
            method <- applied_state$method
            with_task_loader(
                caption = "Saving sets in QFeatures object",
                expr = {
                    error_handler(
                        add_assays_to_global_rv,
                        component_name = "Add assays to global_rv",
                        processed_qfeatures = applied_state$qfeatures,
                        step_number = step_number,
                        type = "imputation"
                    )

                    global_rv$code_lines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(
                        qf = .qf$qfeatures,
                        step_number = step_number
                    )
                    global_rv$code_lines[[paste0("imputation_", step_number)]] <- codeGeneratorImpute(
                        method = method,
                        step_number = step_number
                    )
                    step_rv(step_rv() + 1L)
                }
            )
        }, ignoreInit = TRUE)
    })
}
