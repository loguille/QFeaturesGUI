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
#' @importFrom shiny moduleServer reactive reactiveVal observe observeEvent req renderText eventReactive updateSelectInput renderUI numericInput selectInput NS
#' @importFrom htmltools tagList
#'
server_module_impute_tab <- function(id, step_number, step_rv, parent_rv) {
    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")
        method_specs <- imputation_method_specs()
        methods_with_margin <- names(Filter(function(spec) {
            !is.null(spec$default_margin)
        }, method_specs))

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
                selected <- if ("bpca" %in% choices) "bpca" else choices[[1]]
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

        output$method_params <- renderUI({
            req(input$method)
            default_margin <- current_method_spec()$default_margin

            params <- list()
            if (input$method %in% methods_with_margin) {
                params <- c(params, list(selectInput(
                    inputId = NS(id, "margin"),
                    label = "Imputation margin",
                    choices = c("Features (rows)" = "1", "Samples (columns)" = "2"),
                    selected = default_margin
                )))
            }
            if (input$method %in% c("MinDet", "MinProb")) {
                params <- c(params, list(numericInput(
                    inputId = NS(id, "q"),
                    label = "Quantile (q)",
                    value = 0.01,
                    min = 0,
                    max = 1,
                    step = 0.01
                )))
            }
            if (input$method %in% c("MinProb", "QRILC")) {
                params <- c(params, list(numericInput(
                    inputId = NS(id, "sigma"),
                    label = "Sigma",
                    value = 1,
                    min = 0,
                    step = 0.1
                )))
            }
            if (input$method == "nbavg") {
                params <- c(params, list(numericInput(
                    inputId = NS(id, "k"),
                    label = "k (edge replacement value)",
                    value = 0,
                    step = 1
                )))
            }
            if (input$method == "with") {
                params <- c(params, list(numericInput(
                    inputId = NS(id, "val"),
                    label = "Replacement value",
                    value = 0,
                    step = 1
                )))
            }
            do.call(tagList, params)
        })

        impute_args <- reactive({
            req(input$method)
            req(input$method %in% method_choices())
            args <- list()
            if (input$method %in% methods_with_margin) {
                req(input$margin)
                args$MARGIN <- as.integer(input$margin)
            }
            if (input$method %in% c("MinDet", "MinProb")) {
                req(is.finite(input$q), input$q >= 0, input$q <= 1)
                args$q <- input$q
            }
            if (input$method %in% c("MinProb", "QRILC")) {
                req(is.finite(input$sigma), input$sigma > 0)
                args$sigma <- input$sigma
            }
            if (input$method == "nbavg") {
                req(is.finite(input$k))
                args$k <- input$k
            }
            if (input$method == "with") {
                req(is.finite(input$val))
                args$val <- input$val
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
                    current_qfeatures <- if (identical(current_method, "none")) {
                        current_assays
                    } else {
                        do.call(
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
                    }

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
            args <- applied_state$args
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
                        step_number = step_number,
                        margin = if (!is.null(args$MARGIN)) args$MARGIN else NULL,
                        q = if (!is.null(args$q)) args$q else NULL,
                        sigma = if (!is.null(args$sigma)) args$sigma else NULL,
                        k = if (!is.null(args$k)) args$k else NULL,
                        val = if (!is.null(args$val)) args$val else NULL
                    )
                    step_rv(step_rv() + 1L)
                }
            )
        }, ignoreInit = TRUE)
    })
}
