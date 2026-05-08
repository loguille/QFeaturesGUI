#' Server logic for the Zero to NA tab
#'
#' @param id The module id
#' @param step_number The step number
#' @param step_rv Reactive value tracking whether this step is saved
#' @param parent_rv Reactive value for the previous step completion
#'
#' @return The server logic for the Zero to NA tab
#' @rdname INTERNAL_server_module_zero_to_na_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer reactive renderUI observeEvent req
#' @importFrom htmltools p
#'
server_module_zero_to_na_tab <- function(id, step_number, step_rv, parent_rv) {
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

        output$zero_to_na_summary <- renderUI({
            req(parent_assays())
            n_sets <- length(parent_assays())
            p(
                paste0(
                    n_sets, " set", if (n_sets != 1L) "s" else "",
                    " will be processed in this step."
                )
            )
        })

        observeEvent(input$export, {
            req(parent_assays())
            assays_to_process <- parent_assays()
            with_task_loader(
                caption = "Replacing zeros with missing values and saving sets",
                expr = {
                    processed_assays <- error_handler(
                        QFeatures::zeroIsNA,
                        component_name = "Zero to NA",
                        object = assays_to_process,
                        i = seq_along(assays_to_process)
                    )
                    req(processed_assays)

                    error_handler(
                        add_assays_to_global_rv,
                        component_name = "Add assays to global_rv",
                        processed_qfeatures = processed_assays,
                        step_number = step_number,
                        type = "zero_to_na"
                    )

                    global_rv$code_lines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(
                        qf = .qf$qfeatures,
                        step_number = step_number
                    )
                    global_rv$code_lines[[paste0("zero_to_na_", step_number)]] <- codeGeneratorZeroToNA(
                        step_number = step_number
                    )
                    step_rv(step_rv() + 1L)
                }
            )
        }, ignoreInit = TRUE)
    })
}
