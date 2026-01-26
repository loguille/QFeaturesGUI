#' Server for the module aggregation tab
#'
#' @param id module id
#' @return The server logic for the aggregation tab
#' @rdname INTERNAL_server_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_aggregation_tab <- function(id, step_number, step_rv, parent_rv) {
    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")
        step_ready <- reactive({
            if (!is.null(parent_rv)) req(parent_rv() > 0L)
            TRUE
        })

        parent_assays <- reactive({
            req(step_ready())
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = .qf$qfeatures,
                pattern = pattern
            )
        })

        aggregation_names <- reactive({
            req(parent_assays())
            annotation_cols(parent_assays(), "rowData")
        })

        observe({
            updateSelectInput(
                inputId = "fcol",
                choices = as.character(aggregation_names())
            )
        })

        feature_choices <- reactive({
            req(parent_assays())
            req(input$fcol)
            as.character(unique(rbindRowData(parent_assays(), seq_along(parent_assays))[[input$fcol]]))
        })

        observe({
            req(parent_assays())
            updateSelectInput(session,
                "color",
                choices = c("NULL", colnames(colData(parent_assays())))
            )
        })
        clicked <- reactiveVal(FALSE)
        observeEvent(input$aggregate, {
            clicked(TRUE)

            choices <- feature_choices()
            selected_feature <- input$features
            if (length(choices) == 0L) {
                selected_feature <- character(0)
            } else if (is.null(selected_feature) ||
                length(selected_feature) == 0L ||
                !(selected_feature %in% choices)) {
                selected_feature <- choices[[1]]
            }
            updateSelectizeInput(
                session = session,
                inputId = "features",
                choices = choices,
                selected = selected_feature,
                server = TRUE
            )
        })

        output$aggregation_boxplot_ui <- renderUI({
            if (!clicked()) {
                return(NULL)
            }
            interface_module_feature_levels_boxplot(
                NS(id, "aggregation_boxplot")
            )
        })

        output$pre_boxplot <- renderText({
            if (!clicked()) {
                "The graph will be displayed after aggregation."
            } else {}
        })


        aggregation_parameters <- eventReactive(input$aggregate, {
            req(input$method)
            req(input$fcol)
            list(
                method = as.character(input$method),
                fcol = input$fcol
            )
        })

        processed_assays <- eventReactive(input$aggregate, {
            req(parent_assays())
            params <- aggregation_parameters()
            error_handler(
                aggregation_qfeatures,
                component_name = "aggregation",
                qfeatures = parent_assays(),
                method = params$method,
                fcol = params$fcol
            )
        })

        server_module_boxplot_box(
            id = "aggregation_boxplot",
            qf = parent_assays,
            qf_aggregate = processed_assays,
            aggregateBy = reactive(aggregation_parameters()$fcol),
            feature = reactive(input$features),
            color = reactive(input$color),
            showPoints = reactive(input$addPoints)
        )


        observeEvent(input$export, {
            req(processed_assays())
            params <- aggregation_parameters()
            with_task_loader(
                caption = "Saving sets in QFeatures object",
                expr = {
                    error_handler(
                        add_assays_to_global_rv,
                        component_name = "Add assays to global_rv",
                        processed_qfeatures = processed_assays(),
                        step_number = step_number,
                        type = "aggregation",
                        varTo = params$fcol,
                        varFrom = params$fcol
                    )
                    global_rv$code_lines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(qf = .qf$qfeatures, step_number = step_number)
                    global_rv$code_lines[[paste0("aggregation_", step_number)]] <- codeGeneratorAggregation(method = params$method, fcol = params$fcol, step_number = step_number)
                    step_rv(step_rv() + 1L)
                }
            )
        })
    })
}

#' Server for the module boxplot box
#'
#' @param id module id
#' @param qf qfeatures object before aggregation
#' @param qf_aggregate qfeatures object after aggregation
#' @param aggregateBy data to aggregate QFeatures object on
#' @param feature feature to plot
#' @param color variable to use for the color
#' @param showPoints logical for hide/show points on boxplot
#' @return a boxplot
#' @rdname INTERNAL_server_module_boxplot_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#' @importFrom QFeatures aggregateFeatures
#' @importFrom stats na.exclude
#' @importFrom tidyr gather
#'

server_module_boxplot_box <- function(id, qf, qf_aggregate, aggregateBy, feature, color, showPoints) {
    moduleServer(id, function(input, output, session) {
        plot_data <- reactive({
            req(qf())
            req(qf_aggregate())
            req(aggregateBy())
            req(feature())
            req(color())

            qf_current <- qf()
            qf_aggregate_current <- qf_aggregate()
            aggregate_by <- aggregateBy()
            selected_feature <- feature()
            selected_color <- color()

            df_qf_list <- list()
            df_qf_aggregate_list <- list()
            for (i in names(qf_current)) {
                set_qf <- qf_current[[i]][rowData(qf_current[[i]])[[aggregate_by]] == selected_feature, ]
                set_qf_aggregate <- qf_aggregate_current[[i]][rowData(qf_aggregate_current[[i]])[[aggregate_by]] == selected_feature, ]
                if (selected_color == "NULL") {
                    df_qf_list[[i]] <- assay(set_qf) |>
                        as.data.frame() |>
                        rownames_to_column(var = "aggregation") |>
                        tidyr::gather(sample, intensity, -aggregation) |>
                        na.exclude()
                    df_qf_aggregate_list[[i]] <- assay(set_qf_aggregate) |>
                        as.data.frame() |>
                        rownames_to_column(var = "aggregation") |>
                        tidyr::gather(sample, intensity, -aggregation) |>
                        na.exclude()
                } else {
                    df_qf_list[[i]] <- assay(set_qf) |>
                        as.data.frame() |>
                        rownames_to_column(var = "aggregation") |>
                        tidyr::gather(sample, intensity, -aggregation) |>
                        mutate(condition = as.vector(colData(qf_current)[sample, selected_color])) |>
                        na.exclude()
                    df_qf_aggregate_list[[i]] <- assay(set_qf_aggregate) |>
                        as.data.frame() |>
                        rownames_to_column(var = "aggregation") |>
                        tidyr::gather(sample, intensity, -aggregation) |>
                        mutate(condition = as.vector(colData(qf_aggregate_current)[sample, selected_color])) |>
                        na.exclude()
                }
            }

            df_qf <- dplyr::bind_rows(df_qf_list)
            df_qf_aggregate <- dplyr::bind_rows(df_qf_aggregate_list)
            data_final <- dplyr::bind_rows(df_qf, df_qf_aggregate)

            data_final$aggregation <- factor(data_final$aggregation, levels = unique(data_final$aggregation))
            data_final
        })

        output$boxplot <- renderPlotly({
            req(plot_data())
            error_handler(
                create_boxplot,
                component_name = "boxplot generation",
                data = plot_data(),
                color = color(),
                showPoints = showPoints()
            )
        })
    })
}

#' Function that generate a boxplot
#'
#' @param data a dataset containing intensity values pre and post aggregation
#' @param color variable to use for the color
#' @param showPoints logical for hide/show points on boxplot
#' @return a boxplot
#' @rdname INTERNAL_create_boxplot
#' @keywords internal
#'
#' @importFrom plotly plotly_build
#'

create_boxplot <- function(data, color, showPoints) {
    if (showPoints) {
        boxpoints <- "all"
        jitter <- 0.3
        pointpos <- 0
    } else {
        boxpoints <- "suspectedoutliers"
        jitter <- 0
        pointpos <- 0
    }
    p <- plot_ly(
        data = data,
        x = ~aggregation,
        y = ~intensity,
        color = if (color == "NULL") NULL else ~condition,
        text = ~sample,
        type = "box",
        boxpoints = boxpoints,
        jitter = jitter,
        pointpos = pointpos,
        hoveron = if (showPoints) "points" else "boxes",
        hovertemplate = paste0(
            "<b>%{text}</b><br>",
            "<extra></extra>"
        )
    ) %>%
        layout(boxmode = "group")
    plotly_build(p)
}
