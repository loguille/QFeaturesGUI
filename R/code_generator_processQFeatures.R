#' @title Code generator that prints the set names for the different steps
#' @param qf QFeatures object
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorInitialization
#' @keywords internal
#'

codeGeneratorInitialization <- function(qf, step_number) {
    vec <- names(qf)
    if (step_number == 1) {
        initial_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#0"), vec)]
        initial_setNames <- remove_QFeaturesGUI(initial_setNames)
        step_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
        step_setNames <- remove_QFeaturesGUI(step_setNames)
        codeLines <- sprintf(
            "####################################
######### initial set names ########
####################################
step0_setNames <- c(%s)\n
####################################
####### Step number %s names ########
####################################
step%s_setNames <- c(%s)\n",
            paste(sprintf('"%s"', initial_setNames), collapse = ", \n\t"),
            step_number,
            step_number,
            paste(sprintf('"%s"', step_setNames), collapse = ", \n\t")
        )
    } else {
        step_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
        step_setNames <- remove_QFeaturesGUI(step_setNames)
        codeLines <- sprintf(
            "####################################
####### Step number %s names ########
####################################
step%s_setNames<- c(%s)\n",
            step_number,
            step_number,
            paste(sprintf('"%s"', step_setNames), collapse = ", \n\t")
        )
    }
    codeLines
}

#' @title Check for missing set
#' @param qf QFeatures object
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_check_for_missing_set
#' @keywords internal
#'

check_for_missing_set <- function(qf, step_number) {
    vec <- names(qf)
    indice_to_remove <- c()
    initial_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number - 1), vec)]
    initial <- gsub("_\\(QFeaturesGUI#[0-9]+\\)_*[a-z]*_*[a-z]*_*[0-9]*", "", initial_setNames)
    currentStep_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
    current <- gsub("_\\(QFeaturesGUI#[0-9]+\\)_*[a-z]*_*[a-z]*_*[0-9]*", "", currentStep_setNames)
    if (length(initial) != length(current)) {
        for (i in seq_along(initial)) {
            if (!(initial[i] %in% current)) {
                indice_to_remove <- append(indice_to_remove, i)
            }
        }
        initial_setNames <- initial_setNames[-indice_to_remove]
        initial_setNames <- remove_QFeaturesGUI(initial_setNames)
        codeLines <- sprintf(
            "##After filtering steps one or more set has been deleted.\nstep%s_setNames <- c(%s)\n",
            step_number - 1,
            paste(sprintf('"%s"', initial_setNames), collapse = ", \n\t")
        )
        return(codeLines)
    }
}


#' @title Code generator for aggregation tab
#' @param method the method used to do the aggregation
#' @param fcol `character(1)` naming a `rowData` variable that defines how to aggregate
#' the features within each assay. This variable is either a character or a (possibly
#' sparse) matrix.
#' @param step_number The step number
#'
#' @return code line generated
#' @rdname INTERNAL_codeGeneratorAggregation
#' @keywords internal
#'

codeGeneratorAggregation <- function(method, fcol, step_number) {
    codeLines <- sprintf(
        "####################################
########### Aggregation ############
####################################
qf <- aggregateFeatures(qf,
\ti = step%s_setNames,
\tname = step%s_setNames,
\tfun = %s,
\tfcol = '%s',
\tna.rm = TRUE
)
for (i in 1:length(step%s_setNames)){
\tassayData <- assay(qf[[step%s_setNames[i]]])
\tassayData[is.nan(assayData)] <- NA_real_
\tassay(qf[[step%s_setNames[i]]]) <- assayData
}\n",
        step_number - 1,
        step_number,
        method,
        fcol,
        step_number,
        step_number,
        step_number
    )
    codeLines
}

#' @title Code generator for join tab
#' @param step_number The step number
#'
#' @return code line generated
#' @rdname INTERNAL_codeGeneratorJoin
#' @keywords internal
#'

codeGeneratorJoin <- function(step_number) {
    codeLines <- sprintf(
        "####################################
############### Join ###############
####################################
qf <- joinAssays(
\tx = qf,
\ti = step%s_setNames,
\tname = step%s_setNames
)\n",
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for filtering missing values tab
#' @param pNA `float` threshold for filtering missing value
#' @param type feature or sample
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorNA
#' @keywords internal
#'

codeGeneratorNA <- function(qf, pNA, type, step_number) {
    codeLines <- check_for_missing_set(qf, step_number = step_number)
    if (type == "features") {
        codeLines <- c(codeLines, sprintf(
            "####################################
###### Missing value features ######
####################################
for (i in 1:length(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- filterNA(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tpNA = %s
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}",
            step_number - 1,
            step_number,
            step_number - 1,
            pNA,
            step_number - 1,
            step_number
        ))
    } else {
        codeLines <- c(codeLines, sprintf(
            "####################################
###### Missing value samples #######
####################################
for(i in 1:length(step%s_setNames)){
  tableNA <- nNA(
    object = qf,
    i = step%s_setNames[i]
  )
  tableMetadata <- colData(qf[[step%s_setNames[i]]])
  tableMetadata$pNA <- tableNA$nNAcols$pNA[match(rownames(tableMetadata), tableNA$nNAcols$name)]
  qf[[step%s_setNames[i]]] <- qf[[step%s_setNames[i]]][, tableMetadata$pNA <= %s]
  qf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}",
            step_number - 1,
            step_number - 1,
            step_number - 1,
            step_number,
            step_number - 1,
            pNA,
            step_number - 1,
            step_number
        ))
    }
    codeLines
}

#' @title Code generator for normalisation tab
#' @param method the method used to do the normalisation
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorNormalisation
#' @keywords internal
#'

codeGeneratorNormalisation <- function(method, step_number) {
    codeLines <- sprintf(
        "####################################
########## Normalisation ###########
####################################
for(i in 1:length(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- normalize(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tmethod = '%s'
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        method,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for Zero to NA tab
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorZeroToNA
#' @keywords internal
#'
codeGeneratorZeroToNA <- function(step_number) {
    codeLines <- sprintf(
        "####################################
############ Zero to NA ############
####################################
for(i in 1:length(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- zeroIsNA(
\t\tobject = qf[[step%s_setNames[i]]]
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for log transform tab
#' @param base the logarithm base used during transformation
#' @param pseudocount pseudocount added before log transformation
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorLogTransform
#' @keywords internal
#'
codeGeneratorLogTransform <- function(base, pseudocount, step_number) {
    codeLines <- sprintf(
        "####################################
########## Log Transform ###########
####################################
for(i in 1:length(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- logTransform(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tbase = %s,
\t\tpc = %s
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        base,
        pseudocount,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for imputation tab
#' @param method imputation method used by `impute`
#' @param step_number The step number
#' @param margin optional imputation margin argument
#' @param q optional quantile argument for `MinDet`/`MinProb`
#' @param sigma optional sigma argument for `MinProb`/`QRILC`
#' @param k optional k argument for `nbavg`
#' @param val optional replacement value argument for `with`
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorImpute
#' @keywords internal
#'
codeGeneratorImpute <- function(method, step_number, margin = NULL, q = NULL, sigma = NULL, k = NULL, val = NULL) {
    if (identical(method, "none")) {
        return(
            "####################################
############ Imputation ############
####################################
## No imputation applied\n"
        )
    }

    arg_lines <- c(paste0("\t\tmethod = '", method, "'"))
    if (!is.null(margin)) {
        arg_lines <- c(arg_lines, paste0("\t\tMARGIN = ", as.integer(margin)))
    }
    if (!is.null(q)) {
        arg_lines <- c(arg_lines, paste0("\t\tq = ", q))
    }
    if (!is.null(sigma)) {
        arg_lines <- c(arg_lines, paste0("\t\tsigma = ", sigma))
    }
    if (!is.null(k)) {
        arg_lines <- c(arg_lines, paste0("\t\tk = ", k))
    }
    if (!is.null(val)) {
        arg_lines <- c(arg_lines, paste0("\t\tval = ", val))
    }
    args_block <- paste(arg_lines, collapse = ",\n")

    codeLines <- sprintf(
        "####################################
############ Imputation ############
####################################
for(i in 1:length(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- impute(
\t\tobject = qf[[step%s_setNames[i]]],
%s
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        args_block,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for filtering tab
#' @param qf QFeatures object
#' @param condition A list of filtering condition specifications
#' @param type feature or sample
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorFiltering
#' @keywords internal
#'

codeGeneratorFiltering <- function(qf, condition, type, step_number) {
    codeLines <- check_for_missing_set(qf, step_number = step_number)
    as_r_string_literal <- function(x) {
        encodeString(as.character(x), quote = "\"")
    }
    as_r_vector_literal <- function(values) {
        if (is.numeric(values[[1]])) {
            paste0("c(", paste(values, collapse = ","), ")")
        } else {
            escaped_values <- vapply(values, as_r_string_literal, character(1))
            paste0("c(", paste(escaped_values, collapse = ","), ")")
        }
    }
    if (length(condition) == 0) {
        codeLines <- c(codeLines, sprintf(
            "####################################
######## %s filtering ########
####################################
## No %s filtering applied\n",
            type,
            type
        ))
    } else {
        if (type == "features") {
            final <- "se <- se["
            for (i in 1:length(condition)) {
                annotation <- as_r_string_literal(condition[[i]]$annotation)
                if (condition[[i]]$annotation == ".qfeaturesgui_rowname") {
                    if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("rownames(rowData(se)) %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else {
                        build_condition <- paste0("!(rownames(rowData(se)) %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    }
                } else {
                    if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("rowData(se)[[", annotation, "]] %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else if (condition[[i]]$operator == "!=") {
                        build_condition <- paste0("!(rowData(se)[[", annotation, "]] %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    } else {
                        build_condition <- paste0("rowData(se)[[", annotation, "]] ", condition[[i]]$operator, " ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    }
                }
                build_condition <- paste0(build_condition, vector)
                if (i == 1) {
                    final <- paste0(final, build_condition)
                } else {
                    final <- paste0(final, " & ", build_condition)
                }
            }
            condition_used <- paste0(final, ",]")
            codeLines <- c(codeLines, sprintf(
                "####################################
######## features filtering ########
####################################
for(i in 1:length(step%s_setNames)){
\tse <- getWithColData(qf, step%s_setNames[i])
\t%s
\tqf[[step%s_setNames[i]]] <- se
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
                step_number - 1,
                step_number - 1,
                condition_used,
                step_number,
                step_number - 1,
                step_number
            ))
        } else {
            final <- "se <- se[,"
            for (i in 1:length(condition)) {
                annotation <- as_r_string_literal(condition[[i]]$annotation)
                if (condition[[i]]$annotation == ".qfeaturesgui_rowname") {
                    if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("rownames(colData(se)) %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else {
                        build_condition <- paste0("!(rownames(colData(se)) %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    }
                } else {
                    if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("colData(se)[[", annotation, "]] %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else if (condition[[i]]$operator == "!=") {
                        build_condition <- paste0("!(colData(se)[[", annotation, "]] %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    } else {
                        build_condition <- paste0("colData(se)[[", annotation, "]] ", condition[[i]]$operator, " ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    }
                }
                build_condition <- paste0(build_condition, vector)
                if (i == 1) {
                    final <- paste0(final, build_condition)
                } else {
                    final <- paste0(final, " & ", build_condition)
                }
            }
            condition_used <- paste0(final, "]")
            codeLines <- c(codeLines, sprintf(
                "####################################
######## samples filtering #########
####################################
for(i in 1:length(step%s_setNames)){
\tse <- getWithColData(qf, step%s_setNames[i])
\t%s
\tqf[[step%s_setNames[i]]] <- se
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
                step_number - 1,
                step_number - 1,
                condition_used,
                step_number,
                step_number - 1,
                step_number
            ))
        }
    }
    codeLines
}
