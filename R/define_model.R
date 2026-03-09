#' @rdname define-model
#' @export
define_model.data.frame = function(.x, to_analyze, ...) {
    processed = process_model_id(to_analyze, data = .x)

    model_id = if (inherits(to_analyze, "formula")) {
        out = list(formula = processed$formula)
        class(out) = "formula"
        out
    } else {
        to_analyze
    }

    out = list(
        model_id  = model_id,
        test_spec = NULL,
        options = vctrs::vec_c(...),
        processed = processed
    )
    class(out) = "model_to_analyze"
    out
}

#' @rdname define-model
#' @export
define_model.infer_model = function(.x, data = NULL, ...) {
    processed = if (!is.null(data)) {
        process_model_id(.x, data = data)
    } else {
        process_model_id_global(.x)
    }

    model_id = if (inherits(.x, "formula")) {
        out = list(formula = processed$formula)
        class(out) = "formula"
        out
    } else {
        .x
    }

    out = list(
        model_id = model_id,
        processed = processed,
        test_spec = NULL,
        options = vctrs::vec_c(...)
    )
    class(out) = "model_to_analyze"
    out
}

#' @rdname define-model
#' @export
rel = function(x, resp, ...) {
    xs = rlang::enquo(x)
    resps = rlang::enquo(resp)

    out = list(
        args = list(x = xs, resp = resps),
        data = NULL
    )
    class(out) = write_class("rel")
    out
}

#' @rdname define-model
#' @export
compare_by = function(x, grp, ...) {
    xs = rlang::enquo(x)
    grps = rlang::enquo(grp)

    out = list(
        args = list(x = xs, grp = grps),
        data = NULL
    )
    class(out) = write_class("compare_by")
    out
}

#' @rdname define-model
#' @export
cont_tab = function(ind, dep, ...) {
    inds = rlang::enquo(ind)
    deps = rlang::enquo(dep)

    out = list(
        args = list(ind = inds, dep = deps),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("cont_tab")
    out
}

#' @rdname define-model
#' @export
prop_model = function(x, ...) {
    xs = rlang::enquo(x)

    out = list(
        args = list(x = xs),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("prop_model")
    out
}

#' @rdname define-model
#' @export
pairwise = function(..., .dir = "leq") {
    dots = rlang::enquos(...)

    out = list(
        args = list(dots = rlang::expr(c(!!!dots))),
        data = NULL,
        formula = NULL,
        .dir = .dir
    )
    class(out) = write_class("pairwise")
    out
}

#' @rdname define-model
#' @export
selected_vars = function(...) {
    dots = rlang::enquos(...)

    out = list(
        args = list(dots = rlang::expr(c(!!!dots))),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("selected_vars")
    out
}

#' @rdname define-model
#' @export
one_sample = function(var, ...) {
    vs = rlang::enquo(var)

    out = list(
        args = list(var = vs),
        data = NULL,
        formula = NULL
    )
    class(out) = write_class("one_sample")
    out
}

#' @export
model_id = function(clss, arg_list, data = NULL, formula = NULL) {
    out = list(
        args = arg_list,
        data = data,
        formula = formula
    )
    class(out) = write_class(clss)
    out
}

#' Extract the model_id from a pipeline object
#'
#' @param .x A `model_to_analyze` or `test_lazy` object.
#' @export
get_model_id = function(.x) {
    .x$model_id
}

#' Validates and extracts data for a given model_id against a data frame.
#' Returns a list with:
#'   -  $data — subsetted data frame containing only the relevant columns
#'   -  $vars — named character vector of resolved column names
#'
#' Errors immediately if any referenced column is missing.
#' @keywords internal
#' @noRd
process_model_id = function(model_id, data) {
    # ---- Process current model ----
    UseMethod("process_model_id")
}

process_model_id.formula = function(model_id, data) {
    # ---- Formula ----
    list(
        data = data,
        vars = NULL,
        formula = model_id
    )
}

process_model_id.rel = function(model_id, data) {
    # ---- "Relation" ----
    x_names = names(tidyselect::eval_select(model_id$args$x, data))
    resp_names = names(tidyselect::eval_select(model_id$args$resp, data))

    all_names = c(x_names, resp_names)
    .check_cols(data, all_names)

    list(
        data = data[, all_names, drop = FALSE],
        vars = list(x = x_names, resp = resp_names)
    )
}

process_model_id.compare_by = function(model_id, data) {
    # ---- "Comparison" ----
    x_names = names(tidyselect::eval_select(model_id$args$x, data))
    grp_names = names(tidyselect::eval_select(model_id$args$grp, data))

    all_names = c(x_names, grp_names)
    .check_cols(data, all_names)

    list(
        data = data[, all_names, drop = FALSE],
        vars = list(x = x_names, grp = grp_names)
    )
}

process_model_id.cont_tab = function(model_id, data) {
    # ---- Cont. Table ----
    ind_name = rlang::as_name(model_id$args$ind)
    dep_name = rlang::as_name(model_id$args$dep)

    .check_cols(data, c(ind_name, dep_name))

    list(
        data = data[, c(ind_name, dep_name), drop = FALSE],
        mat = table(
            data[[ind_name]],
            data[[dep_name]],
            dnn = c(ind_name, dep_name)
        ),
        vars = c(ind = ind_name, dep = dep_name)
    )
}

process_model_id.prop_model = function(model_id, data) {
    # ---- Prop. Model ----
    var_name = rlang::as_name(model_id$args$x)

    .check_cols(data, var_name)

    list(
        data = data[, var_name, drop = FALSE],
        vars = c(x = var_name)
    )
}

process_model_id.pairwise = function(model_id, data) {
    # ---- Pairwise ----
    sel = tidyselect::eval_select(model_id$args$dots, data)
    var_names = names(sel)
    .dir = model_id$.dir

    pairs = all_pairs(var_names, direction = .dir)

    list(
        data = vctrs::vec_slice(data, seq_len(nrow(data)))[var_names],
        vars = var_names,
        .dir = .dir,
        pairs = pairs
    )
}

process_model_id.selected_vars = function(model_id, data) {
    # ---- Independent Vars ----
    sel = tidyselect::eval_select(model_id$args$dots, data)
    var_names = names(sel)

    list(
        data = data[, var_names, drop = FALSE],
        vars = var_names
    )
}

process_model_id.one_sample = function(model_id, data) {
    # ---- Only 1 Var ----
    var_name = rlang::as_name(model_id$args$var)

    .check_cols(data, var_name)

    list(
        data = data[, var_name, drop = FALSE],
        vars = c(var = var_name)
    )
}

process_model_id.infer_model = function(model_id, data) {
    cli::cli_abort(c(
        "No {.fn process_model_id} method for model type {.cls {class(model_id)[[1]]}}.",
        "i" = "This model type is not yet supported in {.fn define_model}."
    ))
}

#' Processing model in the current environment
#'
#' When the variables of the model "to be analyzed" is not supplied with
#' data, it will look up at the current environment
#'
#' @keywords internal
#' @noRd
process_model_id_global = function(model_id) {
    # ---- Process model globally ----
    UseMethod("process_model_id_global")
}

process_model_id_global.rel = function(model_id) {
    # ---- "Relation" ----
    x = rlang::eval_tidy(model_id$args$x)
    resp = rlang::eval_tidy(model_id$args$resp)

    data = data.frame(x = x, resp = resp)
    names(data) = c(
        rlang::as_label(model_id$args$x),
        rlang::as_label(model_id$args$resp)
    )

    list(
        data = data,
        vars = list(x = names(data)[[1]], resp = names(data)[[2]])
    )
}

process_model_id_global.formula = function(model_id) {
    # ---- Formula ----
    data = environment(model_id)

    list(
        data = data,
        vars = NULL,
        formula = model_id
    )
}

process_model_id_global.cont_tab = function(model_id) {
    # ---- Cont. Table ----
    ind = rlang::eval_tidy(model_id$args$ind)
    dep = rlang::eval_tidy(model_id$args$dep)

    ind_label = rlang::as_label(model_id$args$ind)
    dep_label = rlang::as_label(model_id$args$dep)

    data = data.frame(ind, dep)
    names(data) = c(ind_label, dep_label)

    list(
        data = data,
        mat = table(ind, dep, dnn = c(ind_label, dep_label)),
        vars = c(x = names(data)[[1]], resp = names(data)[[2]])
    )
}

process_model_id_global.infer_model = function(model_id) {
    cli::cli_abort(c(
        "No {.fn process_model_id_global} method for model type {.cls {class(model_id)[[1]]}}.",
        "i" = "This model type is not yet supported without a data frame."
    ))
}

.check_cols = function(data, cols) {
    missing = cols[!cols %in% names(data)]
    if (length(missing) > 0L) {
        cli::cli_abort(c(
            "Column{?s} not found in data: {.val {missing}}.",
            "i" = "Available columns: {.val {names(data)}}"
        ))
    }
    invisible(NULL)
}

#' @keywords internal
#' @export
print.model_to_analyze = function(x, ...) {
    model_id  = x$model_id
    processed = x$processed
    model_cls = class(model_id)[[1]]

    cli::cli_text(cli::style_bold("Model to Analyze"))
    cli::cat_line(cli::rule(line = "-"))

    cli::cli_text("{.field Type}  : {.cls {model_cls}}")

    switch(
        model_cls,
        rel = {
            cli::cli_text("{.field x}     : {.val {processed$vars$x}}")
            cli::cli_text("{.field resp}  : {.val {processed$vars$resp}}")
        },
        cont_tab = {
            cli::cli_text("{.field ind}   : {.val {processed$vars[['ind']]}}")
            cli::cli_text("{.field dep}   : {.val {processed$vars[['dep']]}}")
        },
        prop_model = {
            cli::cli_text("{.field x}     : {.val {processed$vars[['x']]}}")
        },
        pairwise = {
            cli::cli_text("{.field vars}  : {.val {processed$vars}}")
            cli::cli_text("{.field pairs} : {length(processed$pairs)} pair{?s}")
        },
        selected_vars = {
            cli::cli_text("{.field vars}  : {.val {processed$vars}}")
        },
        one_sample = {
            cli::cli_text("{.field var}   : {.val {processed$vars[['var']]}}")
        },
        formula = {
            cli::cli_text("{.field formula}: {.code {deparse(processed$formula)}}")
        },
        {
            nms = setdiff(names(processed), "data")
            for (nm in nms)
                cli::cli_text("{.field {nm}} : {.val {processed[[nm]]}}")
        }
    )

    if (!is.null(processed$data)) {
        dims = dim(processed$data)
        if (!is.null(dims))
            cli::cli_text("{.field data}  : {dims[[1]]} row{?s} x {dims[[2]]} col{?s}")
    }

    invisible(x)
}

#' @keywords internal
#' @export
print.infer_model = function(x, ...) {
    model_cls = class(x)[[1]]

    cli::cli_text(cli::style_bold("Model ID"))
    cli::cat_line(cli::rule(line = "-"))
    cli::cli_text("{.field Type} : {.cls {model_cls}}")

    switch(
        model_cls,
        rel = {
            cli::cli_text("{.field x}      : {.code {rlang::as_label(x$args$x)}}")
            cli::cli_text("{.field resp}   : {.code {rlang::as_label(x$args$resp)}}")
        },
        cont_tab = {
            cli::cli_text("{.field ind}    : {.code {rlang::as_label(x$args$ind)}}")
            cli::cli_text("{.field dep}    : {.code {rlang::as_label(x$args$dep)}}")
        },
        prop_model = {
            cli::cli_text("{.field x}      : {.code {rlang::as_label(x$args$x)}}")
        },
        pairwise = {
            vars = vapply(rlang::call_args(x$args$dots), rlang::as_label, character(1))
            cli::cli_text("{.field vars}   : {.code {vars}}")
            cli::cli_text("{.field dir}    : {.val {x$.dir}}")
        },
        selected_vars = {
            vars = vapply(rlang::call_args(x$args$dots), rlang::as_label, character(1))
            cli::cli_text("{.field vars}   : {.code {vars}}")
        },
        one_sample = {
            cli::cli_text("{.field var}    : {.code {rlang::as_label(x$args$var)}}")
        },
        {
            # fallback for user-defined model types
            nms = setdiff(names(x$args), "dots")
            for (nm in nms)
                cli::cli_text("{.field {nm}} : {.code {rlang::as_label(x$args[[nm]])}}")
        }
    )

    invisible(x)
}
