#' @rdname prepare-test
#' @export
prepare_test.model_to_analyze = function(.x, .test, ...) {
    test_spec = as_test_spec(.test, model_id = .x$model_id)

    out = list(
        model_id = .x$model_id,
        processed = .x$processed,
        test_spec = test_spec,
        options = utils::modifyList(.x$options, list(...))
    )
    class(out) = "test_lazy"
    out
}

#' @export
update.test_lazy = function(object, ...) {
    object$test_spec$args = utils::modifyList(object$test_spec$args, list(...))
    object
}

#' @rdname prepare-test
#' @export
resample.test_lazy = function(.x, .method, ...) {
    dots = list(...)

    registered = Find(
        function(e)
            inherits(e$method, "method_replicate") &&
            identical(e$method$method_name, .method),
        .x$test_spec$impl
    )

    if (is.null(registered)) {
        cli::cli_abort(c(
            "No resample implementation for method {.val {(.method)}}.",
            "i" = "Registered methods: {.val {.get_replicate_method_names(.x$test_spec$impl)}}"
        ))
    }

    valid_args = registered$method$args$args %||% list()
    valid_names = vapply(valid_args, `[[`, character(1), "name")
    test_names = names(.x$test_spec$args)
    all_valid = c(valid_names, test_names)
    unknown = setdiff(names(dots), all_valid)

    if (length(unknown) > 0L) {
        cli::cli_abort(c(
            "Unknown argument(s) for method {.val {(.method)}}.",
            "x" = "Unknown: {.val {unknown}}",
            "i" = "Valid method arguments: {.val {valid_names}}",
            "i" = "Valid test arguments  : {.val {test_names}}"
        ))
    }

    supplied_method = dots[names(dots) %in% valid_names]
    supplied_test = dots[names(dots) %in% test_names]

    final_args = lapply(valid_args, function(arg) {
        if (arg$name %in% names(dots)) {
            supplied_method[[arg$name]]
        } else {
            arg$default
        }
    })
    names(final_args) = valid_names

    if (length(supplied_test) > 0L)
        .x$test_spec$args = utils::modifyList(.x$test_spec$args, supplied_test)

    .x$replicate_spec = list(
        method_name = .method,
        args = final_args
    )
    .x
}

.get_replicate_method_names = function(impl_list) {
    methods = Filter(function(e) inherits(e$method, "method_replicate"), impl_list)
    vapply(methods, function(e) e$method$method_name, character(1))
}

#' Normalise a test input into a test_spec
#'
#' Accepts a test expressed as an already-built `test_spec`, a bare function
#' (e.g. `TTEST`), or a function name string (e.g. `"TTEST"`), and always
#' returns a `test_spec` with `model_id` carried inside. Used internally by
#' [prepare_test()] and [run_test.model_to_analyze()].
#'
#' Note: the function or string path always calls the test function with
#' `model_id = NULL` to get a bare `test_spec`. The `model_id` is then
#' injected separately so that `build_htest()` does not attempt to execute
#' the test prematurely.
#'
#' @param .test A `test_spec`, a function, or a length-1 character string.
#' @param model_id A model ID object from [rel()], [cont_tab()], etc.
#'
#' @return A `test_spec` object.
#'
#' @seealso [prepare_test()], [run_test()]
#' @keywords internal
#' @noRd
as_test_spec = function(.test, model_id) {
    spec = if (inherits(.test, "test_spec")) {
        .test
    } else if (is.function(.test)) {
        .test(.model = NULL)
    } else if (is.character(.test) && length(.test) == 1L) {
        fn = tryCatch(
            get(.test, mode = "function", envir = parent.frame()),
            error = function(e) cli::cli_abort("Could not find test function {.val {.test}}.")
        )
        fn(.model = NULL)
    } else {
        cli::cli_abort(c(
            "{.arg .test} must be a {.cls test_spec}, a function, or a string.",
            "i" = "Example: {.code prepare_test(TTEST)} or {.code prepare_test('TTEST')}"
        ))
    }

    spec$model_id = model_id
    spec
}

#' @export
print.test_lazy = function(x, ...) {
    model_id  = x$model_id
    processed = x$processed
    test_spec = x$test_spec
    model_cls = class(model_id)[[1]]

    cli::cli_text(cli::style_bold("Lazy Test"))
    cli::cat_line(cli::rule(line = "-"))

    # ---- Model ----
    cli::cat_line(cli::style_bold("Model"))
    cli::cat_line(cli::style_italic(paste0("  Type  : ", model_cls)))

    switch(
        model_cls,
        rel = {
            cli::cat_line(paste0("  x     : ", paste(processed$vars$x,    collapse = ", ")))
            cli::cat_line(paste0("  resp  : ", paste(processed$vars$resp, collapse = ", ")))
        },
        cont_tab = {
            cli::cat_line(paste0("  ind   : ", processed$vars[["ind"]]))
            cli::cat_line(paste0("  dep   : ", processed$vars[["dep"]]))
        },
        prop_model = {
            cli::cat_line(paste0("  x     : ", processed$vars[["x"]]))
        },
        pairwise = {
            cli::cat_line(paste0("  vars  : ", paste(processed$vars, collapse = ", ")))
            cli::cat_line(paste0("  pairs : ", length(processed$pairs), " pair(s)"))
        },
        selected_vars = {
            cli::cat_line(paste0("  vars  : ", paste(processed$vars, collapse = ", ")))
        },
        one_sample = {
            cli::cat_line(paste0("  var   : ", processed$vars[["var"]]))
        },
        formula = {
            cli::cat_line(paste0("  formula : ", deparse(processed$formula)))
        },
        {
            nms = setdiff(names(processed), "data")
            for (nm in nms)
                cli::cat_line(paste0("  ", nm, " : ", paste(processed[[nm]], collapse = ", ")))
        }
    )

    if (!is.null(processed$data)) {
        dims = dim(processed$data)
        if (!is.null(dims))
            cli::cat_line(paste0("  data  : ", dims[[1]], " row(s) x ", dims[[2]], " col(s)"))
    }

    cli::cat_line("")

    # ---- Test ----
    cli::cat_line(cli::style_bold("Test"))
    cli::cat_line(cli::style_italic(paste0("  Name  : ", test_spec$name)))
    cli::cat_line(paste0("  Class : ", test_spec$cls))

    args = test_spec$args[!names(test_spec$args) %in% ".data"]
    for (nm in names(args))
        cli::cat_line(paste0("  ", nm, "    : ", paste(args[[nm]], collapse = ", ")))

    cli::cat_line("")

    # ---- Status ----
    cli::cat_line(cli::rule(line = "-"))
    cli::cli_text(cli::col_yellow(cli::style_italic(
        "! Results pending — call {.fn run_test} to execute."
    )))

    invisible(x)
}
