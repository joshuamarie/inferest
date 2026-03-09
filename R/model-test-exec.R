#' @rdname run-test
#' @export
run_test.model_to_analyze = function(.x, .test, ...) {
    .x$test_spec = as_test_spec(.test, model_id = .x$model_id)
    out = execute_test(.x)
    class(out) = c("stat_infer_out", .x$test_spec$cls)
    out
}

#' @rdname run-test
#' @export
run_test.test_lazy = function(.x, ...) {
    out = execute_test(.x)
    class(out) = c("stat_infer_out", .x$test_spec$cls)
    out
}

#' Execute the inference pipeline
#'
#' Shared execution core called by both [run_test.model_to_analyze()] (eager)
#' and [run_test.test_lazy()] (lazy). Resolves the correct `impl` function
#' using `model_id` carried inside `test_spec`, calls it with `model_obj` and
#' `args`, then wraps the result in a `stat_infer_out` object.
#'
#' @param .x A `model_to_analyze` or `test_lazy` object.
#' @param test_spec A `test_spec` object carrying `$impl`, `$args`, and
#'   `$model_id`.
#'
#' @return A `stat_infer_out` object with slots `$result`, `$test_spec`,
#'   `$model_id`, and `$data`.
#'
#' @seealso [run_test()]
#' @keywords internal
#' @noRd
execute_test = function(.x) {
    test_spec = .x$test_spec

    active_method = if (!is.null(.x$replicate_spec)) {
        method_replicate(method_name = .x$replicate_spec$method_name)
    } else {
        NULL
    }
    impl_fn = find_impl(test_spec$model_id, test_spec$impl, method = active_method)
    if (!is.null(.x$replicate_spec))
        test_spec$args$replicate_spec = .x$replicate_spec

    model_obj = c(list(model_id = .x$model_id), .x$processed)

    res = impl_fn(model_obj = model_obj, args = test_spec$args)
    res$name = test_spec$name
    class(res) = c(class(res), test_spec$cls)

    out = list(
        result = res,
        test_spec = test_spec,
        model_id = .x$model_id,
        data = .x$processed$data
    )
    class(out) = "stat_infer_out"
    out
}

#' @export
print.stat_infer_out = function(x, ...) {
    test_title = x$result$name
    test_title_block = cli::rule(left = test_title, line = "-")
    test_title_block = cli::style_bold(test_title_block)
    cli::cat_line("\n", test_title_block, "\n")
    print(x$result)
    invisible(x)
}
