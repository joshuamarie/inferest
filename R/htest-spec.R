#' Register an impl function for a model type
#'
#' Associates a single `impl` function with a `model_id` class string (e.g.
#' `"rel"`, `"pairwise"`). Pass one or more of these into [build_htest()] to
#' declare which model types a test supports.
#'
#' @param impl A function with signature `function(model_obj, args)`.
#' @param model_id_class A single string matching the primary class of a
#'   model ID object, e.g. `"rel"` or `"pairwise"`.
#'
#' @return A `registered_impl` object.
#'
#' @seealso [build_htest()]
#' @keywords internal
#' @export
reg_model_id = function(impl, model_id_class, method = NULL) {
    out = list(
        impl = impl,
        model_id_class = model_id_class,
        method = method
    )
    class(out) = "registered_impl"
    out
}

#' Find the impl function matching a model type
#'
#' Walks `impl_list` and returns the `impl` whose `model_id_class` matches
#' the primary class of `model_id`. For example, if `model_id` is a `rel`
#' object, it returns the impl registered for `"rel"`.
#'
#' @param model_id A model ID object from [rel()], [cont_tab()], etc.
#' @param impl_list A list of `registered_impl` objects from [reg_model_id()].
#'
#' @return A function with signature `function(model_obj, args)`.
#'
#' @seealso [build_htest()]
#' @keywords internal
#' @noRd
find_impl = function(model_id, impl_list, method = NULL) {
    target = class(model_id)[[1]]

    for (entry in impl_list) {
        model_match = entry$model_id_class == target

        method_match = if (is.null(method)) {
            is.null(entry$method)
        } else {
            identical(class(entry$method)[[1]], class(method)[[1]]) &&
                identical(entry$method$method_name, method$method_name)
        }

        if (model_match && method_match)
            return(entry$impl)
    }

    registered_types = purrr::map_chr(
        impl_list,
        function(imp) {
            if (is.null(imp$method)) {
                paste0(imp$model_id_class, " [classical]")
            } else {
                paste0(imp$model_id_class, " [", imp$method$method_name, "]")
            }
        }
    )

    cli::cli_abort(c(
        "No implementation found for model type {.val {target}}.",
        "i" = "Registered types: {.val {vapply(impl_list, \\(e) e$model_id_class, character(1))}}"
    ))
}

#' Construct a classed htest result
#'
#' Public constructor called at the end of every `impl` function. Wraps the
#' raw result with a consistent class vector:
#' `c(impl_cls, test_cls, "htest_infer")`.
#'
#' This means `print.ttest` dispatches for all t-test variants, while
#' `print.ttest_two` can override for variant-specific output.
#'
#' @param res The raw result returned by the impl function, e.g. an `htest`.
#' @param impl_cls A string naming the specific impl class, e.g. `"ttest_two"`.
#' @param test_cls A string naming the parent test class, e.g. `"ttest"`.
#'
#' @return An `htest_infer` object with slot `$data` holding the raw result,
#'   and class `c(impl_cls, test_cls, "htest_infer")`.
#'
#' @export
new_htest = function(res, impl_cls) {
    out = list(data = res)
    class(out) = c(impl_cls, "htest_infer")
    out
}

#' Core builder for hypothesis test functions
#'
#' `build_htest()` is called inside every user-facing test function such as
#' [TTEST()] or [CHI2_TEST()]. It handles two execution modes:
#'
#' - **Eager** — when `model_id` is supplied, the test executes immediately
#'   and returns an `htest_infer` object.
#' - **Deferred** — when `model_id` is `NULL`, execution is deferred and a
#'   `test_spec` object is returned for [run_test()] to execute later.
#'
#' @section Developer API:
#' `build_htest()` is part of the developer-facing extension API. It is the
#' single entry point for defining new test functions. A minimal test function
#' calls `build_htest()` with a `cls`, `args`, `impl`, `model_id`, and
#' `.name`.
#'
#' @param cls A string naming the test class, e.g. `"ttest"` or `"chi2test"`.
#'   Appended to the class vector of the result.
#' @param args A named list of test arguments, e.g.
#'   `list(.paired = TRUE, .mu = 0, .ci = 0.95)`. Passed into the impl
#'   function as `args`.
#' @param impl A list of `registered_impl` objects from [reg_model_id()].
#'   Declares which model types the test supports, and under which methods.
#' @param model_id A model ID object from [rel()], [cont_tab()], etc., or
#'   `NULL`. When non-`NULL`, triggers immediate execution.
#' @param .name A string displayed as the test title in output, e.g.
#'   `"T-Test"` or `"Chi-Square Test"`.
#' @param .env The caller environment. Used internally for error reporting.
#'   Defaults to [rlang::caller_env()].
#'
#' @return An `htest_infer` object (eager) or a `test_spec` object (deferred).
#'
#' @seealso [reg_model_id()], [new_htest()], [run_test()], [prepare_test()]
#'
#' @examples
#' NEW_TEST = function(.model = NULL, .data = NULL, .name = "Test something new", ...) {
#'     args = list(.data = .data)
#'
#'     build_htest(
#'         cls = "newtest",
#'         args = args,
#'         impl = list(
#'             reg_model_id(my_impl_fn, "rel")
#'         ),
#'         model_id = .model,
#'         .name = .name
#'     )
#' }
#'
#' @export
build_htest = function(cls, args, impl, model_id, .name, .env = rlang::caller_env()) {
    if (!is.null(model_id)) {
        processed = if (!is.null(args$.data)) {
            process_model_id(model_id, data = args$.data)
        } else {
            process_model_id_global(model_id)
        }

        impl_fn = find_impl(model_id, impl)

        model_obj = c(
            list(model_id = model_id),
            processed
        )

        res = impl_fn(model_obj = model_obj, args = args)
        res$name = .name
        class(res) = c(class(res), cls)

        return(res)
    }

    out = list(
        cls = cls,
        args = args,
        impl = impl,
        model_id = model_id,
        name = .name,
        env = .env
    )
    class(out) = c("test_spec", cls)
    out
}
