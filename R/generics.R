#' Define a model for statistical inference
#'
#' `define_model()` initializes the pipeline for statistical inference.
#'
#' @param .x A data frame, or missing for lexical scoping.
#' @param to_analyze A model declaration from `rel()`, `cont_tab()`,
#'   `prop_model()`, or `pairwise()`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `stat_infer_pipeline` object.
#'
#' @name define-model
#' @export
define_model = function(.x, ...) {
    UseMethod("define_model")
}

#' Declare hypotheses
#'
#' Not yet implemented.
#'
#' @name hypotheses-claims
#' @export
write_claim = function(.x, ...) {
    UseMethod("write_claim")
}

#' Lazily prepare a single test
#'
#' `prepare_test()` registers a single test for lazy execution. Use this
#' when you intend to call `plot_test()` after `run_test()`.
#'
#' @param .x A `stat_infer_pipeline` object.
#' @param test A single test specification from a test function like `TTEST()`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `stat_infer_pipeline` object.
#'
#' @name prepare-test
#' @export
prepare_test = function(.x, ...) {
    UseMethod("prepare_test")
}

#' @importFrom stats update
#' @export
stats::update

#' Set a resampling strategy
#'
#' `resample()` defines the resampling method and number of repetitions.
#' Can appear before or after `update()` in the pipeline.
#'
#' @param .x A `stat_infer_pipeline` object.
#' @param .method Resampling method. One of `"perm"` or `"boot"`.
#' @param n Number of repetitions.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `stat_infer_pipeline` object.
#'
#' @name prepare-test
#' @export
resample = function(.x, ...) {
    UseMethod("resample")
}

#' Register multiple tests
#'
#' `test_with()` registers one or more tests for lazy execution. Tests must
#' share the same `model_id` as the declared model.
#'
#' @param x A `stat_infer_pipeline` object.
#' @param test A `test_spec` object from a test function.
#' @param ... Additional named or unnamed `test_spec` objects.
#'
#' @return A `stat_infer_pipeline` object.
#'
#' @name prepare-test
#' @export
test_with = function(.x, ...) {
    UseMethod("test_with")
}

#' Execute the pipeline
#'
#' `run_test()` executes the statistical inference pipeline. Dispatches on
#' both the pipeline object and the presence of a `resample_spec`.
#'
#' @param .x A `stat_infer_pipeline` object.
#' @param resample A `resample_spec` object, or `NULL`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `stat_infer_out` object.
#'
#' @name run-test
#' @export
run_test = function(.x, ...) {
    UseMethod("run_test")
}

#' Tidy the test output
#'
#' Registers an S7 method on `stat_infer_out` via `broom::tidy`.
#'
#' @importFrom broom tidy
#' @export
tidy = broom::tidy

#' Display the test output
#'
#' `display()` prints a formatted summary to the console. Returns invisibly.
#'
#' @param .x A `stat_infer_out` object.
#' @param ... Additional arguments passed to methods.
#'
#' @name display
#' @export
display = function(.x, ...) {
    UseMethod("display")
}

#' Plot the test output
#'
#' `plot_test()` produces a plot for a single-test pipeline. Errors
#' informatively if more than one test is registered.
#'
#' @param .x A `stat_infer_out` object.
#' @param ... Additional arguments passed to methods.
#'
#' @name plot-test
#' @export
plot_test = function(.x, ...) {
    UseMethod("plot_test")
}
