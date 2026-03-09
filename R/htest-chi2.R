#' Chi-Square Test
#'
#' `CHI2_TEST()` performs a chi-square test of independence or goodness of fit.
#' It dispatches on the `cont_tab()` model ID declared in [define_model()].
#'
#' @section Supported model IDs:
#' - `cont_tab()` — chi-square test of independence between two categorical
#'   variables, or goodness of fit when `.p` is supplied
#'
#' @section Simulation:
#' When exact computation is not feasible, Monte Carlo simulation of the
#' p-value can be enabled via `opts_simul_chi()` passed to `.simulate_p_value`:
#'
#' ```r
#' mtcars |>
#'     define_model(cont_tab(vs, am)) |>
#'     prepare_test(CHI2_TEST) |>
#'     update(.simulate_p_value = opts_simul_chi(simul = TRUE, B = 5000)) |>
#'     run_test()
#' ```
#'
#' @param .model A `cont_tab()` model ID object. When supplied, the test
#'   executes immediately. When `NULL` (default), returns a `test_spec` for
#'   use in the lazy pipeline via [prepare_test()].
#' @param .data A data frame. Only used when `.model` is supplied directly
#'   (standalone path). Ignored in the pipeline path.
#' @param .correct Logical. Whether to apply Yates' continuity correction for
#'   2x2 tables. Defaults to `TRUE`.
#' @param .p A numeric vector of probabilities representing the expected
#'   proportions under the null hypothesis. When `NULL` (default), equal
#'   probabilities are assumed (goodness of fit against a uniform
#'   distribution).
#' @param .rescale_p Logical. Whether to rescale `.p` to sum to 1. Defaults
#'   to `FALSE`.
#' @param .simulate_p_value A list from `opts_simul_chi()` controlling Monte
#'   Carlo simulation of the p-value. Defaults to `NULL` (no simulation).
#' @param ... Additional arguments passed to methods.
#'
#' @return An `htest_infer` object of subclass `chi2test` (standalone), or a
#'   `test_spec` object (pipeline).
#'
#' @seealso [define_model()], [cont_tab()], [prepare_test()], [run_test()],
#'   [opts_simul_chi()], [stats::chisq.test()]
#'
#' @examples
#' mtcars |>
#'     define_model(cont_tab(vs, am)) |>
#'     prepare_test(CHI2_TEST) |>
#'     run_test()
#'
#' mtcars |>
#'     define_model(cont_tab(vs, am)) |>
#'     run_test(CHI2_TEST)
#'
#' # With simulation
#' mtcars |>
#'     define_model(cont_tab(vs, am)) |>
#'     prepare_test(CHI2_TEST) |>
#'     update(.simulate_p_value = opts_simul_chi(simul = TRUE, B = 5000)) |>
#'     run_test()
#'
#' @export
CHI2_TEST =
    function(
        .model = NULL,
        .data = NULL,
        .correct = TRUE,
        .p = NULL,
        .rescale_p = FALSE,
        .simulate_p_value = NULL,
        ...
    ) {

        args = list(
            .data = .data,
            .correct = .correct,
            .p = .p,
            .rescale_p = .rescale_p,
            .simulate_p_value = .simulate_p_value
        )

        model_id_obj = if (!rlang::quo_is_missing(rlang::enquo(.model)) && !is.null(.model)) {
            .model
        } else {
            NULL
        }

        build_htest(
            cls = "chi2test",
            args = args,
            impl = list(
                reg_model_id(chi2test_impl_cont, "cont_tab")
            ),
            model_id = model_id_obj,
            .name = "Chi-Square Test"
        )
    }

#' Simulation options for `CHI2_TEST()`
#'
#' Constructs a named list of options controlling Monte Carlo simulation of the
#' p-value in [CHI2_TEST()]. Pass the result to the `.simulate_p_value`
#' argument, either directly in [CHI2_TEST()] or via [update()] in the lazy
#' pipeline.
#'
#' @param simul Logical. Whether to simulate the p-value via Monte Carlo.
#'   Defaults to `FALSE`.
#' @param B Integer. Number of Monte Carlo replicates. Defaults to `2000`.
#'
#' @return A named list with elements `simul` and `B`.
#'
#' @seealso [CHI2_TEST()]
#'
#' @examples
#' opts_simul_chi(simul = TRUE, B = 5000)
#'
#' mtcars |>
#'     define_model(cont_tab(vs, am)) |>
#'     prepare_test(CHI2_TEST) |>
#'     update(.simulate_p_value = opts_simul_chi(simul = TRUE)) |>
#'     run_test()
#'
#' @export
opts_simul_chi = function(simul = FALSE, B = 2000)
    list(simul = simul, B = 2000)

chi2test_impl_cont = function(model_obj, args) {
    dat = model_obj$mat

    test_out = stats::chisq.test(
        x = dat,
        correct = args$.correct,
        p = args$.p %||% rep(1 / length(dat), length(dat)),
        rescale.p = args$.rescale_p,
        simulate.p.value = args$.simulate_p_value$simul %||% FALSE,
        B = args$.simulate_p_value$B %||% 2000L
    )

    new_htest(test_out, impl_cls = "chi2test_cont")
}

#' @keywords internal
#' @export
print.chi2test_cont = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}
