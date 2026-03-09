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
