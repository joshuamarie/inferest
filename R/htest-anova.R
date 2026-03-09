#' @export
ANOVA =
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
                reg_model_id(anova_impl_rel, "rel"),
                reg_model_id(anova_impl_formula, "formula")
            ),
            model_id = model_id_obj,
            .name = "Chi-Square Test"
        )
    }

anova_impl_rel = function(model_obj, args) {
    dat = model_obj$mat

    test_out = stats::chisq.test(
        x = dat,
        correct = args$.correct,
        p = args$.p %||% rep(1 / length(dat), length(dat)),
        rescale.p = args$.rescale_p,
        simulate.p.value = args$.simulate_p_value$simul %||% FALSE,
        B = args$.simulate_p_value$B %||% 2000L
    )

    new_htest(test_out, impl_cls = "anova_rel")
}

anova_impl_formula = function(model_obj, args) {
    dat = model_obj$data

    test_out = stats::chisq.test(
        x = dat,
        correct = args$.correct,
        p = args$.p %||% rep(1 / length(dat), length(dat)),
        rescale.p = args$.rescale_p,
        simulate.p.value = args$.simulate_p_value$simul %||% FALSE,
        B = args$.simulate_p_value$B %||% 2000L
    )

    new_htest(test_out, impl_cls = "anova_formula")
}
