#' T-Test
#'
#' `TTEST()` performs a t-test for one-sample, two-sample, paired, pairwise,
#' or formula-based comparisons. It dispatches on the `model_id` type declared
#' in [define_model()], automatically selecting the appropriate variant.
#'
#' @section Supported model IDs:
#' - `compare_by()`: targeted for two-sample or paired t-test, comparing `x` across (2)
#'   groups
#' - `one_sample()`: one-sample t-test against a reference value `.mu`
#' - `pairwise()`: all pairwise t-tests across selected variables
#' - `formula`: passed directly to [stats::t.test()]
#'
#' @section Resampling:
#' `TTEST()` supports permutation and bootstrap resampling via [resample()]
#' for `rel()` models. Use `.method = "permute"` for permutation testing or
#' `.method = "boot"` for bootstrap confidence intervals.
#'
#' ``` r
#' sleep |>
#'     define_model(rel(group, extra)) |>
#'     prepare_test(TTEST) |>
#'     resample(.method = "permute", n = 5000) |>
#'     run_test()
#' ```
#'
#' @param .model A model ID object from [rel()], [one_sample()], [pairwise()],
#'   or a formula. When supplied, the test executes immediately. When `NULL`
#'   (default), returns a `test_spec` for use in the lazy pipeline via
#'   [prepare_test()].
#' @param .data A data frame. Only used when `.model` is supplied directly
#'   (standalone path). Ignored in the pipeline path.
#' @param .name A string used as the test title in output. Defaults to
#'   `"T-Test"`.
#' @param .paired Logical. Whether to perform a paired t-test. Defaults to
#'   `TRUE`.
#' @param .mu Numeric. The reference value under the null hypothesis. Defaults
#'   to `0`.
#' @param .alt A string specifying the alternative hypothesis. One of
#'   `"two.sided"` (default), `"greater"`, or `"less"`.
#' @param .ci Numeric. The confidence level for the confidence interval.
#'   Defaults to `0.95`.
#' @param ... Additional arguments passed to methods.
#'
#' @return An `htest_infer` object (standalone), or a `test_spec` object
#'   (pipeline). The specific subclass depends on the model ID:
#'   - `ttest_two` for `rel()`
#'   - `ttest_one` for `one_sample()`
#'   - `ttest_pairwise` for `pairwise()`
#'   - `ttest_formula` for formula
#'   - `ttest_permute` for permutation resampling
#'   - `ttest_boot` for bootstrap resampling
#'
#' @seealso [define_model()], [prepare_test()], [run_test()], [resample()],
#'   [stats::t.test()]
#'
#' @examples
#' # Pipeline path (lazy)
#' sleep |>
#'     define_model(rel(group, extra)) |>
#'     prepare_test(TTEST) |>
#'     update(.paired = FALSE) |>
#'     run_test()
#'
#' # Eager path (immediate)
#' sleep |>
#'     define_model(rel(group, extra)) |>
#'     run_test(TTEST)
#'
#' # Standalone path
#' TTEST(rel(group, extra), sleep)
#'
#' # One-sample
#' sleep |>
#'     define_model(one_sample(extra)) |>
#'     run_test(TTEST)
#'
#' # Pairwise
#' sleep |>
#'     define_model(pairwise(extra, group)) |>
#'     run_test(TTEST)
#'
#' # Permutation resampling
#' sleep |>
#'     define_model(rel(group, extra)) |>
#'     prepare_test(TTEST) |>
#'     resample(.method = "permute", n = 5000) |>
#'     run_test()
#'
#' @export
TTEST = function(
        .model = NULL,
        .data = NULL,
        .name = "T-Test",
        .paired = TRUE,
        .mu = 0,
        .alt = "two.sided",
        .ci = 0.95,
        ...
) {
    args = list(
        .data = .data,
        .paired = .paired,
        .mu = .mu,
        .alt = .alt,
        .ci = .ci
    )

    build_htest(
        cls = "ttest",
        args = args,
        impl = list(
            reg_model_id(ttest_impl_two, "compare_by"),
            reg_model_id(ttest_impl_formula, "formula"),
            reg_model_id(ttest_impl_pairwise, "pairwise"),
            reg_model_id(ttest_impl_one, "one_sample"),
            reg_model_id(
                ttest_impl_rel_permute,
                "rel",
                method = method_replicate(
                    method_name = "permute",
                    args = extra_args(
                        n ~ 1000L,
                        seed ~ NULL
                    )
                )
            ),
            reg_model_id(
                ttest_impl_rel_boot,
                "rel",
                method = method_replicate(
                    method_name = "boot",
                    args = extra_args(
                        n ~ 1000L,
                        seed ~ NULL
                    )
                )
            )
        ),
        model_id = .model,
        .name = .name
    )
}

ttest_impl_two = function(model_obj, args) {
    grp = model_obj$data[[model_obj$vars$grp[[1]]]]
    resp = model_obj$data[[model_obj$vars$x[[1]]]]

    grp = as.character(grp)
    lvls = unique(grp)

    if (length(lvls) != 2L) {
        cli::cli_abort(c(
            "Two-sample t-test requires exactly 2 groups.",
            "i" = "Found {length(lvls)} group{?s} in {.val {model_obj$vars$x[[1]]}}."
        ))
    }

    res = stats::t.test(
        x = resp[grp == lvls[[1]]],
        y = resp[grp == lvls[[2]]],
        paired = isTRUE(args$.paired),
        mu = args$.mu  %||% 0,
        alternative = args$.alt %||% "two.sided",
        conf.level  = args$.ci  %||% 0.95
    )

    new_htest(res, impl_cls = "ttest_two")
}

ttest_impl_formula = function(model_obj, args) {
    res = stats::t.test(
        formula = model_obj$formula,
        data = model_obj$data,
        mu = args$.mu %||% 0,
        alternative = args$.alt %||% "two.sided",
        conf.level = args$.ci %||% 0.95
    )

    new_htest(res, impl_cls = "ttest_formula")
}

ttest_impl_pairwise = function(model_obj, args) {
    pairs = model_obj$pairs

    res = lapply(pairs, function(pair) {
        out = stats::t.test(
            x = model_obj$data[[pair[[1]]]],
            y = model_obj$data[[pair[[2]]]],
            paired = isTRUE(args$.paired),
            mu = args$.mu %||% 0,
            alternative = args$.alt %||% "two.sided",
            conf.level = args$.ci %||% 0.95
        )
        tidy_df = broom::tidy(out)
        tidy_df$pair = paste0(pair[[1]], " vs ", pair[[2]])
        tidy_df
    })

    new_htest(dplyr::bind_rows(res), impl_cls = "ttest_pairwise")
}

ttest_impl_one = function(model_obj, args) {
    res = stats::t.test(
        x = model_obj$data[[model_obj$vars[["var"]]]],
        mu = args$.mu,
        alternative = args$.alt,
        conf.level = args$.ci
    )

    new_htest(res, impl_cls = "ttest_one")
}

ttest_impl_rel_permute = function(model_obj, args) {
    n = args$replicate_spec$args$n
    seed = args$replicate_spec$args$seed

    if (!is.null(seed)) set.seed(seed)

    data = model_obj$data
    grp = model_obj$vars$x[[1]]
    resp = model_obj$vars$resp[[1]]
    lvls = unique(data[[grp]])

    if (length(lvls) != 2L) {
        cli::cli_abort(c(
            "Permutation t-test requires exactly 2 groups.",
            "i" = "Found {length(lvls)} group{?s} in {.val {grp}}."
        ))
    }

    obs = stats::t.test(
        x = data[[resp]][data[[grp]] == lvls[[1]]],
        y = data[[resp]][data[[grp]] == lvls[[2]]],
        paired = isTRUE(args$.paired)
    )$statistic

    null_dist = replicate(n, {
        perm = sample(data[[resp]])
        stats::t.test(
            x = perm[data[[grp]] == lvls[[1]]],
            y = perm[data[[grp]] == lvls[[2]]],
            paired = isTRUE(args$.paired)
        )$statistic
    })

    new_htest(
        list(
            observed = obs,
            null_dist = null_dist,
            p.value = mean(abs(null_dist) >= abs(obs)),
            n = n
        ),
        impl_cls = "ttest_permute"
    )
}

ttest_impl_rel_boot = function(model_obj, args) {
    n = args$replicate_spec$args$n
    seed = args$replicate_spec$args$seed

    if (!is.null(seed)) set.seed(seed)

    data = model_obj$data
    grp  = model_obj$vars$x[[1]]
    resp = model_obj$vars$resp[[1]]
    lvls = unique(data[[grp]])

    if (length(lvls) != 2L) {
        cli::cli_abort(c(
            "Bootstrap t-test requires exactly 2 groups.",
            "i" = "Found {length(lvls)} group{?s} in {.val {grp}}."
        ))
    }

    boot_dist = replicate(n, {
        idx = sample(nrow(data), replace = TRUE)
        boot = data[idx, ]
        stats::t.test(
            x = boot[[resp]][boot[[grp]] == lvls[[1]]],
            y = boot[[resp]][boot[[grp]] == lvls[[2]]],
            paired = isTRUE(args$.paired)
        )$statistic
    })

    new_htest(
        list(
            boot_dist = boot_dist,
            ci = quantile(
                boot_dist,
                c((1 - args$.ci) / 2, 1 - (1 - args$.ci) / 2)
            ),
            n = n
        ),
        impl_cls = "ttest_boot"
    )
}

#' @keywords internal
#' @export
print.ttest_two = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_formula = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_pairwise = function(x, ...) {
    print(x$data)
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_one = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_permute = function(x, ...) {
    cli::cli_text("{.field Observed statistic}    : {round(x$data$observed, 4)}")
    cli::cli_text("{.field p-value (permutation)} : {round(x$data$p.value, 4)}")
    cli::cli_text("{.field Replicates}            : {x$data$n}")
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_boot = function(x, ...) {
    ci = round(x$data$ci, 4)
    cli::cli_text("{.field Bootstrap CI} : [{ci[[1]]}, {ci[[2]]}]")
    cli::cli_text("{.field Replicates}   : {x$data$n}")
    invisible(x)
}
