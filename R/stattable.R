
#' @title create stat table
#' @description create stat table
#' @param df data.table
#' @param dimensions column names string vector
#' @param measure A measure class
#' @param conv missing values conversion method conversion class
#' @param weight weight name
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr hoist
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr across
#' @importFrom dplyr %>%
#' @importFrom dplyr one_of
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom utils data
#' @importFrom data.table data.table
#' @export
stattable <- function(
    df,
    dimensions,
    measure,
    conv = conversion_zero$new(),
    weight = NULL
    ) {

    func_calc <- function(data, base_func) {
        f <- base_func$clone()
        f$calc(data)
        return(f)
    }
    func_ret <- function(f) {
        f$ret
    }
    func_sum <- function(funcs) {

        ref <- funcs$func[[1]]$clone()
        if (nrow(funcs) >= 2) {
            for (idx in 2:nrow(funcs)) {
                ref$add(funcs$func[[idx]])
            }
        }
        return(ref)
    }
    func_info <- function(f) {
        f$info()
    }


    measure$init()

    func <- "func"


    # missing value conversion
    df <- df %>%
    conv$convert(measure$name)

    # weight
    if (!is.null(weight)) {
        df <- df %>%
        dplyr::mutate(
            !!weight := as.numeric(!!as.name(weight)),
            !!measure$name := !!as.name(measure$name) * !!as.name(weight)
            )
    }


    dfx <- df %>%
        tidyr::nest(data = -dimensions) %>%
        dplyr::mutate(
            !!func := purrr::map(data, func_calc, base_func = measure)) %>%
        dplyr::mutate(
            ret = purrr::map(func, func_ret))


    dfx <- dfx %>%
           dplyr::select(-data)


    columns <- c(dimensions, "func", "ret")

    for (sumf in dimensions) {

        if (length(dimensions) == 1) {

            dfy <- data.table(list(
                    "func" = func_sum(dfx)
                    )) %>%
                    dplyr::rename("func" = "V1")
        } else {
            nestf <- dimensions[-which(dimensions %in% sumf)]

            dfy <- dfx %>%
                    dplyr::select(-dplyr::one_of(c(sumf, "ret"))) %>%
                    tidyr::nest(-nestf) %>%
                    dplyr::mutate(
                        !!func := purrr::map(data, func_sum))

        }
        dfy <- dfy %>%
                    dplyr::mutate(
                        ret = purrr::map(func, func_ret)) %>%
                    dplyr::mutate(
                        !!sumf := "T") %>%
                    dplyr::select(dplyr::one_of(columns))

        dfx <- rbind(dfx, dfy)

    }

    ret <- dfx %>%
        dplyr::mutate(
            "info" = purrr::map(func, func_info)) %>%
        tidyr::hoist(
            "info",
            count = "count",
            sum = "sum",
            min = "min",
            max = "max",
            top1 = "top1",
            top2 = "top2",
            rate = "rate",
            added = "added"
            ) %>%
        dplyr::rename(value = ret) %>%
        tidyr::unnest("value") %>%
        dplyr::arrange(dplyr::across(dimensions))


    return(ret)

}