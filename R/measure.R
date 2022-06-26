


#' @title Master class for aggregate items
#' @description When creating aggregate items, be sure to inherit from and implement this class.
#' @field ret return value
#' @field name using column name
#' @field count data count
#' @field sum sum data
#' @field min min in the data
#' @field max max in the data
#' @field top1 top1 value in the data
#' @field top2 top2 value in the data
#' @field rate Percentage of top1 in sum
#' @field added is added
#' @importFrom R6 R6Class
measure_base <- R6Class("measure_base",
    private = list(
        top_n = function(data, n) {
            sort(data, decreasing = TRUE)[n]
        }
    ),
    public = list(


        ret = NULL,
        name = NULL,
        count = NULL,
        sum = NULL,
        min = NULL,
        max = NULL,
        top1 = NULL,
        top2 = NULL,
        rate = NULL,
        added = NULL,

        #' @description Initialize internal variables.
        #' @export
        init = function() {
            self$ret <- 0
            self$count <- 0
            self$sum <- 0
            self$min <- 0
            self$max <- 0
            self$top1 <- 0
            self$top2 <- 0
            self$rate <- 0
            self$added <- FALSE
        },

        #' @description list of internal variables.
        #' @export
        #' @return variables list
        info = function() {
            x <- list(
                count = self$count,
                sum = self$sum,
                min = self$min,
                max = self$max,
                top1 = self$top1,
                top2 = self$top2,
                rate = self$rate,
                added = self$added
            )
            return(x)
        },

        #' @description calculation.
        #' @param data A measure class
        #' @export
        #' @importFrom dplyr select
        #' @importFrom dplyr %>%
        #' @importFrom dplyr rename
        #' @importFrom dplyr summarise
        #' @importFrom dplyr n
        calc = function(data) {
            i <- data %>%
                dplyr::select(one_of(self$name)) %>%
                dplyr::rename("value" := self$name) %>%
                dplyr::summarise(
                    count = dplyr::n(),
                    sum = sum(value),
                    min = min(value),
                    max = max(value),
                    top1 = private$top_n(value, 1),
                    top2 = private$top_n(value, 2),
                    )
            self$count <- i$count
            self$sum <- i$sum
            self$min <- i$min
            self$max <- i$max
            self$top1 <- i$top1
            self$top2 <- i$top2
            self$rate <- self$top1 / self$sum
            self$calc_core(data)
        },

        #' @description Add up the data.
        #' @param target A measure class
        #' @export
        add = function(target) {

            tops <- c(self$top1, self$top2, target$top1, target$top2)

            self$count <- self$count + target$count
            self$sum <- self$sum + target$sum
            self$min <- ifelse(self$min < target$min, self$min, target$min)
            self$max <- ifelse(self$max > target$max, self$max, target$max)
            self$top1 <- private$top_n(tops, 1)
            self$top2 <- private$top_n(tops, 2)
            self$rate <- self$top1 / self$sum

            self$added <- TRUE
            self$add_core(target)
        }
    )
)

#' @title measure class for sum
#' @description measure class for sum.
#' @importFrom R6 R6Class
measure_sum <- R6Class("measure_sum",
    inherit = measure_base,
    public = list(

        #' @description initialize object.
        #' @param name using column name
        #' @export
        initialize = function(name) {
            self$name <- name
        },

        #' @description calculation for sum.
        #' @param data data.table
        #' @importFrom dplyr select
        #' @importFrom dplyr %>%
        calc_core = function(data) {
            self$ret <- data %>% dplyr::select(one_of(self$name)) %>% sum
        },

        #' @description Add up the data.
        #' @param target A measure class
        add_core = function(target) {
            self$ret <- self$ret + target$ret
        }
    )
)


measure_average <- R6Class("measure_average",
    inherit = measure_base,
    public = list(
        num_name = NULL,
        den_name = NULL,
        num = NULL,
        den = NULL,
        initialize = function(name, den_name = NULL) {
            self$num_name <- name
            self$den_name <- den_name
            self$name <- name
        },

        #' @importFrom dplyr select
        #' @importFrom dplyr %>%
        calc_core = function(data) {
            self$num <- data %>% dplyr::select(one_of(self$num_name)) %>% sum

            if (is.null(self$den_name)) {
                self$den <- nrow(data)
            } else {
                self$den <- data %>%
                            dplyr::select(one_of(self$den_name)) %>%
                            sum
            }
            self$ret <- self$num / self$den
        },
        add_core = function(target) {
            self$num <- self$num + target$num
            self$den <- self$den + target$den

            self$ret <- self$num / self$den
        }
    )
)

measure_var <- R6Class("measure_var",
    inherit = measure_base,
    public = list(
        mean = NULL,
        diff = NULL,
        var = NULL,
        initialize = function(name) {
            self$name <- name
        },

        #' @importFrom dplyr select
        #' @importFrom dplyr %>%
        #' @importFrom dplyr pull
        calc_core = function(data) {
            val <- data %>% dplyr::select(one_of(self$name)) %>% dplyr::pull()
            self$mean <- mean(val)
            self$diff <- sum((val - mean(val)) ^ 2)

            self$var <- var(val) * (self$count - 1) / self$count

            if (is.na(self$var)) {
                self$var <- 0
                }

            self$ret <- self$var
        },
        add_core = function(target) {


            new_mean <- self$sum / self$count

            self$diff <- (self$mean - new_mean) ^ 2 *
                         (self$count - target$count) +
                         self$diff +
                         (target$mean - new_mean) ^ 2 * target$count +
                         target$diff


            self$mean <- new_mean
            self$var <- self$diff / self$count
            self$ret <- self$var


        }
    )
)

measure_sd <- R6Class("measure_sd",
    inherit = measure_var,
    public = list(
        sd = NULL,
        initialize = function(name) {
            self$name <- name
        },

        #' @importFrom dplyr select
        #' @importFrom dplyr %>%
        #' @importFrom dplyr pull
        calc_core = function(data) {
            super$calc_core(data)
            self$sd <- sqrt(self$var)
            self$ret <- self$sd
        },
        add_core = function(target) {

            super$add_core(target)
            self$sd <- sqrt(self$var)
            self$ret <- self$sd

        }
    )
)

