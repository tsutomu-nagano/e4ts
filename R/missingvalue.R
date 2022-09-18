


#' @title missing value convertor.
#' @description Processes missing values by the selected method..
#' @field missing_values missing_values vector
#' @field is_missing is_missing method
#' @importFrom R6 R6Class
#' @export
conversion_base <- R6Class("conversion_base",
    private = list(



    ),
    public = list(
        missing_values = NULL,
        is_missing = NULL,
        initialize = function(missing_values = NULL) {

            self$missing_values <- missing_values

            if (is.null(missing_values)) {
                self$is_missing <- function(values) {
                    is.na(suppressWarnings(as.numeric(values)))
                    }
            } else {
                self$is_missing <- function(values) {
                    values %in% self$missing_values
                    }
            }

        },


        #' @description missing values convert.
        #' @param df target data data.frame
        #' @param name target column name character
        #' @export
        convert = function(df, name) {
            return(private$convert_core(df, name))
        }
    )
)

#' @title Converts missing values to 0.
#' @description Converts missing values to 0.
#' @importFrom R6 R6Class
#' @export
conversion_zero <- R6Class("conversion_zero",
    inherit = conversion_base,
    private = list(
        convert_core = function(df, name) {

            if (class(df[[name]]) == "character") {
                zero <- "0"
            } else {
                zero <- 0
            }

            df %>%
            dplyr::mutate(!!name := as.numeric(dplyr::if_else(
                self$is_missing(!!as.name(name)) == TRUE,
                zero, !!as.name(name))))

        }
    ),
    public = list(
        initialize = function(missing_values = NULL) {
            super$initialize(missing_values)
        }
    )
)
