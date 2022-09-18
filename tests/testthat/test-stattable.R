library(data.table)


test_that("stattable test1", {

    df <- data.frame(list(
        F1 = c("A", "A", "B", "B", "B"),
        F2 = c(1, 2, 3, 4, 5)
        ))
    dimensions <- c("F1")
    measure <- measure_sum$new(name = "F2")

    st <- stattable(
        df = df,
        dimensions = dimensions,
        measure = measure
        )

    expect_setequal(
        st$value,
        c(3, 12, 15))

})


test_that("stattable test2", {

    df <- data.frame(list(
        F1 = c("A", "A", "B", "B", "B"),
        F2 = c("1", "2", "3", "4", "5"),
        W1 = c("2", "3", "4", "5", "6")
        ))
    dimensions <- c("F1")
    measure <- measure_sum$new(name = "F2")
    weight <- "W1"


    st <- stattable(
        df = df,
        dimensions = dimensions,
        measure = measure,
        weight = weight
        )

    expect_setequal(
        st$value,
        c(8, 62, 70))

})

