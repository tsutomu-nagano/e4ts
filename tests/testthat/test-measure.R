library(data.table)

test_that("measure_sum test", {

    df <- data.frame(list(F1 = c(1, 2, 3)))
    f <- measure_sum$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 6)

})

test_that("measure_sum with weight test", {

    df <- data.frame(list(F1 = c(1, 2, 3), W = c(2, 3, 4)))
    f <- measure_sum$new(name = "F1")
    f$init()
    f$set_weight("W")
    f$calc(df)
    expect_equal(f$ret, 20)

})


test_that("measure_avarage test1", {

    df <- data.frame(list(F1 = c(10, 5, 27), F2 = c(1, 2, 3)))
    f <- measure_average$new(name = "F1", den_name = "F2")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 7)

})

test_that("measure_avarage test2", {

    df <- data.frame(list(F1 = c(10, 20, 30, 40, 50, 60)))
    f <- measure_average$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 35)

})

test_that("measure_avarage with weight test1", {

    df <- data.frame(list(F1 = c(10, 5, 27), F2 = c(1, 2, 3), W = c(2, 3, 4)))
    f <- measure_average$new(name = "F1", den_name = "F2")
    f$init()
    f$set_weight("W")
    f$calc(df)
    expect_equal(f$ret, 7.15)

})

test_that("measure_avarage with weight test2", {

    df <- data.frame(list(F1 = c(10, 20, 30), W = c(2, 3, 5)))
    f <- measure_average$new(name = "F1")
    f$init()
    f$set_weight("W")
    f$calc(df)
    expect_equal(f$ret, 23)

})


test_that("measure_var test1", {

    df <- data.frame(list(F1 = c(10, 20, 30), F2 = c(40, 50, 89)))
    f <- measure_var$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(
        round(f$ret, digits = 4),
        round(66.6667, digits = 4)
    )

})

test_that("measure_var test2", {

    df <- data.frame(list(F1 = c(10, 20, 30), F2 = c(40, 50, 89)))
    f1 <- measure_var$new(name = "F1")
    f1$init()
    f1$calc(df)

    f2 <- measure_var$new(name = "F2")
    f2$init()
    f2$calc(df)

    f1$add(f2)

    expect_equal(
        round(f1$ret, digits = 4),
        round(650.1389, digits = 4)
        )

})

test_that("measure_var test3", {

    df <- data.frame(list(F1 = c(10, 20, 30), F2 = c(40, 50, 89)))
    f1 <- measure_var$new(name = "F1")
    f1$init()
    f1$calc(df)

    f2 <- measure_var$new(name = "F2")
    f2$init()
    f2$calc(df)

    f1$add(f2)

    df2 <- data.frame(list(F1 = c(25, 30)))
    f3 <- measure_var$new(name = "F1")
    f3$init()
    f3$calc(df2)

    f1$add(f3)

    expect_equal(
        round(f1$ret, digits = 4),
        round(517.6875, digits = 4)
        )

})

test_that("measure_sd test1", {

    df <- data.frame(list(F1 = c(10, 20, 30), F2 = c(40, 50, 89)))
    f <- measure_sd$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(
        round(f$ret, digits = 4),
        round(8.1650, digits = 4)
    )

})

test_that("measure_sd test2", {

    df <- data.frame(list(F1 = c(10, 20, 30), F2 = c(40, 50, 89)))
    f1 <- measure_sd$new(name = "F1")
    f1$init()
    f1$calc(df)

    f2 <- measure_sd$new(name = "F2")
    f2$init()
    f2$calc(df)

    f1$add(f2)

    expect_equal(
        round(f1$ret, digits = 4),
        round(25.4978, digits = 4)
        )

})

test_that("measure_sd test3", {

    df <- data.frame(list(F1 = c(10, 20, 30), F2 = c(40, 50, 89)))
    f1 <- measure_sd$new(name = "F1")
    f1$init()
    f1$calc(df)

    f2 <- measure_sd$new(name = "F2")
    f2$init()
    f2$calc(df)

    f1$add(f2)

    df2 <- data.frame(list(F1 = c(25, 30)))
    f3 <- measure_sd$new(name = "F1")
    f3$init()
    f3$calc(df2)

    f1$add(f3)

    expect_equal(
        round(f1$ret, digits = 4),
        round(22.7527, digits = 4)
        )

})


test_that("measure_var test4", {

    df <- data.frame(list(F1 = c(10)))
    f1 <- measure_var$new(name = "F1")
    f1$init()
    f1$calc(df)

    expect_equal(
        round(f1$ret, digits = 4),
        round(0, digits = 4)
        )

})

test_that("measure_sd test4", {

    df <- data.frame(list(F1 = c(10)))
    f1 <- measure_sd$new(name = "F1")
    f1$init()
    f1$calc(df)

    expect_equal(
        round(f1$ret, digits = 4),
        round(0, digits = 4)
        )

})

test_that("measure_min test1", {

    df <- data.frame(list(F1 = c(10, 5, 18)))
    f1 <- measure_min$new(name = "F1")
    f1$init()
    f1$calc(df)

    expect_equal(f1$ret, 5)

})

test_that("measure_min test2", {

    df1 <- data.frame(list(F1 = c(10, 5, 18)), stringsAsFactors = FALSE)
    f1 <- measure_min$new(name = "F1")
    f1$init()
    f1$calc(df1)


    df2 <- data.frame(list(F1 = c(10, 1, 18)), stringsAsFactors = FALSE)
    f2 <- measure_min$new(name = "F1")
    f2$init()
    f2$calc(df2)

    f1$add(f2)

    expect_equal(f1$ret, 1)

})

test_that("measure_max test1", {

    df <- data.frame(list(F1 = c(10, 5, 18)), stringsAsFactors = FALSE)
    f1 <- measure_max$new(name = "F1")
    f1$init()
    f1$calc(df)

    expect_equal(f1$ret, 18)

})

test_that("measure_max test2", {

    df1 <- data.frame(list(F1 = c(10, 5, 18)), stringsAsFactors = FALSE)
    f1 <- measure_max$new(name = "F1")
    f1$init()
    f1$calc(df1)


    df2 <- data.frame(list(F1 = c(10, 1, 19)), stringsAsFactors = FALSE)
    f2 <- measure_max$new(name = "F1")
    f2$init()
    f2$calc(df2)

    f1$add(f2)

    expect_equal(f1$ret, 19)

})

test_that("measure_q4_1 test1", {

    df1 <- data.frame(list(F1 = seq(51)), stringsAsFactors = FALSE)
    f1 <- measure_q4_1$new(name = "F1")
    f1$init()
    f1$calc(df1)

    expect_equal(f1$ret, 13.5)

})

test_that("measure_q4_1 test2", {

    df1 <- data.frame(
        list(
            F1 = seq(51)),
            stringsAsFactors = FALSE)
    f1 <- measure_q4_1$new(name = "F1")
    f1$init()
    f1$calc(df1)

    df2 <- data.frame(
        list(
            F1 = seq(52, 101)),
            stringsAsFactors = FALSE)
    f2 <- measure_q4_1$new(name = "F1")
    f2$init()
    f2$calc(df2)

    f1$add(f2)

    expect_equal(f1$ret, 26)

})

test_that("measure_median test1", {

    df1 <- data.frame(list(F1 = seq(51)), stringsAsFactors = FALSE)
    f1 <- measure_median$new(name = "F1")
    f1$init()
    f1$calc(df1)

    expect_equal(f1$ret, 26)

})

test_that("measure_median test2", {

    df1 <- data.frame(
        list(
            F1 = seq(51)),
            stringsAsFactors = FALSE)
    f1 <- measure_median$new(name = "F1")
    f1$init()
    f1$calc(df1)

    df2 <- data.frame(
        list(
            F1 = seq(52, 101)),
            stringsAsFactors = FALSE)
    f2 <- measure_median$new(name = "F1")
    f2$init()
    f2$calc(df2)

    f1$add(f2)

    expect_equal(f1$ret, 51)

})

test_that("measure_q4_3 test1", {

    df1 <- data.frame(list(F1 = seq(51)), stringsAsFactors = FALSE)
    f1 <- measure_q4_3$new(name = "F1")
    f1$init()
    f1$calc(df1)

    expect_equal(f1$ret, 38.5)

})

test_that("measure_q4_3 test2", {

    df1 <- data.frame(
        list(
            F1 = seq(51)),
            stringsAsFactors = FALSE)
    f1 <- measure_q4_3$new(name = "F1")
    f1$init()
    f1$calc(df1)

    df2 <- data.frame(
        list(
            F1 = seq(52, 101)),
            stringsAsFactors = FALSE)
    f2 <- measure_q4_3$new(name = "F1")
    f2$init()
    f2$calc(df2)

    f1$add(f2)

    expect_equal(f1$ret, 76)

})

test_that("measure_weighted_mean test1", {

    df <- data.frame(list(F1 = c(10, 5, 27), F2 = c(2, 10, 13)))
    f <- measure_weighted_mean$new(name = "F1", weight = "F2")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 16.84)

})

test_that("measure_weighted_mean test2", {

    df <- data.frame(list(F1 = c(10, 40, 20), F2 = c(2, 3, 4), W = c(5, 6, 9)))
    f <- measure_weighted_mean$new(name = "F1", weight = "F2")
    f$init()
    f$set_weight("W")
    f$calc(df)
    expect_equal(f$ret, 176.5625)

})
