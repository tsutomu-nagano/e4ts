

test_that("missingvalue base test", {


    conv <- conversion_zero$new()

    df <- data.frame(
        list(F1 = c("1", "2", "3")),
        stringsAsFactors = FALSE) %>%
    conv$convert("F1")



    f <- measure_sum$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 6)

})

test_that("missingvalue zero test", {


    conv <- conversion_zero$new()

    df <- data.frame(
        list(F1 = c("1", "A", "3")),
        stringsAsFactors = FALSE) %>%
    conv$convert("F1")

    f <- measure_sum$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 4)

})

test_that("missingvalue omit test", {


    conv <- conversion_omit$new()

    df <- data.frame(
        list(F1 = c("1", "A", "3")),
        stringsAsFactors = FALSE) %>%
    conv$convert("F1")

    f <- measure_sum$new(name = "F1")
    f$init()
    f$calc(df)
    expect_equal(f$ret, 4)

    expect_equal(nrow(df), 2)



})
