

# df <- data.frame(list(A=c(1,2,3)))

# print(df)


# varvar <- function(data) {

#     return(
#         list(
#             mean = mean(data),
#             total = sum(data),
#             diff =  sum((data - mean(data)) ^ 2),
#             count = length(data),
#             var = var(data) * (length(data) - 1) / length(data)
#     ))

# }

# adder <- function(d1, d2) {


#     ret <- list(
#             total = d1$total + d2$total,
#             count = d1$count + d2$count
#     )


#     ret$mean <- ret$total / ret$count


#     ret$diff <- ((d1$mean - ret$mean) ^ 2 * d1$count + d1$diff) +
#                 ((d2$mean - ret$mean) ^ 2 * d2$count + d2$diff)

#     ret$var <- ret$diff / ret$count

#     return(ret)


# }




# x <- c(10, 20, 30)
# x_var <- varvar(x)


# y <- c(40, 50, 89)
# y_var <- varvar(y)


# z <- c(25, 30)
# z_var <- varvar(z)



# xy <- c(x, y)
# xy_var <- varvar(xy)

# xyz <- c(xy, z)
# xyz_var <- varvar(xyz)


# print(x_var)
# print(y_var)
# print(z_var)
# print(xy_var)

# print(adder(x_var, y_var))


# # df <- fread("R6TestData.csv", stringsAsFactors = FALSE)

# # category <- c("F1","F2")
# # # f <- sdc_measure_total$new(name = "V2")
# # f <- sdc_measure_average$new(num_name = "V1", den_name = "V2")


# # dfx <- df %>%
# #     nest(-category) %>%
# #     mutate(func = map(data,func_calc,base_func = f)) %>%
# #     mutate(ret = map(func, func_ret))

# # dfx <- dfx %>% select(-data)

# # columns <- c(category,"func","ret")

# # for (sumf in category) {

# #     if (length(category) == 1) {

# #         dfy <- data.table(list(
# #                 func = func_sum(dfx)
# #                 )) %>%
# #                 rename(func = V1)
# #     } else {
# #         nestf <- category[-which(category %in% sumf)]

# #         dfy <- dfx %>%
# #                 select(-one_of(c(sumf, "ret"))) %>%
# #                 nest(-nestf) %>%
# #                 mutate(func = map(data, func_sum))

# #     }
# #     dfy <- dfy %>%
# #                 mutate(ret = map(func, func_ret)) %>%
# #                 mutate(!!sumf := "T") %>%
# #                 select(one_of(columns))

# #     dfx <- rbind(dfx, dfy)

# # }


# # dfx %>%
# #     mutate(info = map(func, func_info)) %>%
# #     hoist(
# #         info,
# #         count = "count",
# #         sum = "sum",
# #         min = "min",
# #         max = "max",
# #         top1 = "top1",
# #         top2 = "top2",
# #         rate = "rate"
# #         ) %>%
# #     rename(value = ret) %>%
# #     arrange(across(category)) %>%
# #     print



# # # sumf <- "F3"
# # # nestf <- category[-which(category %in% sumf)] 

# # # dfy <- dfx %>%
# # #             select(-one_of(c(sumf,"ret"))) %>% nest(-nestf) %>%
# # #             mutate(func = map(data, func_sum)) %>%
# # #             mutate(ret = map(func, func_ret)) %>%
# # #             mutate(F3 = "T") %>%
# # #             select(one_of(columns)) %>%
# # #             arrange(across(category))

# # # print(dfx)
# # # print(dfy)
# # # dfx <- rbind(dfx,dfy) %>% arrange(across(category))
# # # print(dfx)

# # # dfy <- dfx %>%
# # #             select(-F2,-ret) %>% nest(-F1, -F3) %>%
# # #             mutate(func = map(data, func_sum)) %>%
# # #             mutate(ret = map(func, func_ret)) %>%
# # #             mutate(F2 = "T") %>%
# # #             select(one_of(columns)) %>%
# # #             arrange(across(category))

# # # print(dfx)
# # # print(dfy)
# # # dfx <- rbind(dfx,dfy) %>% arrange(across(category))
# # # print(dfx)


# # # dfy <- dfx %>%
# # #             select(-F1,-ret) %>% nest(-F2, -F3) %>%
# # #             mutate(func = map(data, func_sum)) %>%
# # #             mutate(ret = map(func, func_ret)) %>%
# # #             mutate(F1 = "T") %>%
# # #             select(one_of(columns)) %>%
# # #             arrange(across(category))

# # # print(dfx)
# # # print(dfy)
# # # dfx <- rbind(dfx,dfy) %>% arrange(across(category))
# # # print(dfx)



# # # # df_F3 <- df_F3 %>%
# # # #             select(-F2, -F3, -ret) %>% nest(-F1) %>%
# # # #             mutate(func = map(data, func_add)) %>%
# # # #             mutate(ret = map(func, func_ret)) %>%
# # # #             mutate(F2 = "T") %>%
# # # #             select(F1,F2,F3,func,ret) %>%
# # # #             arrange(F1,F2,F3)


# # # # print(df_F2)




# # # # rbind(
# # # #     dfx %>% select(F1, F2, ret) %>% unnest %>% as_tibble,
# # # #     dfy %>% select(F1, F2, ret) %>% unnest %>% as_tibble
# # # # ) %>% write.table("R6TestData_sub.csv", row.names = FALSE, sep = ",")
