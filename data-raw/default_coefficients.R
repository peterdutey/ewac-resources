
default_coefficients <- list(
  audit1= data.frame(list(
    audit1_label = c('Never', 'Monthly or less',
                     '2 to 4 times a month', '2 to 3 times a week',
                     '4 to 5 times a week', '6 or more times a week'),
    audit1_value = c(0,
                     0.211,
                     0.579,
                     1.645,
                     3.314,
                     5.475),
    audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),

  audit2 = data.frame(list(
    audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                     "7 to 9", "10 to 12", "13 to 15", "16 or more"),
    audit2_value = c(2.132,
                     4.354,
                     5.603,
                     6.950,
                     10.184,
                     10.859,
                     15.972),
    audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),

  audit3 = data.frame(list(
    audit3_label = c("Never", "Less than monthly", "Monthly",
                     "Weekly", "Daily or almost daily"),
    audit3_value = c(0.129,
                     0.343,
                     0.645,
                     1.402,
                     5),
    audit3_score = 0:4)),

  binge_value = 5.703
)

usethis::use_data(default_coefficients, overwrite = TRUE)
