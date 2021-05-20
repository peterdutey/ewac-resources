test_that("compute_ewac", {

  test_ewac_uk <- round(compute_ewac(example_ewac), 1)
  test_ewac_grams <- round(compute_ewac(example_ewac, uk_units = FALSE))
  expect_equal(
    test_ewac_uk,
    c(1.7, 5.5, 15.2, 12.4, 7.8, 31.4, 7.9, 1.2, 15.2, 59.2)
  )
  expect_equal(
    test_ewac_grams,
    c(13, 44, 121, 99, 62, 251, 63, 9, 121, 474)
  )

  expect_equal(
    compute_ewac(data.frame(list(
      audit1_label = "Monthly or less",
      audit2_label = "3 to 4",
      audit3_label = "Never"
    )), uk_units = F),
    13.235048
  )
  expect_equal(
    compute_ewac(data.frame(list(
      audit1_label = "Monthly or less",
      audit2_label = "5 to 6",
      audit3_label = "Less than monthly"
    )), uk_units = F),
    25.106896
  )
  expect_equal(
    compute_ewac(data.frame(list(
      audit1_label = "4 to 5 times a week",
      audit2_label = "3 to 4",
      audit3_label = "Never"
    )), uk_units = F),
    121.318744
  )
  expect_equal(
    compute_ewac(data.frame(list(
      audit1_label = "2 to 3 times a week",
      audit2_label = "1 to 2",
      audit3_label = "Monthly"
    ))),
    7.185575
  )
  expect_equal(
    compute_ewac(data.frame(list(
      audit1_label = "2 to 3 times a week",
      audit2_label = "1 to 2",
      audit3_label = "invalid_input"
    ))),
    NA_real_
  )
  expect_equal(
    compute_ewac(data.frame(list(
      audit1_label = "2 to 3 times a week",
      audit2_label = "1 to 2",
      audit3_label = NA
    ))),
    NA_real_
  )
  expect_error(
    compute_ewac(data.frame(list(
      audit1_label = "2 to 3 times a week",
      audit2_label = "1 to 2"
    )))
  )
})


test_that("compute_auditc_score", {
  expect_equal(
    compute_auditc_score(data.frame(list(
      audit1_label = "Monthly or less",
      audit2_label = "3 to 4",
      audit3_label = "Never"
    ))),
    2L
  )
  expect_equal(
    compute_auditc_score(data.frame(list(
      audit1_label = "Monthly or less",
      audit2_label = "5 to 6",
      audit3_label = "Less than monthly"
    ))),
    4L
  )
  expect_equal(
    compute_auditc_score(data.frame(list(
      audit1_label = "4 to 5 times a week",
      audit2_label = "3 to 4",
      audit3_label = "Never"
    ))),
    5L
  )
  expect_equal(
    compute_auditc_score(data.frame(list(
      audit1_label = "4 to 5 times a week",
      audit2_label = "3 to 4",
      audit3_label = "missing"
    ))),
    NA_real_
  )
  expect_error(
    compute_auditc_score(data.frame(list(
      audit1_label = "4 to 5 times a week",
      audit2_label = "3 to 4"
    )))
  )
  expect_equal(
    compute_auditc_score(data.frame(list(
      audit1_label = "Never",
      audit2_label = "3 to 4",
      audit3_label = "Never"
    )), zero_if_never_drinks = T),
    0
  )
  expect_equal(
    compute_auditc_score(data.frame(list(
      audit1_label = "Never",
      audit2_label = "3 to 4",
      audit3_label = "Never"
    )), zero_if_never_drinks = F),
    1
  )
})
