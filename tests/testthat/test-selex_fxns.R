test_that("logistic selectivity works", {
 lengths <- seq(1, 100, by = 1)
 sel <- logistic1(len = lengths, a = 50, b = 10)
 expect_length(sel, length(lengths))
 expect_true(sel[1] < sel[length(sel)])
})

test_that("double normal selectivity works", {
  lengths <- seq(1, 100, by = 1)
  init_sel_val <- 0.1
  fin_sel_val <- 0.1
  sel <- doubleNorm24(lengths, a = 50 , b = 10, c = 30, d = 30, e = init_sel_val,
                      f = fin_sel_val, use_e_999 = FALSE, use_f_999 = FALSE)
  expect_equal(sel[1], init_sel_val, ignore_attr = TRUE, tolerance = 0.001)
  expect_equal(sel[length(sel)], fin_sel_val, ignore_attr = TRUE,
               tolerance = 0.001)
  expect_equal(max(sel), 1, ignore_attr = TRUE)
  sel <- doubleNorm24(lengths, a = 50 , b = 10, c = 10, d = 1, e = 0.1,
                      f = 0.1, use_e_999 = TRUE, use_f_999 = TRUE)
  expect_equal(max(sel), 1)
  #TODO: add better checks
})

test_that("nonparametric selectivity works", {
  lengths <- seq(1, 100, by = 1)
  last_selex <- 0.1
  sel <- nonparasize6(x = lengths, len_pt_first = 10, len_pt_last = 90,
                      sel_val_pts = log(c(0.1, 0.3, 0.5, 0.2, last_selex)))
  expect_length(sel, length(lengths))
  expect_true(sel[1] == exp(-10))
  expect_equal(sel[length(sel)], last_selex, ignore_attr = TRUE)
  expect_true(all(sel <= 0.5))
  expect_true(!any(is.na(sel)))
})
