test_that("effects BM", {
 # calculate manually
 out <- lemna(metsulfuron)
 envir2 <- metsulfuron$envir
 envir2$conc <- 0
 ctrl <- lemna(metsulfuron, envir=envir2)
 efx <- 100 * (1 - tail(out$BM, 1)/tail(ctrl$BM, 1))

 # effect levels must match
 expect_equal(effect(metsulfuron)["BM"], efx, ignore_attr=TRUE)

 # no exposure, no effect
 expect_equal(effect(metsulfuron, envir=envir2)["BM"], 0, ignore_attr=TRUE)

  # less exposure, less effect
 envir2$conc <- 0.5
 expect_lt(effect(metsulfuron, envir=envir2)["BM"], effect(metsulfuron)["BM"])

 # duration argument
 expect_equal(effect(metsulfuron, times=c(0:14,20), duration=14)["BM"], efx, ignore_attr=TRUE)
})

test_that("effects r", {
  out <- lemna(metsulfuron)
  envir2 <- metsulfuron$envir
  envir2$conc <- 0
  ctrl <- lemna(metsulfuron, envir=envir2)

  r_exp <- log(out$BM[15] / out$BM[1])/14
  r_ctrl <- log(ctrl$BM[15] / ctrl$BM[1])/14
  efx <- 100 * (1 - r_exp/r_ctrl)

  # effect levels must match
  expect_equal(effect(metsulfuron)["r"], efx, ignore_attr=TRUE)

  # no exposure, no effect
  expect_equal(effect(metsulfuron, envir=envir2)["r"], 0, ignore_attr=TRUE)

  # less exposure, less effect
  envir2$conc <- 0.5
  expect_lt(effect(metsulfuron, envir=envir2)["r"], effect(metsulfuron)["r"])

  # duration argument
  expect_equal(effect(metsulfuron, times=c(0:14,20), duration=14)["r"], efx, ignore_attr=TRUE)
})
