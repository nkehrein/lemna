test_that("simulate scenarios", {
  res <- lemna(metsulfuron)
  expect_s3_class(res, "data.frame")
  expect_s3_class(res, "lemna_result")
  expect_equal(nrow(res), length(metsulfuron$times))
  expect_equal(res$time, metsulfuron$times)

  # test overwriting scenario properties
  res <- lemna(metsulfuron, init=c(BM=0,M_int=1), nout=0)
  expect_equal(unclass(res[1,]), list(time=0, BM=0, M_int=1), ignore_attr=TRUE)

  res <- lemna(metsulfuron, times=c(0,1,2,3), nout=0)
  expect_equal(res$time, c(0,1,2,3))

  # ...
})

test_that("simulate custom", {
  res <- lemna(init=metsulfuron$init,
               times=metsulfuron$times,
               param=metsulfuron$param,
               envir=metsulfuron$envir)
  expect_s3_class(res, "data.frame")
  expect_s3_class(res, "lemna_result")
  expect_equal(nrow(res), length(metsulfuron$times))

  res_sc <- lemna(metsulfuron)
  expect_equal(res_sc, res)
})

test_that("ODE modes", {
  res <- lemna(metsulfuron, ode_mode="r", nout=0)
  expect_equal(names(res), c("time","BM","M_int"))
  res <- lemna(metsulfuron, ode_mode="r", nout=1)
  expect_equal(names(res), c("time","BM","M_int","C_int"))
  res <- lemna(metsulfuron, ode_mode="r", nout=2)
  expect_equal(names(res), c("time","BM","M_int","C_int","FrondNo"))

  # todo test C ODEs
})
