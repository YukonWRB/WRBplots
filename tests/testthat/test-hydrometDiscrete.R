#NOTE: as of June 2023 hydrometDiscrete does not accept start/end dates other than 1 and 365. This explained in the help file.

test_that("violin plot is as expected for full year with numeric startDay and endDay", {
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  unlink(dir, recursive=TRUE)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = 1, endDay = 365, years = "2022", save_path = dir))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/snow.png"))
  expect_snapshot_file(paste0(dir, "/snow.png"))
  unlink(dir, recursive=TRUE)
})

test_that("console plot output is as expected", {
  plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = 1, endDay = 365, years = "2022"))
  vdiffr::expect_doppelganger("full year violin", plot)
})

test_that("violin plot is as expected for full year with Date startDay and endDay", {
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/snow2.png"))
  expect_snapshot_file(paste0(dir, "/snow2.png"))
  unlink(dir, recursive=TRUE)
})

test_that("box plot is as expected for full year with numeric startDay and endDay", {
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = 1, endDay = 365, years = "2022", save_path = dir,  plot_type = "boxplot"))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/snow3.png"))
  expect_snapshot_file(paste0(dir, "/snow3.png"))
  unlink(dir, recursive=TRUE)
})

test_that("box plot is as expected for full year with Date startDay and endDay", {
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, plot_type = "boxplot"))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/snow4.png"))
  expect_snapshot_file(paste0(dir, "/snow4.png"))
  unlink(dir, recursive=TRUE)
})

test_that("plot scale factor and titles works", {
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, plot_type = "boxplot", plot_scale = 2, title = FALSE))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/snow5.png"))
  expect_snapshot_file(paste0(dir, "/snow5.png"))
  unlink(dir, recursive=TRUE)
})

test_that("depth plots work", {
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "SWE", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/snow6.png"))
  expect_snapshot_file(paste0(dir, "/snow6.png"))
  unlink(dir, recursive=TRUE)
})

