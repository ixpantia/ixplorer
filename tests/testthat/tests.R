testthat::test_that("git commands are well executed", {
  testthat::expect_silent(ixplorer::set_git_credentials_cache(global = TRUE,
                                                                hours = 6))

  testthat::expect_silent(ixplorer::set_git_credentials_cache(global = FALSE, hours = 8))

  testthat::expect_silent(ixplorer::set_git_timeout(timeout = 14402, global = TRUE))

  testthat::expect_silent(ixplorer::set_git_timeout(timeout = 14403, global = FALSE))

  testthat::expect_silent(ixplorer::fijar_tiempo_credenciales(pausa = 14404, global = FALSE))

  testthat::expect_silent(ixplorer::fijar_tiempo_credenciales(pausa = 14405, global = TRUE))
})

