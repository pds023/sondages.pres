# Disable automatic network access only for the duration of the test suite.
withr::local_options(sondages.pres.auto_download = FALSE,
                     .local_envir = testthat::teardown_env())
