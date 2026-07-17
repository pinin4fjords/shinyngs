# exec_smoke_fixture()
#
# Path to a committed exec-smoke-test fixture file.

exec_smoke_fixture <- function(...) {
  testthat::test_path("fixtures", "exec-smoke", ...)
}

# exec_smoke_libpath()
#
# A library directory holding a fresh install of the current source tree.
# library(shinyngs) inside an exec/*.R script resolves against whatever is on
# the ordinary library path, which may be a stale build left over from other
# work, so subprocesses are instead pointed at this throwaway library.
# Installed once per test session and cached.

exec_smoke_libpath <- local({
  lib <- NULL
  function() {
    if (is.null(lib)) {
      pkg_root <- dirname(system.file("DESCRIPTION", package = "shinyngs"))
      new_lib <- file.path(tempdir(), "shinyngs-exec-smoke-lib")
      dir.create(new_lib, showWarnings = FALSE, recursive = TRUE)

      r_bin <- file.path(R.home("bin"), "R")
      install_output <- system2(
        r_bin,
        c(
          "CMD", "INSTALL",
          "--no-docs", "--no-byte-compile", "--no-help", "--no-test-load",
          paste0("--library=", shQuote(new_lib)),
          shQuote(pkg_root)
        ),
        stdout = TRUE, stderr = TRUE
      )
      status <- attr(install_output, "status")
      if (!is.null(status) && status != 0) {
        stop(
          "Failed to install shinyngs into a temporary library for exec smoke tests:\n",
          paste(install_output, collapse = "\n")
        )
      }
      lib <<- new_lib
    }
    lib
  }
})

# run_exec_script()
#
# Run an exec/*.R script as a subprocess against the temporary smoke-test
# library. Returns a list with `status` (integer exit code) and `output`
# (character vector of combined stdout/stderr).

run_exec_script <- function(script, args) {
  script_path <- system.file("exec", script, package = "shinyngs")
  if (!nzchar(script_path)) {
    stop("Could not locate exec/", script, " via system.file()")
  }

  lib <- exec_smoke_libpath()
  r_libs <- paste(c(lib, .libPaths()), collapse = .Platform$path.sep)

  old_r_libs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = r_libs)
  on.exit({
    if (is.na(old_r_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_r_libs)
  })

  rscript_bin <- file.path(R.home("bin"), "Rscript")
  output <- system2(
    rscript_bin,
    shQuote(c(script_path, args)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")

  list(status = if (is.null(status)) 0L else status, output = output)
}
