# Placeholder binding so tests can mock requireNamespace() via
# testthat::local_mocked_bindings() - R skips non-function bindings when
# resolving a call, so this doesn't affect the real base::requireNamespace()
# used at runtime.
requireNamespace <- NULL
