# The installed DESCRIPTION must keep the provenance of the upstream INSEE
# rtauargus package (all authors and the copyright holder), the fork
# maintainer, and the contributors of the bundled engine and solver.

context(":::description provenance")

test_that("DESCRIPTION keeps the upstream, fork and engine credits", {

  description <- read.dcf(
    file.path(find.package("rtauargus.native"), "DESCRIPTION")
  )
  authors <- description[1, "Authors@R"]

  # Upstream INSEE rtauargus authors and copyright holder.
  for (name in c(
    "Berrard", "Jamme", "Rastout", "Pointet", "Beroud",
    "Socard", "Pomel", "Baudry",
    "Institut National de la Statistique et des"
  )) {
    expect_match(authors, name, fixed = TRUE)
  }

  # Fork maintainer.
  expect_match(authors, "Falch", fixed = TRUE)
  expect_match(authors, "cre", fixed = TRUE)

  # Bundled engine and solver contributors.
  for (name in c(
    "de Wolf", "Hundepool", "Giessing", "Castro", "Fischetti",
    "ERGO-Code"
  )) {
    expect_match(authors, name, fixed = TRUE)
  }
})
