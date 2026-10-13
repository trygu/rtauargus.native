.onLoad <- function(libname, pkgname) {

  # options du package : initialisees au chargement seulement
  #   si n'existent pas deja dans les options de R
  # op.rtauargus defini dans options.R

  op <- options()
  toset <- !(names(op.rtauargus) %in% names(op))
  if (any(toset)) options(op.rtauargus[toset])

  invisible()

}

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    '\n',
    'Tau-Argus : native headless engine (tauargus-engine)\n',
    '\n',
    '  Batches are executed in-process by the bundled native engine;\n',
    '  no TauArgus.exe installation is required.\n\n',
    '  To display all the options of the package :\n',
    '    rtauargus_options()',
    '\n'
  )

}

.onUnload <- function(libpath) {

  options(purrr::map(op.rtauargus, ~ NULL))
  packageStartupMessage('rtauargus : package options disabled')

}
