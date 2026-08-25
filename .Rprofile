if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (locale in c(
    "es_PE.UTF-8",
    "Spanish_Peru.utf8",
    "en_US.UTF-8",
    "English_United States.utf8",
    "C.UTF-8"
  )) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))

    if (isTRUE(l10n_info()[["UTF-8"]])) {
      break
    }
  }
}
