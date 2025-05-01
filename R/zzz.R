# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("-----------------------------------------------\n",
#                         "dsmextraLite: version 0.0.9002:\n",
#                         "  A trimmed version of the dsmextra package.\n",
#                         "-----------------------------------------------\n",
#                         "* Please see:\nBouchet PJ, Miller DL, Roberts JJ, Mannocci L, Harris CM, Thomas L (2020). dsmextra: Extrapolation assessment tools for density surface models. Methods in Ecology and Evolution, 11(11): 1464-1469. DOI: 10.1111/2041-210X.13469\n",
#                         "\n* Further information:\ndsmextra is an output of the DenMod project. For more details, please visit:\nhttps://synergy.st-andrews.ac.uk/denmod/")
# }

.onAttach <- function(library, pkgname)
{
  info <-utils::packageDescription(pkgname)
  package <- info$Package
  version <- info$Version
  date <- info$Date
  packageStartupMessage(
    paste0(paste(package, version, sep=" "), "\n"),
    "A trimmed version of the dsmextra package.\n",
    "Please see:  https://github.com/densitymodelling/dsmextra")
}



