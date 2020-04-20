# init.R
#
# Example R code to install packages if not already installed
#

if ("mcmc" %in% rownames(installed.packages()) == FALSE) {
  install.packages("mcmc")
}

if ("quantreg" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantreg")
}

if ("MCMCpack" %in% rownames(installed.packages()) == FALSE) {
  packageurl <- "https://cran.r-project.org/src/contrib/Archive/MCMCpack/MCMCpack_1.4-4.tar.gz"
  install.packages(packageurl, repos=NULL, type="source")
}

my_packages = c("shinydashboard","xts","EpiEstim")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
