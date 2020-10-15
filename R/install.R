# first install package devtools - it will install other packages
install.packages('devtools')

# install package Peaks from archive version (this step on windwos requires Rtools 4.0.0 installed)
devtools::install_version('Peaks', version = '0.2', repos = "http://cran.us.r-project.org")

# Install package EBImage from Bioconductor (it enables loading and showing tiff files in the app)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")
BiocManager::install("EBImage")

# install package findpeaks (this way you also update findpeaks package)
devtools::install_github('astrzalka/findpeaks')

# run app :)
findpeaks::run_app()