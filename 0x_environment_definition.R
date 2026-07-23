install.packages("renv")

pkgs <- renv::dependencies()$Package
pkgs <- sort(unique(na.omit(pkgs)))

writeLines(pkgs, "requirements.txt")