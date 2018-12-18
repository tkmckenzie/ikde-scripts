setwd("~/git/ikde-scripts")
devtools::check("../ikde")

# setwd("~/git/ikde")
# usethis::use_travis()

setwd("~/git/ikde-scripts")
system("R CMD check --as-cran ikde_0.0.1.tar.gz")
