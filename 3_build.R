setwd("~/git/ikde-scripts")

devtools::build("../ikde", ".")
system("R CMD Rd2pdf ../ikde --force --output=ikde.pdf --no-preview .")
