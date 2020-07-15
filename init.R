install.packages(c("arules","ggplot2","lubridate","ggplot2","gridExtra","arulesViz","plotly"), dependencies=TRUE)

my_packages = c("arules","ggplot2","lubridate","ggplot2","gridExtra","arulesViz","plotly")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing))
