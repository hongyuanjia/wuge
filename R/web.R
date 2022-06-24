get_wuge_app <- function() {
    check_packages(c("shiny", "shinyvalidate"))
    shiny::shinyAppFile(system.file("www/app.R", package = "wuge"))
}
