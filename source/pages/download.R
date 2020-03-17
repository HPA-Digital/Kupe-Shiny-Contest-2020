pageDownloadUI <- function(id) {
  ns <- NS(id)
  createFlexSidebarPage(ns("downloadPage"),
                        "",
                        p("downloads page content goes here."),
                        "Download datasets",
                        header = TRUE
                        )
}