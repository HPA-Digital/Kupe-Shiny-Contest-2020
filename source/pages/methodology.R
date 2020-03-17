pageMethodologyUI <- function(id) {
  ns <- NS(id)
  createFlexPage(ns("methodology"),
                          fluidRow(class="content-pad",
                                   fluidRow(
                                     uiOutput(ns("breadcrumb"))
                                   ),
                            column(7,
                                   div(class="indicator-info-app-title large-title",
                                       "Method"
                                       ),
                                   uiOutput(ns("page"))
                                   ),
                            column(5, class="pull-right",
                                   uiOutput(ns("references"))
                            )
                          ),
                        "Methodology",
                        T
  )
}