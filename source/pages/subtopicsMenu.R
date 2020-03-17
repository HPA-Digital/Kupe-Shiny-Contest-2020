library(plotly)



# SubtopicsMenu page for HPA Kupe data Explorer.
# From this page the user can select a Subtopic of the Topic selected in pageSplashUI to explore.
pageSubtopicsMenuUI <- function(id) {
  ns <- NS(id)
  
  # Create the page and structure
  createFlexPage(ns("subgroupResults"),
                 fluidRow(class = "content-pad",
                          column(12,
                            div(class = "container-fluid",
                                
                                # Create Row for selecting topic and subtopic
                                fluidRow(
                                  column(12,
                                         createFilterPanel(ns("PopAndInd"),
                                                           div("Choose a Topic"),
                                                           selectInput(ns("topic"), "Choose a topic", NULL, selectize = F),
                                                           collapsed = F,
                                                           interactive = F
                                         )
                                  )
                                ),
                                
                              # Create breadcrumb
                              fluidRow(uiOutput(ns("breadcrumb"))),
                              
                              # Icon and name of the Topic that was selected in Splash.
                              fluidRow(uiOutput(ns("icon")),
                                       column(8,
                                         div(style ="margin-top: 7px;margin-left: 10px;", class ="indicator-info-app-title", uiOutput(ns("topic")))
                                       )
                              ),
                              
                              # Render the cards for the Subtopics
                              fluidRow(uiOutput(ns("subtopicCards")))
                            )
                          )),
                 "Subtopic Menu",
                 T)
}