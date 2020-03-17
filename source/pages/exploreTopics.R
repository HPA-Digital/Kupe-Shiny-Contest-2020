# Explore Topics page for HPA Kupe data Explorer.
# From this page the user can browse topics and subtopics and an overview of their respective indicators.
pageExploreTopicsUI <- function(id) {
  ns <- NS(id)
  
  # Create the page and structure
  createFlexPage(ns("exploreTopics"),
                        fluidRow(class="content-pad",
                                 lightboxBody(ns("lb-year"),
                                              p("Year of survey.")
                                              ),
                                 
                                 # Lightbox body for when the "changed between" info button is pushed
                                 lightboxBody(ns("lb-changes"), 
                                              tagList(
                                                p(
                                                  "Statistical significance is measured at the 5% significance level (", span(class="italic", "p"), "value \u003C .05)."
                                                ),
                                                p(
                                                  "Statistically significant differences between survey years (", span(class="italic", 'p'), " \u003C .05) are indicated by the symbols",
                                                  tags$i(class = 'fa fa-caret-up fa-lg'),
                                                  "and",
                                                  tags$i(class = 'fa fa-caret-down fa-lg')
                                                  ,
                                                  "(significant increase or decrease respectively)."
                                                ),
                                                p(
                                                  "The symbol",
                                                  intToUtf8(0x2248L),
                                                  "indicates there is no statistically significant difference between survey years."
                                                ),
                                                p(
                                                  "A statistically significant difference is likely to represent a real change over time, rather than random variation due to the sampling process."
                                                ))),
                                 column(12,
                                        div(class="container-fluid",
                                            
                                            # Create Row for selecting topic and subtopic
                                            fluidRow(
                                              column(12,
                                                     createFilterPanel(ns("PopAndInd"),
                                                                       div("Choose a Topic"),
                                                                          selectInput(ns("topic"), "Choose a topic", NULL, selectize = F),
                                                                          selectInput(ns("subtopic"), "Choose a subtopic", NULL, selectize = F),
                                                                       collapsed = F,
                                                                       interactive = F
                                                                       )
                                              )
                                            ),
                                            
                                            # Create the breadcrumb
                                            fluidRow(div(class="col-sm-12 breadcrumb", uiOutput(ns("breadcrumb")))),
                                            fluidRow(
                                              div(class="col-sm-6", style="height: 55px", uiOutput(ns("topicHeading"))),
                                              div(class="col-sm-4", " "),
                                              div(class="col-sm-2", style="margin-top: -20px;",
                                                  uiOutput(ns("downloads"))
                                              )
                                            ),
                                            # Create the title of the Subtopic and a description
                                            fluidRow(
                                              div(class="col-sm-12 col-md-6",
                                                  uiOutput(ns("subtopicDescription"))
                                              )
                                            ),
                                            
                                            # Create a row to hold the "prevelence for selected topic" explanation of what the tables about
                                            # as well as the dropdown to let the user change which population to work with
                                            fluidRow(
                                              div(class="col-sm-12 col-md-7",
                                                  div(class="chart-title",
                                                      style="margin-top:40px",
                                                      "Prevalence for selected subtopic"
                                                      ),
                                                  
                                                  # The explanation
                                                  p(class="chart-subtitle", 
                                                    "This table gives the percentage of the population affected (that is, the unadjusted prevalence in the specified population). Click on an indicator to find out more about it. "
                                                    )
                                              ),
                                              div(uiOutput(ns("groupSelect")))
                                            ),
                                            
                                            # Create the prevalence table.
                                            # custom-link-binding allows us to know which row of the table was clicked (through JS)
                                            fluidRow(
                                              div(class="custom-link-binding",
                                                  dataTableOutput(ns("table"))
                                              )
                                            ),
                                            
                                            # If the subtopic contains indicators which have mean data, create a datatable for that
                                            fluidRow(
                                              div(style = "margin-top: 40px",
                                              uiOutput(ns("meansTable")))
                                            ),
                                            
                                            fluidRow(
                                              div(class="source", "Source: Health and Lifestyles Survey")
                                            ),
                                            
                                            # A Disclaimer row for notes
                                            fluidRow(
                                              tags$p("Notes:"),
                                              htmltools::withTags(
                                                ul(
                                                  li("Dashes indicate that the data is not available.")
                                                )
                                              )
                                            )
                                        )
                                 )
                        ),
                        "Explore topics",
                        T
                        
  )
}