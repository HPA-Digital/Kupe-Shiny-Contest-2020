library(plotly)

pageExploreIndicatorsUI <- function(id) {
  ns <- NS(id)
  
  createFlexPage(ns("subgroupResults"),
                        fluidRow(class="content-pad",
                                 column(12,
                                        div(class="container-fluid",
                                            createFilterGroup(ns("filters"),
                                                              createFilterPanel(ns("PopAndInd"),
                                                                                div("Topic, Subtopic, and indicator"),
                                                                                selectInput(ns("topic"), "Choose a topic", NULL, selectize = F), #will be updated from server
                                                                                selectInput(ns("subtopic"), "Choose a subtopic",  NULL, selectize = F),
                                                                                selectInput(ns("indicator"), "Choose an indicator", NULL, selectize = F), #will be updated from server
                                                                                collapsed = F,
                                                                                interactive = F)
                                            ),
                                            fluidRow(
                                              div(class="col-sm-12 breadcrumb",
                                                  uiOutput(ns("breadcrumb"))
                                              )
                                            ),
                                            fluidRow(
                                              div(class="col-sm-6", style="height: 55px",
                                                  uiOutput(ns("topicHeading"))
                                              ),
                                              div(class="col-sm-4", " "),
                                              div(class="col-sm-2", style="margin-top: -20px;", uiOutput(ns("downloads")))
                                            ),
                                            fluidRow(
                                              column(10,
                                                     uiOutput(ns("summary"))
                                              )
                                            )
                                        ),
                                        tabsetPanel( type = "pills", id=ns("tabset"),
                                                     tabPanel("Overview",
                                                              lightboxBody(ns("lb-timeTrends"),
                                                                           div(
                                                                             p("This chart presents unadjusted results; that is the overall prevalence estimate for each survey year. Data points only appear where there is data available."),
                                                                             p("The comparison between years is only available where the same questions were asked in more than one survey year. ")
                                                                           )
                                                               ),
                                                              lightboxBody(ns("lb-ARR"),
                                                                           div(
                                                                             p("Total response ethnicity means that people who reported belonging to more than one ethnic group are counted once in each group they reported.")
                                                                           )
                                                              ),
                                                              lightboxBody(ns("lb-ageDist"),
                                                                           "This chart presents the age and sex distribution. "
                                                              ),
                                                              div(class="container-fluid indicator-overview-bg",
                                                                  div(class="survey-heading",
                                                                      uiOutput(ns("overviewTitle"))
                                                                  ),
                              
                                                                  conditionalPanel("output.showOverview", ns=ns,
                                                                                   div(class="row flex-row",
                                                                                       div(class="col-sm-12 col-md-6 indicator-overview-card",
                                                                                           div(class="indicator-overview-card-content",
                                                                                               div(class="pull-right", infoLightboxBtn(ns("lb-overviewChart"))),
                                                                                               uiOutput(ns("overviewChartRender"))
                                                                                           )
                                                                                       ),
                                                                                       div(class="col-sm-12 col-md-6 indicator-overview-card",
                                                                                           div(class="indicator-overview-card-content",
                                                                                               div(class="pull-right", infoLightboxBtn(ns("lb-timeTrends"))),
                                                                                               div(class="overview-card-header", "Time trends"),
                                                                                               div(class="chart-subtitle"),
                                                                                               uiOutput(class = "overview-indicator-mean-unit", ns("TimeSeriesMeanUnit")),
                                                                                               plotOutput(ns("rightWidth"), height = 0, width = "95%"),
                                                                                               uiOutput(ns("overviewTS"), height="auto")
                                                                                               )
                                                                                           )
                                                                                       ),
                                                                                   uiOutput(ns("totalGraphs"))
                                                                                       ),
                                                                  fluidRow(
                                                                    tags$p(style = "color: #9c9c9c; margin: 30px 0",
                                                                           "Source: Health and Lifestyles Survey")
                                                                  ),
                                                                  uiOutput(ns("overviewNotes"))
                                                              )
                                                              
                                                     ),
                                                     tabPanel(
                                                       "Prevalence / mean",
                                                        div(
                                                         class = "container-fluid",
                                                         fluidRow(
                                                           div(
                                                             class = "col-sm-12 col-md-7",
                                                             uiOutput(ns("prevTableTitle")),
                                                             uiOutput(ns("prevTableSubtitle"))
                                                           ),
                                                           uiOutput(ns("prevColsRender"), class =
                                                                      "pull-right")
                                                         ),
                                                         fluidRow(uiOutput(ns("yearPrevalence"))),
                                                         fluidRow(dataTableOutput(ns("prevalence.table"))),
                                                         fluidRow(
                                                           uiOutput(ns("prevNotes"))
                                                         )
                                                       )
                                                     ), 
                                                     tabPanel("Subgroups comparison",
                                                              lightboxBody(ns("lb-sub-ARR"), 
                                                                  div(
                                                                    uiOutput(ns("subgroupsLightboxText"))
                                                                )),
                                                              div(class="container-fluid",
                                                                  fluidRow(
                                                                    div(class="col-sm-12 col-md-7",
                                                                        uiOutput(ns("subgroupsCompTitle"))
                                                                        
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    dataTableOutput(ns("comparison.table"))
                                                                  ),
                                                                  uiOutput(ns("comparisonNotes"))
                                                              )
                                                     ),
                                                     tabPanel("Changes over time",
                                                              lightboxBody(ns("lb-ts-sig"),
                                                                           tagList(
                                                                             p("Statistical significance between survey years is measured at the 5% significance level (", span(style="margin-left: -3px;", class="italic", "p"), " value \u003C .05). Statistically significant differences are shown in bold."),
                                                                             p("Age-standardised results are not used so analytical procedures do not take into account the changing age structures in the underlying populations over time."),
                                                                             p("A statistically significant difference is likely to represent a real change over time, rather than random variation due to the sampling process.")
                                                                             
                                                                           )),
                                                              div(class="container-fluid",
                                                                  fluidRow(
                                                                    div(class="col-sm-12 col-md-7",
                                                                        uiOutput(ns("changeOTInfo"))
                                                                    ),
                                                                    div(class="pull-right",
                                                                      uiOutput(ns("pValueCheckbox"))
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    dataTableOutput(ns("timeseries.table"))
                                                                  ),
                                                                  fluidRow(
                                                                    uiOutput(ns("timeseriesNotes"))
                                                                  )
                                                              )
                                                     )
                                        )
                                 )
                                 ),
                        "Explore indicators",
                        T
  )
}
