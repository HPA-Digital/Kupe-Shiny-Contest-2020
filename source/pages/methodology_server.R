pageMethodology <- function(input, output, session) {
  ns <- session$ns
  
  
  output$breadcrumb <- renderUI({
    div(class='breadcrumb',
        # Breadcrumb Item: Home
        span(class="breadcrumb-item", 
             tags$a(href="", `data-toggle`="tab",
                    tags$i(id = "home_bread", class="fa fa-home")
             )
        ), ">",
        tags$span(
          class='breadcrumb-item breadcrumb-focus',
          "Method"
        )
    )
  })
  
  output$page <- renderUI({
    div(
      div(class="method-main-section",
          h2("Overview of survey design"),
          div(class="method-section",
              p(
                #
                "This section provides an overview of the survey design for the latest year of the Health and Lifestyles Survey 
                (HLS; click ", tags$a(target="_blank", href="https://www.hpa.org.nz/sites/default/files/Health%20and%20Lifestyles%20Survey%20Methodology%20Report%202018.pdf", style="color: #02a7c9; text-decoration: underline", target="_blank", "here"), "for further details)."
              )
          ),
          div(class="method-section",
              h3("When did the survey take place?"),
              p("The 2018 interviews were completed between 2 May and 10 October 2018.")
          ),
          div(class="method-section",
              h3("How were people selected for the survey?"),
              p("The 2018 HLS was designed to be nationally representative. 
                A primary consideration in the sample design was the need for sufficient 
                sample sizes of ethnic subgroups: M\u0101ori, Pacific people and people of European/Other ethnicities.
                The survey used a three-stage selection procedure: stratifying and selecting meshblocks; 
                selecting households from each meshblock; and selecting an individual from within each 
                household to complete the questionnaire. One adult and one parent/caregiver (if any) 
                were selected from the lists of those who were eligible in each household using the 
                following protocols. First a parent/caregiver was selected if there were children living in 
                the house. An adult was then selected for the Adult survey. Sometimes the same respondent 
                completed both the Adult and the Parent/caregiver survey. It was also possible that the 
                two surveys were completed by two different people in the same household."
              )
          ),
          div(class="method-section",
              h3("How many people took part?"),
              HTML("<p>Of those invited to participate in the survey in 2018, 75% of adults (<span class='italic'>n </span>= 2,725) and 81% 
                of parents or primary caregivers (<span class='italic'>n </span>= 827) participated. The table below summarises the number of survey respondents in 2018 by ethnicity and sex.</p>"
              ),
              tags$table(class="method-table",
                tags$thead(
                  tags$tr(
                    tags$th(rowspan="2",
                      "Ethnicity (prioritised)"
                    ),
                    tags$th(rowspan="2", class="left",
                      "Sex"
                    ),
                    tags$th(colspan="2", class="center",
                      "Adult"
                    ),
                    tags$th(colspan="2", class="center",
                        "Parent/caregiver"
                    )
                  ),
                  tags$tr(
                    tags$th("Actual", class="right"),
                    tags$th("Weighted", class="right"),
                    tags$th("Actual", class="right"),
                    tags$th("Weighted", class="right")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(
                      rowspan="2",
                      paste0("M",intToUtf8(0x0101L),"ori")
                    ),
                    tags$td("Male", class="left"),
                    tags$td("201"),
                    tags$td("170"),
                    tags$td("64"),
                    tags$td("67")
                 ),
                 tags$tr(
                   tags$td("Female"),
                   tags$td("362"),
                   tags$td("187"),
                   tags$td("172"),
                   tags$td("104")
                 ),
                 tags$tr(
                   tags$td(
                     rowspan="2",
                     paste0("Pacific")
                   ),
                   tags$td("Male", class="left"),
                   tags$td("164"),
                   tags$td("76"),
                   tags$td("63"),
                   tags$td("26")
                 ),
                 tags$tr(
                   tags$td("Female"),
                   tags$td("306"),
                   tags$td("80"),
                   tags$td("168"),
                   tags$td("45")
                 ),
                 tags$tr(
                   tags$td(
                     rowspan="2",
                     paste0("Asian")
                   ),
                   tags$td("Male", class="left"),
                   tags$td("114"),
                   tags$td("201"),
                   tags$td("32"),
                   tags$td("50")
                 ),
                 tags$tr(
                   tags$td("Female"),
                   tags$td("131"),
                   tags$td("209"),
                   tags$td("43"),
                   tags$td("59")
                 ),
                 tags$tr(
                   tags$td(
                     rowspan="2",
                     paste0("European/Other")
                   ),
                   tags$td("Male", class="left"),
                   tags$td("571"),
                   tags$td("883"),
                   tags$td("91"),
                   tags$td("185")
                 ),
                 tags$tr(
                   tags$td("Female"),
                   tags$td("876"),
                   tags$td("919"),
                   tags$td("194"),
                   tags$td("292")
                 )
                )
              ),
              p("The sample sizes for each HLS were: ", 
                HTML("<span class='bold'>2006</span>: 1,973"),
                HTML("<span class='bold'>2008</span>: 1,608"),
                HTML("<span class='bold'>2010</span>: 1,740"),
                HTML("<span class='bold'>2012</span>: 2,672"),
                HTML("<span class='bold'>2014</span>: 2,594"),
                HTML("<span class='bold'>2016</span>: 3,854"),
                HTML("<span class='bold'>2018</span>: 2,725")
              )
          ),
          div(class="method-section",
              h3("How was data collected?"),
              p("The most recent survey data was collected by professional social research surveyors from CBG Health Research Ltd. 
                Data collection involved a face-to-face interview in the survey respondents' homes. 
                Interviewers entered responses directly into laptop computers, with some questions being completed by the 
                respondents independently. Showcards with predetermined response categories were used to assist respondents 
                where appropriate."
              )
          )
      ),
      div(class="method-main-section",
          h2("Overview of analysis"),
          div(class="method-section",
              p(
                "This section provides a brief description of the methods and derived variables used for the HLS data within Kupe. A full methodology report and specific analyses of all HLS publications can be viewed on  
                ", tags$a(href="https://www.hpa.org.nz/research-library/research-publications", target="_blank", "HPA's website", style="margin-right: -3px;"),"."
              )
          ),
          div(class="method-section",
              h3("Ethnicity"),
              p("Both total response and prioritised ethnicity have been used in Kupe. "
              ),
              tags$ul(
                tags$li("Total response ethnicity means that people who reported belonging to more than one ethnic group are counted once in each group they reported."),
                tags$li("Prioritised ethnicity means that a respondent is allocated to a single ethnicity even when they reported belonging to more than one ethnic group. The prioritised order is  M\u0101ori, Pacific, Asian, then European/Other. ")
              ),
              p(
                "Further details of these output options are in the Health Information Standards Organisation (HISO) Ethnicity Data Protocols (Ministry of Health, 2017)."
              )
          ),
          div(class="method-section",
              h3("Deprivation"),
              p("NZDep2013 is a small-area-based index. It provides a measure of neighbourhood deprivation, by looking at the comparative socioeconomic positions of small areas and assigning them decile numbers, from least deprived (1) to most deprived (10). The index is based on nine socioeconomic variables from the 2013 Census. Please click 
                ", tags$a(href="https://www.health.govt.nz/our-work/populations/maori-health/tatau-kahukura-maori-health-statistics/nga-awe-o-te-hauora-socioeconomic-determinants-health/neighbourhood-deprivation", target="blank", "here")," for more information. ")
          ),
          div(class="method-section",
              h3("Subgroups comparison for binary indicators"),
              p("The aim of these analyses was to understand how the risk by each indicator varied across demographic subgroups (such as sex) while adjusting for other factors such as age."),
              p("We used a quasi-Poisson regression model with a logarithm link function (Lumley, 2011) to estimate relative risks (RRs) and related 95% confidence intervals (CIs) for binary indicators."),
              p("We replaced estimates by dashes when we had any of the following indications of unreliability: "),
              tags$ul(
                tags$li("Fewer than 10 \"events per parameter\" in the model. This follows the guideline for logistic regression outlined in Hosmer et al (2013), pp407-408."),
                tags$li("Unusually wide confidence intervals, that is, whenever the width of the CI is greater than or equal to 4 times the ratio estimate."),
                tags$li("We applied a conservative approach to ratio estimates and suppressed those that were greater than 7 or less than 1/7.")
              )
          ),
          div(class="method-section",
              h3("Changes over time"),
              p("The prevalence/mean of each indicator was compared with the most recent available survey year if at least two data points were available. 
                For cases where there was a small sample size (", span(style="margin-left: -3px", class="italic", "n"), " < 30) in a particular subgroup, 
                       any differences between that group and others are not commented on in the results. This is because the 
                       small sample size means that the results are subject to a very wide margin of error."
              ),
              p("Some of the variation of estimated prevalences/means between survey years could potentially be from changes in the questionnaire")
          ),
          div(class="method-section",
              h3("Weighting adjustment"),
              p("Statistical selection weighting adjustments were applied to each HLS dataset to compensate for selection bias. 
                Post-stratification weight was used to ensure that findings from the survey are representative of the New Zealand 
                population with respect to major demographic characteristics such as sex, age, and ethnicity."
              )
          ),
          div(class="method-section",
              h3("Reliability of survey results - sampling error"),
              p("We show 95% confidence intervals (95% CI) to indicate the uncertainty in an estimate due to collecting data from only a sample of the population.")
          ),
          div(class="method-section",
              h3("Reliability of survey results - non sampling error"),
              p("Findings are likely to under- or overestimate some indicators due to the nature of self-reported information. For instance, when a question was asked 'thinking about the last 12 months, how often have you felt that you might have a problem with gambling?' the respondents then responded with either never, sometimes, most of the time, or almost always. Depending on what the respondent considers to be socially desirable, this can lead to over-reporting of good behaviours or under-reporting of risk behaviours. Also asking for the last 12 months assumes respondents can accurately recall previous events over the time period which may not be the case."
              )
          ),
          div(class="method-section",
              h3("Survey results show associations rather than cause-and-effect relationships "),
              p("Kupe provides a snapshot of the lifestyles among New Zealand adults at one point in time. Results can be used to look at associations between different factors, such as gambling harm and neighbourhood deprivation. It does not look at cause-and-effect relationships."
              ),
              p("For example, if we find out that a problem with gambling is more common in people living in deprived areas, it does not mean that problem gambling is caused by living in deprived areas.")
          )
        ),
        div(class="method-main-section",
            h2("Revisions to previously published data"),
            div(class="method-section",
                p("Findings obtained from Kupe may differ from previous HPA reports and between Kupe releases for the following reasons: "),
                tags$ul(
                  tags$li(
                    "Subgroups comparison: We apply quasi-Poisson models to calculate risk ratios, as described in the Overview of analysis section. The technique has not been used in HPA's previous reports."
                  ),
                  tags$li(
                    "Benchmarking: Each survey year is now benchmarked to the estimated resident population of New Zealand at the time of the survey year, whereas, in reports before 2016 the survey data was benchmarked to the most recent New Zealand census population."
                  ),
                  tags$li(
                    "Extreme selection weights: As part of an on-going data quality assurance, in 2018, we trimmed extreme selection weights of all survey years. This was to reduce the overall variation in weights and to increase the reported precision of the estimates."
                  ),
                  tags$li("Confidence Intervals: A modified version of the binomial intervals as described in Korn and Graubard (1998) is used in Kupe.")
                )
            ),
            div(class="method-section",
                h3("References"),
                p(
                  "Lumley, T. (2011). ", 
                  span(class="italic", "Complex surveys: a guide to analysis using R"),
                  "(Vol. 565). Hoboken, New Jersey: John Wiley & Sons."
                ),
                p(
                  "Ministry of Health. (2017).", tags$i("HISO 10001:2017 Ethnicity data protocols"), ". Wellington: Ministry of Health. " ,
                  tags$a(href="https://www.health.govt.nz/publication/hiso-100012017-ethnicity-data-protocols", target="_blank", "www.health.govt.nz/publication/hiso-100012017-ethnicity-data-protocols")
                ),
                p(
                  "Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). ", tags$i("Applied logistic regression (Third edition)"), ". Hoboken, New Jersey: Wiley."
                  ),
                p(
                  "Korn, E. L., & Graubard, B. I. (1998). Confidence intervals for proportions with small expected number of positive counts estimated from survey data. ", tags$i("Survey Methodology "), "24(2): 193-201."
                )
            )
        )
    )
  })
  
  
  output$references <- renderUI({
    div(
      div(class="method-section",
          div(class="indicator-info-app-title", "Related publications"),
          tags$table(class="ref-table",
                     tags$tr(
                       tags$td(class="bold", "Methodology reports:"),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/Health%20and%20Lifestyles%20Survey%20Methodology%20Report%202018.pdf", target="_blank", "2018")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/2016 Health and Lifestyles Survey - Methodology Report.pdf", target="_blank", "2016")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/2014 HLS Methodology.pdf", target="_blank", "2014")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/Methodology report for the 2012 HLS V5 %283%29.pdf", target="_blank", "2012")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/HLS%202010%20methodology-report-fnl-110128.pdf", target="_blank", "2010")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/HLS%202008-methodology-report-fnl-100625.pdf", target="_blank", "2008")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/06-07%20GBAS%20Method%20Report-fnl-090707_LR.pdf", target="_blank", "2006"))
                     ),
                     tags$tr(
                       tags$td(class="bold", "Questionnaires:"),
                       # TODO: Add link for 2018 Questionnaire repoprt when URL is given
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/FINAL%20HLS%20Questionnaire%20Nov%202018.pdf", target="_blank", "2018")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/2016 Health and Lifestyles Survey Questionnaire_0.pdf", target="_blank", "2016")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/2014-HLS-questionnaire-web.pdf", target="_blank", "2014")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/2012 HLS Questionaire FA.pdf", target="_blank", "2012")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/HLS%202010-Questionnaire-final-100510.pdf", target="_blank", "2010")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/HLS%202008-Questionnaire-fnl-080729-1.pdf", target="_blank", "2008")),
                       tags$td(tags$a(href="https://www.hpa.org.nz/sites/default/files/06-079%20GBAS%20Questionnaire.pdf", target="_blank", "2006"))
                       # TODO: the 2006/7 link doesn't work yet 
                     ),
                     tags$tr(
                       tags$td(class="bold", "	Other publications:")
                     )
          ),
          # TODO: Update link and text to 2018 if required 
          p(tags$a(href="https://www.hpa.org.nz/sites/default/files/Final-Report_Results-from-2016-Health-And-Lifestyles-Survey_Gambling-Feb2018.pdf", target="_blank", "Gambling report: Results from the 2016 Health and Lifestyles Survey", style="margin-right:-3px; margin-bottom:10px;"),".")
      ),
      div(class="method-section", style= "margin-top: 40px;", 
          div(class="indicator-info-app-title", "Related data sets",
              tags$a(href="data/prevalence-mean_kupe.csv", target="_blank", "Prevalence / Mean"),
              tags$a(href="data/subgroups-comparison_kupe.csv", target="_blank", "Subgroups comparison"),
              tags$a(href="data/changes-over-time_kupe.csv", target="_blank", "Changes over time")
              )
      )
    )
  })
  
}