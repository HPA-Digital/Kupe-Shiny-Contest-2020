G_helpLightbox <- lightboxBody(
  "help-modal",
  div(class="container-fluid", style="margin-top: -30px; width:90%",
      tagList(
        h3(class = "mh3", "Help"),
        p(
          tags$b(style="margin-right: -3px","Please note"), ": Kupe may time-out if left inactive for a while or when the internet connection is interrupted,
          which will cause the screen to grey-out. To use the app again, refresh the page. "
        ),
        p(
          "Kupe was mainly developed for desktop use. The screens may look different on mobile devices."
        ),
        p(
          tags$b("Using links:"),"Text in", tags$span(style="color:#337ab7", "light blue"), "in Kupe indicates a clickable link either 
          to another page or to a document. Make sure pop-up blockers in your browser are disabled to use this feature."
        ),
        h4(class = "modal-section", style="margin-top: 30px;", "How do I navigate the Kupe data explorer?"),
        htmltools::withTags(ul(class="help-body-ui",
                               li(
                                 p("On the", tags$b("Home"), "page you can directly access the topics. Click on a topic box to go to the respective", tags$b("Topic"), "page (1)."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_1.gif", width = "100%")
                                 ),
                               li(
                                 p("On the", tags$b("Topic"), "page, click on the subtopic you are interested in to go to the respective page (2)."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_2.gif", width = "100%")
                               ),
                               li(
                                 p("On the", tags$b("Subtopic"), "page, click on the indicator you are interested in to go to the respective page (3). 
                                   You can also navigate between subtopics using the drop-down menus at the top of the page (4)."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_3_4.gif", width = "100%")
                               ),
                               li(
                                 p("On the", tags$b("Indicator"), "page you can navigate between indicators using the drop-down menus at the top of the page (5)."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_5.gif", width = "100%")
                               )
                               )),
        h4(class = "modal-section", tags$br(), "How do I access topic and indicator information?"),
        htmltools::withTags(ul(class="help-body-ui", 
                               li(
                                 p("On the", tags$b("Indicator"), "page you can navigate between the tabs 'Overview', 
                                   'Prevalence/mean', 'Subgroups comparison', and 'Changes over time' (6).")
                               ),
                               li(
                                 p("You can also access information about indicator definitions in the More/less information (7)."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_6_7.gif", width = "100%")
                                 )
                               )),
        
        h4(class = "modal-section", tags$br(), "How can I access exact values for the data points in the charts?"),
        htmltools::withTags(ul(class="help-body-ui",
                               li(
                                 p("The", tags$b("Indicator"), "page presents a series of charts. 
                                   Hover over the data points or bars in the charts 'Time trends' (8), 
                                   'Age and sex' (9) and 'Ethnicity (total)' (10) to get exact values (11)."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_8-11.gif", width = "100%")
                               ),
                               li(
                                 p("By clicking on an item in the legend (12) a subgroup can be hidden or shown in the chart."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_12.gif", width = "100%")
                               )
                               )),
        h4(class = "modal-section", tags$br(), "What downloads are available?"),
        htmltools::withTags(ul(class="help-body-ui",
                               li(
                                 p("Note: All downloadable data is at an aggregated level; no microdata is available on Kupe.")
                                 ),
                               li(
                                 p("If you want to download data for all available indicators as well as related publications, 
                                   go to the", tags$b("Method"), "page (13). There, the following data is available for download: 
                                   'Prevalence/mean', 'Subgroups comparison' and 'Changes over time'.")
                               ),
                               li(
                                 p("You can download customised csv datasets from the", tags$b("Subtopic"), "page (14) and from the", tags$b("Indicator"), "page (15) 
                                   in tabs Prevalence/Mean, Subgroups comparison, and Changes over time.")
                               ),
                               li(
                                 p("You can download customised charts in html format from the", tags$b("Indicator"), "page (16) in the tab Overview."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_13_14.gif", width = "100%"),
                                 p(),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_15.gif", width = "100%"),
                                 p(),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_16.gif", width = "100%")
                                 )
                               )),  
        h4(class = "modal-section", tags$br(), "Where do I get more information about the data displayed?"),
        htmltools::withTags(ul(class="help-body-ui",
                               li(
                                 p("Some of the chart and table headings have", tags$b("Info"), "buttons added (17). 
                                   Click on an Info button to get further ad-hoc (statistical) information."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_17.gif", width = "100%")
                                 ),
                               li(
                                 p("Some of the tables contain footnotes with further explanations. You may have to scroll down the page to see the footnote.")
                                 ),
                               li(
                                 p("Go to the", tags$strong("Method"),"page for an overview of the sample design and analysis. You will also find related publications and 
                                   information on any revisions to previously published data here."),
                                 tags$img(class="img_help", src = "img/help/HPA_HelpScreenshots_Methods.gif", width = "100%")
                                 
                               )
        )) 
        )

       ),
  
  "Help")
