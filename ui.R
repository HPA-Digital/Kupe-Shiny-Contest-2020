# This is the user-interface definition of a Shiny web application.
# SR set library paths before running this app
# .libPaths(c("R:/R/R packages", .libPaths()))
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(V8)
#library(shinyrouter)


applyGrid <- "shinyjs.applyGrid = function() {
  var isFirefox = typeof InstallTrigger !== 'undefined';
  if(isFirefox){
      document.querySelector('.card-container').classList.add('grid')
      document.querySelectorAll('.new-card').forEach(function(el){
          el.classList.add('grid')
      });
  }
}"

shinyUI(
  
  tagList(
    useShinyjs(),
    extendShinyjs(text = applyGrid),
    bootstrapPage(NULL, theme = "css/bootstrap.min.css"),#forces BS theme and BS JS libraries to lazy load
    icon(NULL, class = NULL, lib = "font-awesome"),
    tags$head(
      # Favicon icon
      tags$link(rel="icon", href="img/logos/HPA Graphic Only__Green_RGB.png"),
      # Google Analytics
      tags$script(src="js/googleanalytics.js"),
      # End Google Analytics
      tags$script(src="js/IndicatorOverview.js"),
      tags$script(src="js/TopicOverview.js"),
      tags$script(src="js/customLink.js"),
      tags$script(src="js/verticalTabs.js"),
      # tags$link(rel = "stylesheet", type = "text/css", href = "css/font-awesome.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/key-points-custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/indicator-custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/experiment.css"),
      tags$link(rel = "stylesheet", href = "https://use.typekit.net/guf3xso.css")
    ),
    # No script google analytics
    div(HTML('<noscript><iframe src="https://www.googletagmanager.com/ns.html?id=GTM-MPL9S8J"
             height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>')),
    div(id="loadingOverlay",
        HTML("<i id='loadingSpinner' class='fa fa-refresh fa-spin fa-4x fa-fw'></i>")
    ),
    
    router_ui(),
    G_aboutLightbox,
    G_helpLightbox,
    G_feedbackLightbox,
    G_contactLightbox,
    G_privacyLightbox,
    G_termsLightbox,
    tags$script(src="js/util.js")
  )
)
