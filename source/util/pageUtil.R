#This file is effectively static methods for returning frequently used HTML.
#There should be no state, everything here is considered immutable.


capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

createSummary <- function(title, definition, image, ...){
  keyvals <- NULL
  if(length(list(...))){
    keyvals <- lapply(names(...), function(name){
      div(
        span(class="info-key",
             tags$strong(name)
             ),
        span(class="info-val",
             ...[[name]]
             )
      )
    })
  }
  defn <- NULL
  if(!is.null(definition)){
    defn <- div(class="col-md-4",
        div(class="info-key", tags$strong("Indicator definition:")),
        div(class="info-val", definition)
    )
  }
  imageHTML <- NULL
  # keyValclass <- "col-xs-4"
  if(!is.null(image)){
    imageHTML <- tags$img(src=image, class="indicator-image img-responsive")
    # keyValClass <- "col-xs-8"
  }
  div(class="indicator-info-section",
      style="display:flex",
    div(style="width:64px",
        imageHTML
    ),
    div(class="container-fluid", style="width:100%",
        fluidRow(
          div(class="col-md-4",
              div(class="info-title", title),
              keyvals
          ),
          defn
          )
        )
  )
}

createFlexSidebarPage <- function(id, sidecontent, mainContent, name, header=F){
  sidebar <- NULL
  headerHTML <- NULL
  if(!is.null(sidecontent)){
    sidebar <- div(class="flex-sidebar",
                   sidecontent,
                   img(class="img-responsive nzhs-sidebar", src="img/logos/NZ_Health_Survey_Logo_vector.svg")
                   )
  }
  if(header){
    headerHTML <- createMainHeader()
  }
  tagList(
    div(class="wrapper",
        div(class="navPage",
            headerHTML,
            div(id=id, class="flex-page",
                div(class="flex-page-row",
                    sidebar,
                    div(class=if(!is.null(sidebar)){"container-fluid flex-main"}else "flex-main",
                        mainContent,
                        createFooter()
                        )
                )
            )
        )
    )
  )
}

createFlexPage <- function(id, mainContent, name, header=F){
  createFlexSidebarPage(id, NULL, mainContent, name, header)
}




createMainHeader <- function(){
  tagList(
    div(class="header-main",
        tags$ul(class="pull-right header-options col-sm-8", 
                tags$li(class="menu-item", div(a(href="https://www.hpa.org.nz","hpa.org.nz", target="_blank"))),
                tags$li(class="menu-item divider", div("")),
                tags$li(class="menu-item", div(tags$button("Method", id="method_btn", `data-toggle`="tab", href="#"))),
                tags$li(class="menu-item", div(lightboxBtn("help-modal", tags$button(href="#", "Help")))),
                tags$li(class="menu-item", div(lightboxBtn("feedback-modal", tags$button(href="#", "Feedback")))),
                tags$li(class="menu-item", div(lightboxBtn("about-modal", tags$button(href="#", "About"))))
            ),
            div(class="logo float-left",
                tags$a(id="header_img", `data-toggle`="tab", href="#",
                       tags$img(src="img/logos/2.0 Kupe - Data Explorer tool name RGB-01.svg")
                       )
            ),
            div(class="mobile-menu-button pull-right navbar-toggle",
                "data-toggle"="collapse", "data-target"="#navbar-collapsable", "aria-expanded"="false",
                tags$button(
                  HTML(
                    "<i class='fa fa-bars'></i>"
                  ))
                )
    ),
    div(class="mobile-menu",
        div(class="collapse navbar-collapse", id="navbar-collapsable", 
          tags$ul(class="nav navbar-nav navbar-small",
          tags$li(
              lightboxBtn("about-modal", tags$button(href="#", "About"))
            ),
            tags$li(
              lightboxBtn("feedback-modal", tags$button(href="#", "Feedback"))
            ),
            tags$li(
              lightboxBtn("help-modal", tags$button(href="#", "Help"))
            ),
            tags$li(
              tags$button("Method", id="method_btn", `data-toggle`="tab", href="#")
            ),
            tags$li(
              tags$button(href="https://www.hpa.org.nz","Hpa.org.nz", target="_blank")
            )
          )
        )
    )
  )
}

createCard <- function(topic, content){
 
  topicNoMacron <- topic %>% str_replace_all(paste0("M", intToUtf8(0x0101L), "ori"), "Maori")
  
  tagList(
    div(class="new-card", topic=topic, `data-toggle`="tab", 
           div(class="new-card-content",    
               div(class="new-card-topic row",
                   div(style="width: 50px; margin-right: 10px; float: left;", 
                       tags$img(class="new-card-icon", src=paste0("img/icons-color/4.1-RS037-Kupe-icons-",
                                                                  paste(toupper(substr(gsub(" ", "-", topicNoMacron), 1, 1)), tolower(substr(gsub(" ", "-", topicNoMacron), 2, nchar(gsub(" ", "-", topicNoMacron)))), sep="") ,
                                                                  ".svg"))
                   ),
                   div(style="width: calc(100% - 50px); margin: 0 0 0 50px;",
                       tags$h4(style="margin: 16px 0;", topic %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori")))
                   )
               ),
               div(class="row",
                   div(class="subtitle col-md-12",
                       p(content)
                   )
               )
           ),
           div(class="new-link-overlay custom-link", topic=topic, p(id="new-topic-link", topic=topic, HTML(paste0("Explore more"))))
      )
    
  )
}

createFooter <- function(){
  div(id="footer",
      tags$ul(class="footer-border", 
              tags$li(class="footer-color",
                      div(class="footer-border-seg", style="background-color: #84BD00")
              ),
              tags$li(class="footer-color",
                      div(class="footer-border-seg", style="background-color: #0080a4")
              ),
              tags$li(class="footer-color",
                      div(class="footer-border-seg", style="background-color: #ED8B00")
              ),
              tags$li(class="footer-color",
                      div(class="footer-border-seg", style="background-color: #FFD100")
              ),
              tags$li(class="footer-color",
                      div(class="footer-border-seg", style="background-color: #833177")
              )
      ),
      div(class="footer-content",
          
              # tags$ul(
              #   class = "footer-links",
              #   tags$li(class = "footer-links-item",
              #           div(div(
              #             lightboxBtn("contact-modal",
              #                         tags$button(
              #                           href = "#", "Contact"
              #                         ))
              #           ))),
              #   tags$li(class = "footer-links-item",
              #           div(div(
              #             lightboxBtn("privacy-modal",
              #                         tags$button(
              #                           href = "#", "Privacy"
              #                         ))
              #           ))),
              #   tags$li(class = "footer-links-item last-item",
              #           div(div(
              #             lightboxBtn("terms-modal",
              #                         tags$button(
              #                           href = "#", "Terms of use"
              #                         ))
              #           )))
              # )
              div(class="footer-links",
                  div(class="footer-links-item",
                      lightboxBtn("contact-modal", tags$button("Contact"))
                  ),
                  
                  div(class="footer-links-item",
                      lightboxBtn("privacy-modal", tags$button("Privacy"))
                  ),
               
                  div(class="footer-links-item last-item",
                      lightboxBtn("terms-modal", tags$button("Terms of use"))
                  )
              ),
          div(class="legal",
              tags$a(href="https://creativecommons.org/licenses/by/4.0/", target="_blank",
                     img(class="creative-commons", src="img/logos/cc-logo-white.png")
              ),
              p("This work is licenced under a ",
                tags$a(class="footer-text-link", href="https://creativecommons.org/licenses/by/4.0/", target="_blank", "Creative Commons Attribution 4.0 International Licence")
              ),
              p("For more information view the",
                lightboxBtn("terms-modal",
                            tags$button(style="padding: 0", class="footer-text-link",href="#", HTML("HPA copyright statement"))
                )
              )
          ),
          div(class="right-items",
              div(class="row",
                  div(class="hpa-contain",
                      tags$a(href="https://www.hpa.org.nz/", target="_blank", img(src="img/logos/hpa_logo_horizontal_white.svg"))
                  )
              ),
              div(class="row",
                  div(class="nz-govt-contain",
                      img(src="img/logos/logo_nz_govt.svg")
                  )
              )
          )
      )
  )
}


createFilterGroup <- function(id, ..., accordian=F){
  children <- list(...)
  if(accordian){
    children <- lapply(children, function(child){
      child$children[[1]] <- htmltools::tagAppendAttributes(child$children[[1]], "data-parent"=paste0("#", id))
      child
    })
  }
  
  div(class="filters", 
    fluidRow( # class="content-pad-30",
             column(12,
                    div(class="filter-group", id=id,
                        tagList(children)
                    )
                    
             )
    )
  )
}

createFilterPanel <- function(id, title, ..., collapsed=FALSE, interactive=TRUE){
  cls <- "filter-title"
  if(collapsed){cls <- paste(cls, "collapsed")}
  if(interactive){cls <- paste(cls, "interactive")}
  
  div(class="panel filter-panel filters",
      # div(class=cls,
      #     'data-toggle'=if(interactive)"collapse", 'data-target'=if(interactive)paste0("#", id),
      #     title
      # ),
      div(class=if(collapsed && interactive)"filter-body collapse" else "filter-body", id=id,
          tagList(...)
      )
  )
}

createDownloadArea <- function(links, title="Download"){
  div(class="download-area",
      tags$img(src="img/util/Download_icon_blue.svg", style="width: 32px; height: 32px;"),
      div(class="download-links",
          div(title),
          links
          )
      )
}

createIndicatorDefinition <- function(indicator, subtopic, topic, title, description, definition, question){
  imageHTML <- div(class="indicator-image img-responsive")
    lbBody <- lightboxBody("definition-modal", HTML(question))
    lbBtn <- lightboxBtn("definition-modal", a(href="#", "Full definition"))
  div(style="display:flex", class="indicator-info-section",
    div(class="container-fluid", style="width:100%",
        
        if(!is.null(description) && !is.na(description) && (is.character(description) && length(description) != 0L)){
          fluidRow(
            column(8, p(class="indicator-info-app-title", paste0("Indicator: ", title)),
                   expandingContent("summary-expand", c(definition, "<br/><br/>", question))
            )
          )
        }
    )
  )
}

chartRange <- function(max)
{
  if(max <= 2)
  {
    top <- 3.1
    dtick <- 0.5
  } 
  else if (max <= 10) {
    top <- 11
    dtick <- 2
  } else if (max <= 30) {
    top <- 31
    dtick <- 5
  } else if (max <= 60) {
    top <- 61
    dtick <- 10
  } else if (max <= 100) {
    top <- 101
    dtick <- 10
  }
  else
  {
    top <- floor(max * 0.12) * 10 + 1
    dtick <- ceiling(top/100) * 10
  }
  
  return (list(range = c(0, top), tick = dtick))
}


formatPvalue <- function (x) {
  if(is.na(x) | x == " "){
    x <- x
  }else{
    x <- as.numeric(x) %>% round(digits=3) %>% as.character()
    x <- substr(x, 2, nchar(x))
    while(nchar(x) < 4){
      x <- paste0(x, "0")
    }
    if(x == "0000"){
      x <- ".000"
    }
    return(x)
  }
  
}

formatroundL <- function(x, bit)
{
  if(is.na(x) | x == "" | x == " "){
    x <- x
  }
  else
  {
    x <- format(round(as.numeric(x), bit), nsmall=bit, scientific = F)
  }
  x
}