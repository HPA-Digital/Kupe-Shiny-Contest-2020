
# Home page for HPA Kupe data Explorer.
# From this page the user can browse available Topics and select one if they wish to see Subtopics and Indicators 
pageSplashUI <- function(id) {
  ns <- NS(id)

  div(class="main",
    # Create Header - the one that at the top thats the same on each page
    createMainHeader(),
    
    # Create splash header - the hero banner
    tags$header(
      tags$meta(name = "Kupe",
                content = 
                  "Kia ora! Welcome to Kupe your data explorer provided by Te Hiringa Hauora, the Health Promotion Agency (HPA). "
      ),
      class = "splash-header",
      div(class = "container-fluid", style = "clear:both; margin-top: 40px;",
          div(class = "row",
            div(class = "col-sm-6", style = "color:#fff",
              #div(class = "mh2", "Kairangahau Raraunga"),
              div(class = "mh1", "Kupe data explorer"),
              tags$aside(class = "splash-subtitle",
                tags$p("Kia ora! Welcome to Kupe your data explorer provided by Te Hiringa Hauora, the Health Promotion Agency (HPA)."),
                tags$p("Kupe lets you explore Health and Lifestyles Survey data about New Zealanders' views and experiences across several topics."),
                tags$p("The name, Kupe, is inspired by the chiefly Polynesian navigator and fisherman who, according to M\u0101ori oral tradition,
                       discovered the islands of Aotearoa, New Zealand. After some difficulty catching fish off the coast of his homeland in Hawaiki,
                       Kupe learnt of a giant wheke (octopus) eating the bait from his fishing hooks. Kupe set out to capture the troublesome 
                       octopus which led to a great pursuit across the Pacific Ocean, bringing Kupe and his followers to Aotearoa, New Zealand.")
              )
              # tags$button(type = "button", class = "btn btn-outline-primary secondary white",
              #   "More about Kupe " ,
              #   HTML("<i class='link fa fa-external-link'></i>")
              # )
            )
          )
        )
    ),
    
    # Create cards for Topics
    div(id = ns("links"), class = "custom-link-binding",
        uiOutput(ns("topicCards")))
    ,
    
    # Create footer
    createFooter()
  )
}
