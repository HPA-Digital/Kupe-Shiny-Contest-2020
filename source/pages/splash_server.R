# Import libraries
library(tidyr)
library(dplyr)
library(DT)
library(shinyjs)


# Server function for pageSplash provides the backend for the Splash UI page.
# 
# @param input - values from the front-end selected by user interaction
# @param output - values outputed by the server in response to the inputs
# @param session - runtime environment for the application

pageSplash <- function(input, output, session) {
  ns <- session$ns
  
  
  # Triggers when one of the links is clicked
  onclick("links", function(event){
    if(!is.null(input$links)){
      shinyjs::runjs("window.scrollTo(0, 0)")
      change_page(convert2Query(input$links))
    }
    })
  
  
  # Renders a container for the Topic cards and dynamically renders a card for each Topic in G_topicDescriptions
  output$topicCards <- renderUI({
    # Create the container
    div(style="padding: 0 6% 35px", 
        div(style="padding: 25px 0"),  #This div is just a buffer
          div(class="flex-page-vcenter",
              div(class="card-container",
                  # For each Topic in the descriptor file, create a card for it. 
                  cards <- lapply(1:nrow(G_topicDescriptions), function(i){
                            # Pass in the Topic name and its description
                            createCard(G_topicDescriptions[i, 1][[1]], G_topicDescriptions[i, 2][[1]])
                  })
              )
          )
    )
  })
}
