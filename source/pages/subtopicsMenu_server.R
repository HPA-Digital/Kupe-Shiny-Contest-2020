# Imports 
library(tidyr)
library(dplyr)
library(stringr)
library(DT)


pageSubtopicsMenu <- function(input, output, session, name, topic_page) {
  ns <- session$ns
  
  topic <- reactive({
    topic_page
  })
  
  # The name of the Topic that was selected in Splash.
  output$topic <- renderUI({
    topic() %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori"))
  })
  
  # TOPIC Drop down menu change
  onclick("topic", function(event){
    if(input$topic != topic_page){
      
      change_page(paste(convert2Query(c(input$topic)), collapse = "/"))
    }
  })
  
 
  # Also when page is loaded...
  observeEvent({topic()}, {
    splashPassTopic <- topic()
    topChoices <- (unique(G_indicatorList$topic) %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori"))) %>% sort()
    topSelected <- (G_indicatorList %>% filter(tolower(topic) %in% tolower(str_replace_all(splashPassTopic, "Maori", paste0("M", intToUtf8(0x0101L), "ori")))))$topic  %>% unique()
    updateSelectInput(session, "topic", choices = topChoices, selected = str_replace_all(topSelected,"Maori",  paste0("M", intToUtf8(0x0101L), "ori"))) 
  })
  
  # SVG Icon unique for the selected Topic.
  output$icon <- renderUI({
    topicNoMacron <- topic() %>% str_replace_all(paste0("M", intToUtf8(0x0101L), "ori"), "Maori")
    img(src=paste0("img/icons-mono/2.0-RS037-Kupe-icons-", 
                   paste(toupper(substr(gsub(" ", "-", topicNoMacron), 1, 1)), tolower(substr(gsub(" ", "-", topicNoMacron), 2, nchar(gsub(" ", "-", topicNoMacron)))), sep="") , 
                   ".svg"),  
        class="topic-heading-icon")
  })
  
  # link back to home page
  onclick("breadLinks", {
    shinyjs::runjs("window.scrollTo(0, 0)")
    change_page(if(is.null(input$breadLinks)) "/" else convert2Query(input$breadLinks))
  })
  
  
  # Render the breadcrumb for this page
  output$breadcrumb <- renderUI({
    #Breadcrumb is in this format: HomeIcon > Topic
    div(class='breadcrumb',
        # Breadcrumb Item: Home
        span(class="breadcrumb-item", 
             tags$a(href="", `data-toggle`="tab",
                    tags$i(id = "home_bread", class="fa fa-home")
             )
        ), ">",
        tags$span(
          class='breadcrumb-item breadcrumb-focus',
          topic() %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori"))
        )
      )
  })
  
  # Create a container for the Subtopic cards to go into and create a card for each Subtopic for the selected Topic
  output$subtopicCards <- renderUI({
    # Get a list of the Subtopics we want to create a card for
    subtopics <- G_subtopicDescriptions %>% filter(tolower(topic)  == tolower(topic()) ) %>% select("subtopic", "app.description", "app.img.header")
    
    # Create the container. 
    # Having ns("links") allows Shiny to treat this Output as an Input when clicked, and handle the click in observeEvent(input$links)
    div(id=ns("links"), class="custom-link-binding",
        div(
            div(style="padding: 25px 0"),
            div(class="flex-page-vcenter subtopic-card-container",
                div(class="card-container subtopic-card-container",
                    
                    # For each Subtopic create a card
                    cards <- lapply(1:nrow(subtopics), function(i){
                      # Pass in the title of the Subtopic and its description 
                      createSubtopicCard(subtopics[i,1][[1]], subtopics[i,2][[1]], subtopics[i,3][[1]])
                    })
                )
            )
        )
    )
  })
  
  
  
  # When one of the subtopic cards is clicked, change page to the selected Topic and Subtopic 
  # to Explore Topics
  # Triggers when one of the links is clicked
  onclick("links", function(event){
    if(!is.null(input$links)){
      shinyjs::runjs("window.scrollTo(0, 0)")
      topic <- str_replace_all(input$links[1], "Maori", paste0("M", intToUtf8(0x0101L), "ori"))
      subtopic <- str_replace_all(input$links[2], "Maori", paste0("M", intToUtf8(0x0101L), "ori"))
      print(paste(convert2Query(c(topic, subtopic)), collapse = "/"))
      change_page(paste(convert2Query(c(topic, subtopic)), collapse = "/"))
    }
  })
  
  
  
  # Creates a single card for a given subtopic of a topic
  #
  # @param title - the title of the card, i.e. the subtopic name.
  # @param content - the description of the subtopic
  
  createSubtopicCard <- function(title, content, heading){
    
    # === Old way of computing filename ===
    # raw <- (paste(topic(), title) %>% tolower()) %>% gsub(pattern = " ", replacement = "-")
    # filename <- paste0(str_replace_all(raw, pattern = "\u0101", "a"), ".jpg")
    # filename <- filename %>% str_remove_all("/")
    
    # Get filename from subtopicDescription file - not used for now
    # filename <- (G_subtopicDescriptions %>% filter(subtopic == title))$app.img.filename

    # Create a clickable card. The attributes "topic" and "subtopic" get captured by JS and passed back to moduleControl
    div(id="onclick-card", `data-toggle`="tab", class="new-card", style="background: #fff", topic=topic(), subtopic=title,
           
           div(class="new-card-content",
               div(class="subtopic-card-header",
                 div(class="new-card-topic row",
                     div(class="col-md-12",
                         tags$h4(style="padding-left: 0", title)
                     )
                 ),
                 div(class="row",
                     div(class="subtitle col-md-12",
                         p(content)
                     )
                 )
               )
               # ==== Code for Subtopic Image, uncomment if needed for later release ===
               # div(class="row"
               #   div(class="graphic"
               #     div(class="graphic-heading",
               #         paste0(heading)
               #     ),
               #     div(class="graphic-image",
               #         tags$img(src=paste0("img/subtopic-images/", filename))
               #     )
               # )
               # )
           ),
           div(class="new-link-overlay custom-link", topic=topic(), subtopic=title, 
               p(id="new-topic-link", topic=topic(), subtopic=title, 
                 HTML(
                   paste0(
                     "Explore more"
                     )
                 )
               )
           )
    )
  }
}