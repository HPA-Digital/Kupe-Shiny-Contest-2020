library(shiny)
library(readxl)
library(RCurl)
library(shiny.router)

# Wrapper function to handle passing in page specific variables such as topic, subtopic, and indicator 
pageWrapper <- function(page, ...){
  function(input, output, session){
    do.call(callModule, list(module=page, ...))
  }
}



# generateRoutes is called at the start of launching the server 
# pre-generates each page before launching due to restirctions with passing in elements into shiny.router
generateRoutes <- function(){
  
  # Pages that aren't dynamic
  pages <- list(
    default=route("/", default_pages[["splash"]], callModule(pageSplash, "splash")), # Home page
    route("home", default_pages[["splash"]], callModule(pageSplash, "splash")), # Home page
    route("methodology", default_pages[["methodology"]],  callModule(pageMethodology, "methodology"))
  )
  
  # Topic pages - loops through all topics in G_topicDescriptions, generating each route
  topic_paths <- map(G_topicDescriptions$topic, 
                     ~ {
                       route(
                         convert2Query(.x), 
                         default_pages[["subtopicsMenu"]], 
                         pageWrapper(pageSubtopicsMenu, id="subtopicsMenu", name="Subtopic Menu",  topic_page=.x)
                       )})
  
  # Subtopic pages - loops through all topics in G_subtopicDescriptions, generating each route
  subtopics_paths <- map2(G_subtopicDescriptions$topic, G_subtopicDescriptions$subtopic, 
                          ~ {
                            ns <- convert2Query(.y)
                            route(
                              paste(convert2Query(c(.x, .y)), collapse = "/"), 
                              default_pages[[ns]], 
                              pageWrapper(pageExploreTopics, id=ns, name="Explore topics",  topic_page=.x, subtopic_page=.y)
                            )})
  
  # Indicator pages - loops through all topics in G_subtopicDescriptions, generating each route
  indicator_paths <- map(G_indicatorList$indicator,
                         ~{
                           r <- G_indicatorList %>% filter(indicator == .x)
                           ns <- convert2Query(r$indicator)
                           route(
                             paste(convert2Query(c(r$topic, r$subtopic, r$short.description)), collapse = "/"),
                             default_pages[[ns]],
                             pageWrapper(pageExploreIndicators, id=ns,name="Explore indicators",  topic_page=r$topic, subtopic_page=r$subtopic, indicator_page=r$indicator)
                           )
                         })
  
  # Merge all pages into one big list and return 
  pages <- c(pages, topic_paths, subtopics_paths, indicator_paths)
  pages
}


# Funciton to generate routes and return shiny.router object
getRouter <- function(){
  do.call(make_router, generateRoutes())
}


shinyServer(function(input, output, session) {

  # Generate routes and pages
  getRouter()(input, output, session)
  
  
  # JS to change page using shiny.router rather than using external JS
  onclick("header_img", {
    change_page("/")
  })
  
  onclick("method_btn", {
    change_page("methodology")
  })
  
  onclick("home_bread", {
    change_page("/")
  })

  
})




