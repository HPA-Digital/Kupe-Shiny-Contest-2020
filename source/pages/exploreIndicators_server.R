
# Imports
library(stringr)
library(readxl)
library(rapport)
library(purrr)
# Server function for pageExploreTopics provides the backend for the Explore Topics UI page.

pageExploreIndicators <- function(input, output, session, name, topic_page, subtopic_page, indicator_page) {
  ns <- session$ns
  
  localMsg <- reactiveValues(
    topic = topic_page,
    subtopic = subtopic_page,
    ind = indicator_page
  )
  
  
  
  # ====================================== OBSERVE EVENTS ===========================================
  
  # Behaviour for a change in the currentPage. This is normally when the page changes and the data needs to be initialised
  # observeEvent({localMsg$topic; localMsg$subtopic; localMsg$ind}, {
    # Filter G_indicatorList by the incoming data if it is actually "fresh" data, that is, data from the previous page 
    # and not just from a change observed on this page
    ret <- G_indicatorList %>%
      filter(tolower(topic)  %in% tolower(str_replace_all(localMsg$topic, "Maori", paste0("M", intToUtf8(0x0101L), "ori"))),
             tolower(subtopic) %in% tolower(localMsg$subtopic),
             tolower(indicator) %in% tolower(localMsg$ind)
      ) %>% select(topic, subtopic, indicator, short.description) %>% as.list()
    
    
    # Set the options and selected item in the Topic dropdown
    topChoices <- (unique(G_indicatorList$topic) %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")))  %>% sort()
    topSelected <- ret$topic
  
    # Set the options and selected item Subtopic dropdown
    tempData <- G_indicatorList %>% filter(tolower(topic) %in% tolower(topSelected  %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")))) 
    subtopicChoices <- unique(tempData$subtopic) 
    subtopicSelected <- ret$subtopic
    
    # Get a list of indicators for the current subtopic/topic pair
    indicators <- G_indicatorList %>%
      filter(
        tolower(subtopic) == tolower(subtopicSelected),
        tolower(topic) == tolower(topSelected)
      ) %>% select("indicator",
             "short.description")
    
    # Set the options for the Indicator dropdown to this list
    indChoices <- indicators$indicator
    names(indChoices) <- indicators$short.description
    indSelected <- ret$indicator
    
    # Update the dopdown list options and selected items 
    updateSelectInput(session, "topic", choices=topChoices %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")),
                      selected = topSelected %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")))
    
    updateSelectInput(session, "subtopic", choices=subtopicChoices %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")),
                      selected = subtopicSelected %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")))
    
    updateSelectInput(session, "indicator", choices=indChoices,
                      selected = indSelected %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")))

  
  # Find subtopic and indicator after change in topic drop down then change page
  onclick("topic", function(event){
    if(input$topic != topic_page){
      # Get a list of Subtopics for this Topic, set the selected one to the first Subtopic in the dropdown
      tempData <- G_indicatorList %>% filter(tolower(topic) %in% tolower(str_replace_all(input$topic, "Maori", paste0("M", intToUtf8(0x0101L), "ori")))) 
      subtopicChoices <- unique(tempData$subtopic)
      subtopicSelected <- subtopicChoices[1]
      
      # Get a list of Inidicators for the Subtopic, set the selected one to the first Indicator in the dropdown
      indicators <- G_indicatorList %>%
        filter(
          tolower(subtopic) == tolower(subtopicSelected),
          tolower(topic) == tolower(tolower(str_replace_all(input$topic, "Maori", paste0("M", intToUtf8(0x0101L), "ori"))))
        ) %>% select("indicator",
                     "short.description")
  
      indSelected <- indicators$short.description[1]
      change_page(paste(convert2Query(c(input$topic, subtopicSelected, indSelected)), collapse = "/"))
      localMsg$topic <- input$topic
      localMsg$subtopic <- subtopicSelected
      localMsg$ind <- indicators$indicator[1]
    }
  })
  
  
  
  # Behaviour for when the selected Subtopic in the Subtopic dropdown is changed
  onclick("subtopic", function(event){
    if(input$subtopic != subtopic_page){
      # Get a list of Inidicators for the Subtopic, set the selected one to the first Indicator in the dropdown
      
      indicators <- G_indicatorList %>%
        filter(
          tolower(subtopic) == tolower(input$subtopic),
          tolower(topic) == tolower(str_replace_all(input$topic, "Maori", paste0("M", intToUtf8(0x0101L), "ori")))
        ) %>% select("indicator",
                     "short.description")
      
      indSelected <- indicators$short.description[1]
      
      
      change_page(paste(convert2Query(c(input$topic, input$subtopic, indSelected)), collapse = "/"))
      localMsg$subtopic <- input$subtopic
      localMsg$ind <- indicators$indicator[1]
    }
  })
  
  


  
  # Behaviour for when the selected Indicator in the Indicator dropdown is changed
  onclick("indicator", function(event){
    if(input$indicator != indicator_page){
      print(indicator_page)
      print(paste(convert2Query(c(input$topic, input$subtopic, input$indicator)), collapse = "/"))
      indSelected <- G_indicatorList %>% filter(indicator == input$indicator) %>% pull(short.description)
      
      change_page(paste(convert2Query(c(input$topic, input$subtopic, indSelected)), collapse = "/"))
      localMsg$ind <- input$indicator
    }
  })
 
  # Change page when a breadcrumb link is pressed
  onclick("breadLinks", {
    print(input$breadLinks)
    # First scroll to the top of the page for when the new page is rendered
    shinyjs::runjs("window.scrollTo(0, 0)")
    if(!is.null(input$breadLinks)){
      # Two elements in input$breadLinks means Subtopic was clicked - set the moduleMsg to these two elements
      if(length(input$breadLinks) == 2){
        # moduleControl$moduleMsg <- list("topic"=input$breadLinks[1], "subtopic"=input$breadLinks[2], "fresh"=T)
        change_page(paste(convert2Query(c(input$breadLinks[1], input$breadLinks[2])), collapse = "/"))
      }else{
        # Otherwise it will just have 1 element - the Topic. 
        # moduleControl$moduleMsg <- list("topic"=input$breadLinks[1] %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")),  "fresh"=T)
        change_page(paste(convert2Query(c(input$breadLinks[1])), collapse = "/"))
      }
    }
  })
  
  
  # ====================================== GENERAL UTILITY DATA ===========================================
  
  # List of all the datapoint years
  allyears <- reactive({
    dat <- mainData() %>%
      arrange(desc(year)) %>%
      mutate(year=as.numeric(year),
             name=paste0(year))
    years <- dat$year
    years <- unique(years)
  })
  
  yearsToOmit <- reactive({
    allTopicInds <- G_indicatorList[G_indicatorList$topic == topic_page, ]
    allTopicIndData <- allTopicInds %>% 
      left_join(G_timeseries, by="indicator")
    # Get the names of the time trends columns first, 
    # then by checking each of the time trend columns for the data,
    # we can find the columns that are all empty - the ones we need to leave out
    percentColumns <- colnames(allTopicIndData)[grepl("percent", colnames(allTopicIndData))]
    pvalueColumns <- colnames(allTopicIndData)[grepl("p.value", colnames(allTopicIndData))]
    
    yearColsToOmit <- lapply(1:ncol(allTopicIndData), function(i){
      if(colnames(allTopicIndData)[i] %in% percentColumns) {
        if(all(is.na(allTopicIndData[, i]))) {
          return(colnames(allTopicIndData)[i])
        }
      }
    }) %>% unlist()
    
    if(topic_page == "M\u0101ori cultural identity") {
      if(any(grepl("percent.12", yearColsToOmit)) & any(grepl("percent.14", yearColsToOmit))) {
        yearColsToOmit <- yearColsToOmit[-grep("percent.12", yearColsToOmit)]
        yearColsToOmit <- yearColsToOmit[-grep("percent.14", yearColsToOmit)]
      }
    }
    
    shortPyears <-  yearColsToOmit %>% str_remove("percent.")
    
    pColsToOmit <- lapply(1:length(pvalueColumns), function(index){
      currentP <- pvalueColumns[index]
      lapply(shortPyears, function(year) {
        if(grepl(year, currentP)){
          return(currentP)
        }
      })
      
    }) %>% unlist()
    
    
    skipYears <- G_yearsList[G_yearsList$short.year %in% shortPyears, ]$full.year
    
    totalColsToOmit <- c(yearColsToOmit, pColsToOmit)
    list(tsCols=totalColsToOmit, years=skipYears)
  })
  
  # Determines if indicator is not a Mean - returns TRUE is value is Prevalence
  isNotMeanInd <- reactive({
    req(indicator_page)
    tempData <- G_indicatorList %>% filter(tolower(indicator) %in% tolower(indicator_page)) 
    return (ifelse(tempData$estimate.type == "%", TRUE, FALSE))
  })
  
  # Gets the highest value for the whole Indicator dataset
  globalMaximumValue <- reactive({
    req(timeseriesData(), distData(), ARRData())
    vmax <- max(max(timeseriesData()$total, na.rm = T), 
                max(distData()$total, na.rm = T), 
                max(distData()$male, na.rm = T), 
                max(distData()$female, na.rm = T), 
                max(ARRData()$total, na.rm = T), na.rm = T)
    vmax
  })
  
  # Dataframe of all the relevant prevalence/mean data for all the selected indicator
  mainData <- reactive( {
    req(indicator_page)
    main <- G_indicatorList %>%
      filter(indicator==indicator_page) %>%
      merge(G_prevalences, by="indicator")  %>%
      select(description,
             total,
             total.low.CI,
             total.high.CI,
             estimated.number,
             group,
             year,
             male,
             male.low.CI,
             male.high.CI,
             female,
             female.low.CI,
             female.high.CI,
             estimate.type,
             estimate.unit,
             dim_var
             
      )
    main    
  })
  
  # Determine the latest year of available data for the selected indicator
  latestYear <- reactive({
    req(mainData(), nrow(mainData())>0)
    res <- mainData() %>%
      filter(year==max(year))
    res[1,]$year
  })
  
  # Capitalise the first letter in a String
  capFirst <- function(string){
    paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
  }
  
  # Control values on whether to show the Download and Notes sections. If no data is available, these values will be False
  notesAndDLControl <- reactiveValues(
    showComp = T,
    showTS = T
  )
  
  # Get the descriptive text for the Indicator
  indicator_desc <- reactive({
    dat <- G_indicatorDescriptions %>% filter(paste0("ind_", indicator) == indicator_page)
    dat$indicator <- paste0("ind_", dat$indicator)
    ind <- G_indicatorList %>% mutate(fullpopname = capFirst(population)) %>% select("indicator", "short.description", "fullpopname")
    
    dat <- dat %>% left_join(ind, by = c("indicator"))
    
    dat
  })
  
  # Creates a standard mean title from a percent one
  toMeanTitle <- function(title){
    return(sub("percent.", "mean.", title))
  }
  
  # Determine which years are time trend years
  time.trend.years <- G_yearsList %>%
    filter(show.in.time.trends == 1)
  
  # Determine which columns are time trend columns
  which.time.trend.columns <-
    paste0("percent.", time.trend.years$short.year)
  
  # Determine which years are p-value years
  p.value.years <- G_yearsList %>%
    filter(show.p.value == 1)
  
  # Determine which columns are p-value columns
  which.p.value.columns <- paste0("p.value.",
                                  G_current.year$short.year,
                                  ".",
                                  p.value.years$short.year)
  
  # Get estimate type for selected indicator
  est.type <- reactive({
    (G_indicatorList %>% filter(indicator == indicator_page))$estimate.type
  })
  
  # =============================================== TAB: OVERVIEW =========================================== 
  # this is the indicator overview page
  
  output$overviewTitle <- renderUI({
    tags$h3(paste0(latestYear(), " Health and Lifestyles Survey"))
  })
  
  # Condition for the Overview to be displayed. If any of the data for the Indicator exists - show the overview
  output$showOverview <- reactive({
    req(ARRData(),  overviewData(), distData(), mainData())
    print(mainData()[1,]$estimate.type)
    return((nrow(ARRData()) > 0 
            && mainData()[1,]$estimate.type=="%")
           || nrow(timeseriesData()) > 0
           || nrow(overviewData()) > 0 || nrow(distData()) > 0)
  })
  outputOptions(output, "showOverview", suspendWhenHidden = FALSE)
  
  # --------------------------------------------------- CARD: OVERVIEW ------------------------------------------------  
  
  # Data for the Overview card (top left)
  overviewData <- reactive({
    mainData() %>%
      filter(
        group == "Total",
        year==latestYear())
  })
  
  # Generate the Overview chart for PREVALENCE values
  overviewChartHTML <- reactive({
    # Ensure data is not mean
    req(overviewData())
    validate(need(nrow(overviewData()) > 0, "No overview available."))
    req(overviewData()$estimate.type == "%")
    
    # The only real data the plot needs is the general Total
    dat <- overviewData()
    dat$total <- 100 - dat$total
    
    # Set plot margins
    m = list(
      l = 40,
      r = 40,
      b = 50,
      t = 10,
      pad = 0
    )
    
    # Generate the Plot base
    pl <- plot_ly(overviewData(), x=~description, y=~total, type = 'bar', orientation = 'v',
                  marker=list(color="#f79520"), hoverinfo="none", height="515") %>%
      # Annotations:
      # Add "The prevalence was"
      add_annotations(x = 0.65, y = 100,
                      xref = "x", yref = "y", xanchor="left", yanchor="top",
                      showarrow = F, xshift= 0,
                      text = ~paste0("The prevalence was"),
                      font=list(size=14, family="ars-maquette-web", color="#494949")
      ) %>%
      # Add the prevalence value as a percentage
      add_annotations(x = 0.65, y = 95,
                      xref = "x", yref = "y", xanchor="left", yanchor="top",
                      showarrow = F, xshift= 0,
                      text = ~paste0("<b>", format(round(total, 1), nsmall = 1, scientific = F), "%</b>"),
                      font=list(size=36, family="ars-maquette-web", color="#494949")
      ) %>%
      # Add "which is an estimated"
      add_annotations(x = 0.65, y = 83,
                      xref = "x", yref = "y", xanchor="left", yanchor="top",
                      showarrow = F, xshift= 0,
                      text = "which is an estimated",
                      font=list(size=14, family="ars-maquette-web", color="#494949")
      ) %>%
      # Add the estimated number 
      add_annotations(x = 0.65, y = 78,
                      xref = "x", yref = "y", xanchor="left", yanchor="top",
                      showarrow = F, xshift= 0,
                      text = ~paste0("<b>", format(overviewData()$estimated.number, big.mark=",", trim=TRUE, scientific = F), "<br>",
                                     "</b>"),
                      align="left",
                      font=list(size=36, family="ars-maquette-web", color="#494949")
      ) %>%
      # Add "adults"
      add_annotations(x = 0.65, y = 68,
                      xref = "x", yref = "y", xanchor="left", yanchor="top",
                      showarrow = F, xshift= 0,
                      text = "adults",
                      font=list(size=14, family="ars-maquette-web", color="#494949")
      ) %>%
      # Add the bar
      add_trace(data = dat, x=~description, y=~total, marker=list(color="#a6a6a6")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        margin = m,
        showlegend = F,
        xaxis= list(visible=F, showgrid=F, showticklabels=F, title="", fixedrange=T),
        yaxis = list(range = c(0,100), dtick = 20, ticksuffix= "%", showticklabels=T, showgrid=F, gridcolor="#a6a6a6", title="", showline=F, zeroline=F, fixedrange=T),
        barmode = 'stack',
        plot_bgcolor='transparent',
        paper_bgcolor='transparent'
      ) %>%
      hide_colorbar()
    pl$elementId <- NULL
    pl
    
  })
  
  # Render the generated Plot
  output$overviewPlot <- renderPlotly({overviewChartHTML()})
  
  # Render the Markup for indicator Overview
  output$overviewChartRender <- renderUI({
    req(overviewData(), topic_page)
    validate(need(nrow(overviewData()) > 0, "No overview available."))
    
    # Ensure data is Mean
    usePercent <- overviewData()$estimate.type == "%"
    
    # Set prefix text
    typeTextPrefix <- "The average (mean) "
    typeText <- if(usePercent){"The prevalence was"} 
    
    # These are the Mean indicators. The "type text" is hard coded in to handle these few indicators
    if(indicator_page == "ind_12"){
      typeText <- paste0(typeTextPrefix, "number of gambling activities participated in the last 12 months was ")
    }
    if(indicator_page == "ind_37"){
      typeText <- paste0(typeTextPrefix, "out of 100 adults in NZ perceived to smoke was ")
    }
    if(indicator_page == "ind_45"){
      typeText <- paste0(typeTextPrefix, "age in years when people first tried smoking was ")
    }
    if(indicator_page == "ind_46"){
      typeText <- paste0(typeTextPrefix, "age in years when people first started smoking daily was ")
    }
    if(indicator_page == "ind_51"){
      typeText <- paste0(typeTextPrefix, "number of cigarettes smoked per day was ")
    }
  
    meanUnit <- tolower(overviewData()$estimate.unit) 
    
    # Initialise Lightbox text values
    topTag <- NULL
    bottomTag <- NULL
    appendixtag <- NULL
    
    # Use the appropriate descriptive text for Mean and Prevalence indicators
    if(usePercent){
      topTag <- p("This is the percentage of the population affected (that is, the unadjusted prevalence in the specified population).")
      if(isNotMeanInd())
      {
        appendixtag <- p("Estimated numbers are rounded to the nearest 1,000 people.")
      }
      
    }else{
      topTag <- p("This is the unadjusted average (mean) in the specified population.")
    }
    
    # Return the markup
    # First the lightbox...
    tagList(
      lightboxBody(ns("lb-overviewChart"),
                   tagList(
                     topTag,
                     bottomTag,
                     appendixtag
                   )
      ),
      # Then the overview plot. If the indicator is prevalence, render the Plot
      if(usePercent){
        column(9,
               plotlyOutput(ns("overviewPlot"), height="auto")
        )
      }
      # Else render some HTML and an Icon to show the mean value
      else{
        fluidRow(class="mean-overview",
                 column(6, style="height: 100%",
                        img(src=getTopicIcon(topic_page), class="img-responsive ind-image")
                 ),
                 column(6, class="mean-overview-content",
                        div(style="word-wrap: break-word; padding-right: 10px;",
                            div(typeText, class="indicator-overview-p"),
                            div(paste0(round(overviewData()$total, digits = 1), paste0(" ")), class="indicator-overview-large")
                        )
                 )
        )
      }
    )
  })
  
  # Unit for the Indicator to show in the Age and Sex overview card (e.g: "Number of x")
  output$AgeDistMeanUnit <- renderUI({
    req(nrow(distData()) > 1)
    if(!isNotMeanInd()) {
      overviewData()[1,]$estimate.unit
    }
  })
  
# ----------------------------------------------------- CARD: AGE AND SEX ------------------------------------------------ 
  
  # Get the data for the chart
  distData <- eventReactive(indicator_page, {
    
    ret <- mainData() %>%
      filter(grepl("[1-9][^a-z]", group),#dirty way to check if this is a range
             year==latestYear()) 
    
    extra <- G_indicatorList %>% 
      filter(
        tolower(indicator) == tolower(indicator_page),
        tolower(subtopic) == tolower(subtopic_page))
    extra <- extra$extra.age.groups
    if(is.na(extra)){extra <-0}
    
    
    # @epi-interactive - addressing #1
    # ------------------------------------
    # Age/Sex data filtered correctly but not explicitly sorted
    ret <- setdiff(ret, ret %>% filter(grepl("[ \\( \\)]", group)))
    # The sort.order we want to use. This is the group column of G_prevalenceSubgroupList with as well as the sort.order column
    sort.order <- G_prevalenceSubgroupList[G_prevalenceSubgroupList$group %in% ret$group, ] %>% select(sort.order, group)
    # Join the existing data (ret) with the sort.order we want to use
    # Then arrange (sort) by the sort order and then remove the sort.order since we don't want to show it in the webtable
    ret <- ret %>% join(sort.order) %>% arrange(sort.order) %>% select(-sort.order)
    
    ret 
  })
  # output is only the age groups
  

  
  # Generate the Age Distribution Plot
  distPlotHTML <- function(){
    req(distData())
    # Show an error box if no Age/Sex data is available
    validate(need(nrow(distData()) > 1, "No age and sex available."))
    validate(need(nrow(distData() %>% filter(!is.na(male), !is.na(female))) > 1, "No age and sex available."))
    
    # Set the chart width
    w <- session$clientData[[paste0("output_", ns("rightWidth"), "_width")]]
    if(is.null(w)){
      w <- "100%"
    }
    
    # Get the estimate unit and set margins for the plot
    isMean <- distData()[1,]$estimate.type!="%"
    unit <- distData()[min(which(!is.na(distData()$total))),]$estimate.unit
    m = list(
      l = 50,
      r = 3,
      b = 50,
      t = 10,
      pad = 15
    )
    
    # Determine x-axis categories for the plot (sorting removed here as its already done in the data)
    categories <- distData()$group
    # Set axis configuration. a = x-axis, b =  y-axis
    a <- list(visible=T, showgrid=F, title="", mirror=F,
              categoryorder = "array",categoryarray=categories, tickFont = "ars-maquette-web", fixedrange=T)
    b <- list(ticksuffix= if(!isMean){"%"}, showticklabels=T, gridwidth=2 , gridcolor="#ddd", title="", title="", zeroline=T, 
              zerolinewidth=3, zerolinecolor="#bbb", showLine=F, mirror=TRUE, tickFont = "ars-maquette-web, Arial", hoverformat="0.1f", fixedrange=T)
    
    # Get the max value for the plot
    r <- chartRange(globalMaximumValue())
    tick0 <- 0
    dtick <- r$tick
    top <- r$range[2]
    b <- c(b, list(range = c(0,top), dtick = dtick))
    
    # Replace NA with 0 so the groups are not ommitted from the plot
    dat <- distData()
    dat[is.na(dat)] <- 0
    
    
    # Generate the plot
    pl <- plot_ly(dat,  type = 'bar', height=430, orientation = 'v',  hoverlabel=~list(bgcolor="#ffffff", font=list(family="ars-maquette-web")),
                  width=w) %>%
      add_trace(x=~group,y= ~male, name = 'Male', marker = list(color = "#4babc5"),
                text= if(isMean) {
                  ~paste0("Mean: ", format(male, digits = 1, nsmall = 1), " (CI ", format(male.low.CI, digits = 1, nsmall = 1), " - ", format(male.high.CI, digits = 1, nsmall = 1), ")")
                } else  {
                  ~paste0("Prevalence: ", format(male, digits = 1, nsmall = 1), "% (CI ", format(male.low.CI, digits = 1, nsmall = 1), " - ", format(male.high.CI, digits = 1, nsmall = 1), ")")
                },
                hoverinfo="text") %>%
      
      add_trace(x=~group, y= ~female, name = 'Female', marker = list(color = "#0080a4"),
                text= if(isMean) {
                  ~paste0("Mean: ", format(female, digits = 1, nsmall = 1), "(CI ", format(female.low.CI, digits = 1, nsmall = 1), " - ", format(female.high.CI, digits = 1, nsmall = 1), ")")
                } else  {
                  ~paste0("Prevalence: ", format(female, digits = 1, nsmall = 1), "% (CI ", format(female.low.CI, digits = 1, nsmall = 1), " - ", format(female.high.CI, digits = 1, nsmall = 1), ")")
                },
                hoverinfo="text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        margin = m,
        showlegend = T,
        xaxis= a,
        yaxis = b,
        font=list(family="ars-maquette-web"),
        legend = list(x = 100, y = 0.9),
        barmode = 'group',
        plot_bgcolor='transparent',
        paper_bgcolor='transparent'
      ) %>%
      hide_colorbar()
    pl$elementId <- NULL
    pl
  }
  
  # Render the Age Distribution chart
  output$ageDist <- renderPlotly({
    distPlotHTML()
  })
  
  
# ------------------------------------------------------ CARD: ETHNICITY ------------------------------------------------  
  
  
  # Unit for the Indicator to show in the Ethnicity overview card (e.g: "Number of x")
  output$EthDistMeanUnit <- renderUI({
    req(nrow(ARRData()) > 1)
    if(!isNotMeanInd()){
      overviewData()[1,]$estimate.unit
    }
  })
  
  # Generate the Ethnicity Plot (bottom right)
  ARRHTML <- function(){
    req(ARRData())
    # Show an error box if there's no Ethnicity data available
    validate(need(nrow(ARRData()) > 1, "No ethnicity available."))
    
    # Store data locally and determine indicator type and unit
    dat <- ARRData()
    isMean <- dat[1,]$estimate.type!="%"
    unit <- distData()[min(which(!is.na(distData()$total))),]$estimate.unit
    
    # Margins for the plot (left, right, bottom, top, and padding)
    m = list(
      l = 50,
      r = 3,
      b = 50,
      t = 10,
      pad = 15
    )
    
    # Axis options for the plot - a = x-axis, b = y-axis
    a <- list(visible=T, linecolor="#bbb", linewidth=3, mirror=F, title="", 
              categoryorder = "array", showgrid=F, gridwidth=2, gridcolor="ddd" ,showline=F, fixedrange=T)
    
    b <- list(ticksuffix= if(!isMean){"%"}, showticklabels=T,
              showgrid=T, gridcolor="#ddd", gridwidth=2, title="", zeroline=T, zerolinewidth=3, zerolinecolor="#bbb", mirror=F, showline=F, fixedrange=T)
    
    # Determine the range for the plot based on the highest value available, add this value to the y-axis options
    r <- chartRange(globalMaximumValue())
    dtick <- r$tick
    top <- r$range[2]
    b <- c(b, list(range = c(0,top), dtick = dtick))
    
    # Determine the gap between bars based on how many ethnicities are available to be shown
    dat <- dat %>% droplevels()
    datalen <- length(dat$total[!is.na(dat$total)])
    gap <- 0.2
    if(datalen == 3)
    {
      gap <- 0.3
    }
    else if(datalen == 2)
    {
      gap <- 0.65
    }
    
    # Generate the actual plot
    pl <- plot_ly(dat, x=~dat$group, y=~dat$total, marker = list(color = c('rgb(0, 147, 178)')),  type = 'bar', height=430, orientation = 'v', marker=list(color="#4babc5"), 
                  hoverinfo="text", hoverlabel=~list(bgcolor="#ffffff", font=list(family="ars-maquette-web")),
                  text=if(isMean) {
                    ~paste0("Mean: ", format(total, digits = 1, nsmall = 1), " (CI ", format(total.low.CI, digits = 1, nsmall = 1), " - ", format(total.high.CI, digits = 1, nsmall = 1), ")")
                  } else  {
                    ~paste0("Prevalence: ", format(total, digits = 1, nsmall = 1), "% (CI ", format(total.low.CI, digits = 1, nsmall = 1), " - ", format(total.high.CI, digits = 1, nsmall = 1), ")")
                  }) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        margin = m,
        showlegend = F,
        xaxis= a,
        yaxis = b,
        font=list(family="ars-maquette-web"),
        plot_bgcolor='transparent',
        paper_bgcolor='transparent',
        barmode = 'group', bargap = gap
      ) %>%
      hide_colorbar()
    
    pl$elementId <- NULL
    pl
  }
  
  # Render the Ethnicity plot (or error if theres no Plot to display)
  output$ARRPlot <- renderPlotly({
    ARRHTML()
  })
  
# --------------------------------------------------- CARD: TIME TRENDS ------------------------------------------------  
  
  # Unit for the Indicator to show in the Time Trends overview card (e.g: "Number of x")
  output$TimeSeriesMeanUnit <- renderUI({
    req(nrow(timeseriesData()) > 1)
    req(indicator_page != "ind_12")  # Surpressing overviewTS from showing for indicator: "Average number of activities" as per HPA
    if(!isNotMeanInd()) {
      overviewData()[1,]$estimate.unit
    }
  })
  
  # Generate the Time Trends plot
  overviewTSHTML <- function(){
    req(timeseriesData())
    # Show an error box if there isn't any timeseries data available
    validate(need(indicator_page != "ind_12", "No time trends available."))  # Surpressing overviewTS from showing for indicator: "Average number of activities" as per HPA
    validate(need(nrow(timeseriesData()) > 1, "No time trends available."))
    
    # Set the width for the chart
    w <- session$clientData[[paste0("output_", ns("rightWidth"), "_width")]]
    if(is.null(w)){
      w <- "80%"
    }
    
    # Determine the unit for the indicator
    isMean <- timeseriesData()[min(which(!is.na(timeseriesData()$total))),]$estimate.type=="Mean"
    unit <- timeseriesData()[min(which(!is.na(timeseriesData()$total))),]$estimate.unit
    
    # Determine the range for the y-axis based on the highest data point for the indicator
    r <- chartRange(globalMaximumValue())
    tick0 <- 0
    dtick <- r$tick
    top <- r$range[2]
    
    # x-axis options for the Plot
    a <- list(
      title = "",
      type = "category",
      categoryorder="array",
      categoryarray=G_yearsList$year[!G_yearsList$year %in% yearsToOmit()$years],
      range=c(-1, length(G_yearsList$year[!G_yearsList$year %in% yearsToOmit()$years])),
      autorange=F,
      showgrid=F,
      gridwidth=2,
      gridcolor="ddd",
      mirror=F,
      showline=F, fixedrange=T
    )
    
    # y-axis options for the Plot
    b <- list(
      hoverformat = "0.1f",
      linecolor="#bbb",
      linewidth=3,
      title="",
      mirror=TRUE,
      showgrid=T,
      gridwidth=2,
      gridcolor="ddd",
      showline=F,
      zeroline=T,
      zerolinewidth=3,
      zerolinecolor="#bbb", fixedrange=T
    )
    
    # If the indicator is a Prevalence one, add a "%" suffix to the tick labels on the y-axis
    if(!isMean){
      b <- c(b, list(range = c(0,top), dtick = dtick, ticksuffix= "%"))
    }
    else
    {
      b <- c(b, list(range = c(0,top), dtick = dtick))
    }
    
    # Position the legend
    lgd <- list(
      xanchor="right",
      yanchor="top",
      x=1,
      y=-0.3
    )
    
    
    
    
    allYears <- timeseriesData() %>% join(G_yearsList, type = "full") %>% 
      select(colnames(timeseriesData()))
    allYears <- allYears %>% arrange(year)
     
    # Generate the actual chart
    plot <- plot_ly(timeseriesData(), type = 'scatter', mode = 'lines+markers',
                    width=w, hoverlabel=~list(bgcolor="#ffffff", font=list(family="ars-maquette-web")),
                    line=list(color="#f79520"),
                    connectgaps = FALSE,
                    height=400,
                    text= if(isMean) {
                      ~paste0("Mean: ", format(total, digits = 1, nsmall = 1), " (CI ", format(total.low.CI, digits = 1, nsmall = 1), " - ", format(total.high.CI, digits = 1, nsmall = 1), ")")
                    } else  {
                      ~paste0("Prevalence: ", format(total, digits = 1, nsmall = 1), "% (CI ", format(total.low.CI, digits = 1, nsmall = 1), " - ", format(total.high.CI, digits = 1, nsmall = 1), ")")
                    },
                    hoverinfo="text",
                    x=~year, y=~total, marker=list(size=15, color="#f79520"))%>%
      layout(xaxis=a, yaxis=b, margin=list(t=10,l=50,r=3,b=30, pad=15), legend=lgd ,
             font=list(family="ars-maquette-web"),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>%
      config(displayModeBar = FALSE)
    
    plot$elementId <- NULL
    plot
  }
  
  # Render the Chart (or error) to this output...
  output$tsoutput <- renderPlotly({
    overviewTSHTML()
  })
  
  # ... Then render that output to the UI
  output$overviewTS <- renderUI({
    plotlyOutput(ns("tsoutput"))
  })
  
  
  
  
  # ========================================== TAB: PREVALENCE / MEAN =========================================== 
  
  # Year select for the Prevalence table. 
  yearPrevalenceHTML <- reactive({
    # If theres more than one available year, show a dropdown selector
    if(length(allyears()) != 1){
      ret <- div(selectInput(
        ns("prevalYear"), label = "Choose year", selected = allyears()[[1]],  choices = allyears(), selectize = F
      ))
    }else {
      # else, just render static text  
      ret <- div(tags$p(style="font-weight: bold; font-size:16px;",paste0(" Year displayed: ", allyears()[[1]])))
    }
    return (ret)
  })
  
  
  # Output either the dropdown or static text to the UI
  output$yearPrevalence <- renderUI({
    yearPrevalenceHTML()
  })
  
  # Display checkboxes to control whether to show Male/Female prevalences as well as the Total 
  output$prevColsRender <- renderUI({
    vals <- NULL
    vals <- list("Men"="male", "Women"="female")
    # Dont show "estimated number" if indicator is a Mean
    if(isNotMeanInd())
    {
      vals <- c(vals, "Estimated number")
    }
    
    # Render a checkbox group
    checkboxGroupInput(ns("prevCols"), "Show:", vals, vals)
  })
  
  # A template container for the prevalence data to be fed into and displayed to the UI.
  prevalence.table.container <- function(population, unit) {
    cspan <- 2
    
    showEst <- F
    
    # Dont show "estimated number" if indicator is a Mean
    if(isNotMeanInd())
    {
      if("Estimated number" %in% input$prevCols){
        showEst <- T
      }
    }
    
    # Determine whether to show Male and Female columns based on which values are checked
    showMale <- F
    if("male" %in% input$prevCols){
      showMale <- T
    }
    showFem <- F
    if("female" %in% input$prevCols){
      showFem <- T
    }
    
    # Set the Lightbox text
    isPrev <- mainData()[1,]$estimate.type=="%"
    lbprebottom <- NULL
    if(isPrev)
    {
      if(isNotMeanInd())
      {
        lbprebottom  <- p("Estimated numbers are rounded to the nearest 1,000 people.")
      }
      
    }
    
    # Generate the Ouput
    htmltools::withTags(
      tagList(
        # First, the lightbox
        lightboxBody(ns("lb-prev-estimate"),
                     div(
                       p(
                         "The estimated number of people is the value that is inferred for that population based on data collected from the survey sample. Estimated numbers are rounded to the nearest 1,000 people."
                       )
                     )),
        # Then the table 
        table(
          class = "display nowrap",
          thead(
            tr(
              th(style="text-align:left; padding-left: 10px", rowspan = 2, "Population group"),
              th(colspan = cspan, class="center", "Total"),
              if(showMale){th(colspan = cspan, class="center", "Men")},
              if(showFem){th(colspan = cspan, class="center","Women")},
              if(showEst){th(colspan = cspan, class="center", paste("Estimated number of adults"), infoLightboxBtn(ns("lb-prev-estimate")))}
            ),
            tr(
              lapply(rep(c(unit, if(cspan == 2){"(95% CI)"}), sum(c(showFem, showMale), na.rm=TRUE)+1), th),
              if(showEst){
                tagList(
                  th("Total"),
                  if(cspan == 2){th("(95% CI)")}
                )
              }
            )
          )
        )))
  }
  
  # Determine which years are available for Prevalence data for the indicator
  selPrevYear <- reactive({
    if(length(allyears()) != 1){
      if(is.null(input$prevalYear))
      {
        allyears()[[1]]
      }
      else
      {
        input$prevalYear
      }
    }else {
      allyears()[[1]]
    }
  })
  
  # All prevalences for the indicator in the selected year
  selected.prevalences <- reactive({
    G_prevalences %>%
      filter(
        indicator == indicator_page,
        year == selPrevYear())
    
  })
  
  # The full data to do into the Table, including subheadings
  prevalence.tableData <- reactive({
    req(selected.prevalences())
    
    # Do some formatting to text 
    prevalences.1 <- selected.prevalences() %>%
      mutate(
        # Add macron to Maori
        group = str_replace_all(group, "Maori", "\u0101ori"),
        
        # Clarify NDep qunitiles
        # TODO: is this correct? - we have only 3 groups
        group = ifelse(group == "Quintile 1",
                       "Quintile 1 (least deprived)",
                       ifelse(group == "Quintile 5",
                              "Quintile 5 (most deprived)",
                              group)),
        total = ifelse(!is.na(total), format(
          round(total, digits = 1)
          , nsmall = 1), total),
        male = ifelse(!is.na(male), format(
          round(male, digits = 1)
          , nsmall = 1), male),
        female = ifelse(!is.na(female), format(
          round(female, digits = 1)
          , nsmall = 1), female),
        
        total.CI  = format.CI(
          ifelse(!is.na(total.low.CI), round(total.low.CI, digits = 1), total.low.CI),
          ifelse(!is.na(total.high.CI), round(total.high.CI, digits = 1), total.high.CI),
          nsmall = 1),
        male.CI   = format.CI(
          ifelse(!is.na(male.low.CI), round(male.low.CI, digits = 1), male.low.CI),
          ifelse(!is.na(male.high.CI), round(male.high.CI, digits = 1), male.high.CI),  
          nsmall = 1),
        female.CI = format.CI(
          ifelse(!is.na(female.low.CI), round(female.low.CI, digits = 1), female.low.CI),
          ifelse(!is.na(female.high.CI), round(female.high.CI, digits = 1), female.high.CI),          
          nsmall = 1),
        estimated.number.CI = format.CI(estimated.number.low.CI,
                                        estimated.number.high.CI,
                                        big.mark = ","),
        estimated.number = ifelse(!is.na(estimated.number),  format(estimated.number, big.mark = ",", trim = T), estimated.number),
        heading=0
      ) %>%
      select(group,
             total,
             total.CI,
             male,
             male.CI,
             female,
             female.CI,
             estimated.number,
             estimated.number.CI,
             sort.order,
             heading)
    
    # Insert rows that will serve as sub-headings in the table
    subheadings <- G_prevalenceSubgroupList %>%
      filter(!is.na(subheading)) %>%
      mutate(group = subheading,
             heading = 1) %>%
      select(group, sort.order, heading)
    
    # Organise the prevalences used for csv download
    prevalences.csv <- prevalences.1 %>%
      bind_rows(subheadings) %>%
      arrange(sort.order) %>%
      filter(!(heading==1 & lead(heading)==1)) %>%
      select(-sort.order, -heading)
    
    # Don't show estimated number if the indicator is a Mean
    if(!"Estimated number" %in% input$prevCols | !isNotMeanInd()){
      prevalences.1 <- prevalences.1 %>%
        select(-contains("estimated.number"))
    }
    
    # Don't show Male/Female unless the checkbox for each one is checked
    if(!"male" %in% input$prevCols){
      prevalences.1 <- prevalences.1[,!grepl("(?<!fe)male", colnames(prevalences.1), perl = T)]
    }
    if(!"female" %in% input$prevCols){
      prevalences.1 <- prevalences.1 %>%
        select(-contains("female"))
    }
    
    # Final formatted and organised Prevalences used in the table
    prevalences.2 <- prevalences.1 %>%
      bind_rows(subheadings) %>%
      arrange(sort.order) %>%
      filter(!(heading==1 & lead(heading)==1)) %>%
      select(-sort.order, -heading)
    
    
    reactiveValues(prevalences.1=prevalences.1, prevalences.2=prevalences.2, prevalences.csv = prevalences.csv, subheadings=subheadings)
  })

  # Render the subtitle if the Indicator is a Prevalence one  
  output$prevTableSubtitle <- renderUI({
    req(mainData())
    req(nrow(mainData())>0)
    isPrev <- mainData()[1,]$estimate.type=="%"
    if(isPrev){
      p(class="chart-subtitle", "This table shows the percentage of the population affected.")
    }
  })
  
  # Render the correct title to say if the Table shows Mean or Prevalence numbers based on the indicator
  output$prevTableTitle <- renderUI({
    req(mainData())
    req(nrow(mainData())>0)
    isPrev <- mainData()[1,]$estimate.type=="%"
    if(isPrev){
      p(class="chart-title", "Prevalence for selected indicator")
    }else{
      p(class="chart-title", "Mean for selected indicator")
    }
    
  })
  
  # Generate the Prevalance table
  prevalence.tableHTML <- reactive({
    # Pre-conditions to make sure all the data is here
    req(prevalence.tableData()$prevalences.1)
    req(nrow(prevalence.tableData()$prevalences.1)>0)
    req(prevalence.tableData()$prevalences.2)
    req(nrow(prevalence.tableData()$prevalences.2)>0)
    req(selected.indicator()$estimate.type)
    req(selected.indicator()$estimate.unit)
    
    
    unit <- selected.indicator()$estimate.type
    if(unit=="Mean"){
      unit <- selected.indicator()$estimate.unit  
    }
    
    filter <- unlist(prevalence.tableData()$subheadings["group"], use.names = FALSE)
    # to avoid the row of group name being replaced by default content "-"
    dat <- prevalence.tableData()$prevalences.2
    dat[dat$group %in% filter, -which(names(dat)=="group")] <- " "
    rightEst <- list(
      className = 'dt-right', targets = which(colnames(prevalence.tableData()$prevalences.2) == "estimated.number") -1
    )
    
    dt <- datatable(dat,
                    # Use the column headings set up in  the appropriate container
                    container = prevalence.table.container(subtopic_page, unit),
                    rownames = FALSE,
                    options = list(paging = FALSE,
                                   bInfo = FALSE,
                                   searching = FALSE,
                                   scrollX = TRUE,
                                   ordering= F,
                                   columnDefs = list(
                                     list(
                                       targets = '_all',
                                       defaultContent = '-'
                                     ),
                                     rightEst
                                   )
                    ),
                    escape = F
    ) %>%
      # Make sub-headings bold
      formatStyle(columns = "group",
                  fontWeight = styleEqual(prevalence.tableData()$subheadings$group,
                                          rep("bold",
                                              times = length(prevalence.tableData()$subheadings$group))
                  )
      ) %>%
      formatStyle(1:ncol(prevalence.tableData()$prevalences.2), 1, target = "cell", "borderTop"=styleEqual(prevalence.tableData()$subheadings$group,
                                                                                                           rep("solid 1px black",
                                                                                                               times=length(prevalence.tableData()$subheadings$group))))
    
    if(("Estimated number" %in% input$prevCols) & isNotMeanInd())  {
      dt <- dt %>% 
        # Write population counts with comma separators for thousands/millions
        formatStyle(columns = grep("estimated.number", names(prevalence.tableData()$prevalences.1), fixed=T, value=T))
    }
    
    dt <- dt %>%
      formatStyle(columns = grep("CI", names(prevalence.tableData()$prevalences.1), fixed=T, value=T), color = "grey")
    
    dt %>%
      formatRound(columns = grep("(?<!fe)male\b(?!CI)|female\b(?!CI)|total\b(?!CI)", perl=T, names(prevalence.tableData()$prevalences.1), value=T), digits = 1)
  })
  
  
  
  # Render the datatable generated to the UI
  output$prevalence.table <- DT::renderDataTable({prevalence.tableHTML()})
  
  output$prevNotes <- renderUI({
    tagList(
      tags$p(style = "color: #9c9c9c; margin: 30px 0",
             "Source: Health and Lifestyles Survey"),
      tags$p("Notes:"),
      tags$ul(
        tags$li("This table presents unadjusted results; that is, the prevalence or mean estimates reflect the actual percentage or mean of the population affected in each time period."),
        tags$li("Total response measure of ethnicity was used. People who reported belonging to more than one ethnic group are counted once in each group they reported."),
        # As per request from HPA, Heavy drinking note added. Adjust text and ordering as needed
        if(subtopic_page == "Alcohol attitudes" | subtopic_page == "Alcohol cutting back") {
          tags$li("Heavy drinking is defined as 5 or more standard drinks for women and 6 or more standard drinkers for men. The definition of a standard drink can be found on ", 
            tags$a(href="http://www.alcohol.org.nz/help-advice/standard-drinks/whats-a-standard-drink", target="_blank",
                   "http://www.alcohol.org.nz/help-advice/standard-drinks/whats-a-standard-drink"
           )
          )
        },
        tags$li("Dashes indicate insufficient sample size.")
      )
    )
  })
  
  # ========================================== GENERAL OUTPUTS =========================================== 
  
  # Renders the breadcrumb at the top of the page
  output$breadcrumb <- renderUI({
    req(indicator_page)
    currentIndicator <- HTML("<span class='breadcrumb-item breadcrumb-focus'>", (G_indicatorList %>% filter(indicator == indicator_page))$short.description, "</span>")

    div(id=ns("breadLinks"), class="custom-link-binding",  
        # Breadcrumb Item: Home
        span(class="breadcrumb-item", 
             tags$a(href="", `data-toggle`="tab",
                    tags$i(id = "home_bread", class="fa fa-home")
             )
        ), ">",
        # Breadcrumb Item: Topic
        span(class="breadcrumb-item", 
             tags$a(href="", class="custom-link", topic= topic_page,  navto="subtopics-menu", `data-toggle`="tab",
                    paste0(substr(topic_page, 1, 1), substr(tolower(topic_page), 2, nchar(topic_page))) %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori"))
             )
        ), ">",
        # Breadcrumb Item: Subtopic
        span(class="breadcrumb-item", 
             tags$a(href="", class="custom-link", topic= topic_page, subtopic=subtopic_page, navto="explore-topics", `data-toggle`="tab",
                    subtopic_page
             )
        ), ">",
        # Breadcrumb Item: Indicator
        span(class="breadcrumb-item breadcrumb-focus", currentIndicator)
    )
  })
  
  # The Topic Heading that will appear under the breadcrumb alongside the Topic Icon
  output$topicHeading <- renderUI({
    # Get Topic name w/o macron to be used for the Icon path
    topicWithoutMacron <- str_replace_all(topic_page, paste0("M", intToUtf8(0x0101L), "ori") ,"Maori")
    HTML(
      paste0(
        "<img class='topic-heading-icon' src='img/icons-mono/2.0-RS037-Kupe-icons-", 
        paste(toupper(substr(gsub(" ", "-", topicWithoutMacron), 1, 1)), tolower(substr(gsub(" ", "-", topicWithoutMacron), 2, nchar(gsub(" ", "-", topicWithoutMacron)))), sep="") ,
        ".svg'/><div class='topic-title'>", paste0(substr(topic_page, 1, 1), substr(tolower(topic_page), 2, nchar(topic_page))) %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori")), "</div>"
      )
    )
  })
  
  # The indicator summary
  output$summary <- renderUI({
    req(subtopic_page, indicator_page, topic_page,
        G_indicatorDescriptions)
    desc <- indicator_desc()
    req(desc)
    createIndicatorDefinition(desc$description, subtopic_page, topic_page, desc$app.title, desc$app.description, desc$app.definition.intro, desc$app.definition.question)
  })
  
  # Output the bottom two charts to the UI
  output$totalGraphs <- renderUI({
    div(class="row flex-row",
        
        # Age Distribution chart
        div(class="col-sm-12 col-md-6 indicator-overview-card",
            div(class="indicator-overview-card-content",
                div(class="overview-card-header", "Age and sex"),
                p(class="chart-subtitle", ""),
                uiOutput(class = "overview-indicator-mean-unit", ns("AgeDistMeanUnit")),
                plotlyOutput(ns("ageDist"), height="auto")
            )
        ),
        # Ethnicity Chart
        div(class="col-sm-12 col-md-6 indicator-overview-card",
            div(class="indicator-overview-card-content",
                div(class="row",
                    div(class="pull-right", infoLightboxBtn(ns("lb-ARR"))),
                    div(class="overview-card-header", "Ethnicity (total)")),
                p(class="chart-subtitle", ""),
                uiOutput(class = "overview-indicator-mean-unit", ns("EthDistMeanUnit")),
                div(style="position: absolute;width: 100%;padding: inherit;left: 0;top: 40;",
                    plotOutput(ns("leftWidth"), height = paste0((nrow(ARRData()) * 85) + 15, "px"), width = "100%")
                ),
                plotlyOutput(ns("ARRPlot"), height = "auto")
            )
        )
    )
  })
  
 
  # ========================================== TAB: SUBGROUPS COMPARISON ===========================================  
  
  # Render the Subgroups Comparison title and subtitle
  output$subgroupsCompTitle <- renderUI({
    req(comparison.tableData()$comparisons.2)
    req(nrow(comparison.tableData()$comparisons.2)>0)
    
    # Not show a Subtitle for a Mean indicator
    if(mainData()$estimate.type[1] == "%"){
      subtitle <- "Adjusted prevalence ratios are used to compare the results for different population subgroups.
      An adjusted ratio above 1 shows that the indicator is more likely, and an adjusted ratio below
      1 shows the indicator is less likely, in the group of interest (e.g. M\u0101ori) than the reference group
      (e.g. non-M\u0101ori), after adjusting for demographic variables that could influence the association."
    }else{
      subtitle <- "The adjusted difference is called a regression coefficient. 
      A value below 0 shows the indicator has the lower occurrence in the group of 
      interest (e.g. M\u0101ori) than the reference group (e.g. non-M\u0101ori), after adjusting for 
      demographic variables that could influence the association. A value above 1 shows the opposite. 
      A value of 1 means there is no difference between the group of interest and reference group."
    }
    div(class="chart-title", paste0("Subgroups comparison (", latestYear(), ")"),
        p(class="chart-subtitle", subtitle)
      )
  })
  
  # All the data for the Subgroups Comparison
  ARRData <- eventReactive(indicator_page, {
    req(indicator_page)
    eth <- mainData() %>% filter(dim_var == "eth_1_total", year == latestYear())
    eth
  })
  
  # A template container for the Subgroups comparison data to be fed into
  comparison.table.container <- function(){
    cellTitle <- if(selected.indicator()$estimate.type == "Mean"){
      "Adjusted difference"
    } else {
      "Adjusted ratios"
    }
    
    htmltools::withTags(table(
      class = "nowrap display",
      thead(
        tr(
          th(style="text-align:left; padding-left: 10px", rowspan = 2, "Population groups being compared"),
          th(colspan = 3, cellTitle, infoLightboxBtn(ns("lb-sub-ARR")))
        ),
        tr(
          th("Ratio"),
          th("(95% CI)"),
          th("Adjustment variables")
        )
      )
    ))
  }
  
  output$subgroupsLightboxText <- renderUI({
    if(selected.indicator()$estimate.type != "Mean"){
      tagList(
        p("For example, an adjusted ratio of 1.50 for M\u0101ori vs non-M\u0101ori means that the indicator is 1.5 times more 
        likely in M\u0101ori than non-M\u0101ori, after adjusting for differences in demographic variables such as sex and age."),
        p("A bold value indicates that the adjusted ratio is statistically significant (", tags$span(style="margin-left:-3px", class='italic', "p "), "value \u003C .05). ")
      )
    } else {
      tagList(
        p("For example, an adjusted difference of 1.50 for M\u0101ori vs non-M\u0101ori means that the indicator average for M\u0101ori 
        is 1.5 higher than for non-M\u0101ori, after adjusting for differences in demographic variables such as sex and age."),
         p("A bold value indicates that the adjusted difference is statistically significant (", tags$span(class='italic', "p "), "value \u003C .05). ")
      )
    }
  })
  
  # All Subgroups Comparisons for the selected Indicator
  selected.comparisons <- reactive({
    dat <- G_RateRatios %>%
      filter(indicator == indicator_page)
    # If any data exists, show the Download and Notes sections, otherwise hide them
    if(nrow(dat) < 1){
      notesAndDLControl$showComp <- F
    }else{
      notesAndDLControl$showComp <- T
    }
    dat
  })
  
  # The full data to go into the comparison data table
  comparison.tableData <- reactive({
    # Do some formatting
    comparisons.1 <- selected.comparisons() %>%
      mutate(
        # Add macron to Maori
        comparison = str_replace_all(comparison, "Maori", "M\u0101ori"),
        adjusted.rate.ratio.CI = format.CI(round(adjusted.rate.ratio.low.CI, 2),
                                           round(adjusted.rate.ratio.high.CI, 2),
                                           nsmall = 2),
        significant = adjusted.rate.ratio.low.CI > 1 | adjusted.rate.ratio.high.CI < 1
      ) 
    
    comparisons.1$adjusted.rate.ratio <- sapply(comparisons.1$adjusted.rate.ratio, function(i) {formatroundL(i, 2)})
    
    comparisons.1 <- comparisons.1 %>% select(comparison,
                                              adjusted.rate.ratio,
                                              adjusted.rate.ratio.CI,
                                              adjusted.for,
                                              sort.order,
                                              significant)
    
    # Insert rows that will serve as sub-headings in the table
    subheadings <- G_ComparisonSubgroupList %>%
      filter(!is.na(subheading)) %>%
      mutate(comparison = subheading,
             comparison = str_replace_all(comparison, "Maori", "M\u0101ori")
      ) %>%
      select(comparison, sort.order) %>%
      mutate(heading=1)
    
    # Formatted and sorted comparisons to be used in the table
    comparisons.2 <- comparisons.1 %>%
      mutate(heading=0) %>%
      bind_rows(subheadings) %>%
      arrange(sort.order)%>%
      filter(!(heading==1 & lead(heading)==1)) %>%
      select(-sort.order, -heading)

    reactiveValues(comparisons.1=comparisons.1, comparisons.2=comparisons.2, subheadings=subheadings)
  })
  
  # The notes that will go underneath the table
  output$comparisonNotes <- renderUI({
    if(notesAndDLControl$showComp){
      div(
        fluidRow(
          tags$p(style = "color: #9c9c9c; margin: 30px 0",
                 "Source: Health and Lifestyles Survey")
        ),
        fluidRow(
          p("Notes:"),
          htmltools::withTags(
            if(selected.indicator()$estimate.type != "Mean"){
              ul(
                li("This table gives comparisons as adjusted prevalence ratios."),
                li("Population groups being compared are based on total ethnicity."), #FIXME: italicise if needed
                li("Neighbourhood deprivation groups are least (1 - 3), mid (4 - 7) and most deprived (8 - 10)."),
                li("The model for neighbourhood deprivation used prioritised ethnicity as an adjustment variable. "),
                li("Dashes indicate that reliable estimates are unavailable due to, for example, an insufficient sample size or unusually wide confidence interval.")
              )
            } else {
              ul(
                li("This table gives comparisons as adjusted mean differences."),
                li("The model for neighbourhood deprivation used prioritised ethnicity as an adjustment variable. "),
                li("Dashes indicate that reliable estimates are unavailable due to an insufficient sample size or an unusually wide confidence interval.")
              )
            }
            
          )
        )
      )
    }
  })
  
  output$overviewNotes <- renderUI({
    div(
      p("Notes:"),
      htmltools::withTags(
          ul(
            li("Missing bar in chart indicates insufficient sample size.")
          )
      )
    )
  })
  
  # Generate the Subgroups comparison table
  comparison.tableHTML <- reactive({
    
    # Ensure the data is available
    req(comparison.tableData()$comparisons.2)
    validate(need(nrow(comparison.tableData()$comparisons.2)>0, "No subgroups comparison available."))
    
    filter <- unlist(comparison.tableData()$subheadings["comparison"], use.names = FALSE)
    dat <- comparison.tableData()$comparisons.2
    
    # to avoid the row of group name being replaced by default content "-"
    dat[dat$comparison %in% filter, -which(names(dat)=="comparison")] <- "  "
    
    # Create the datatable object for the user interface
    sig <- dat$significant
    
    dat$adjusted.rate.ratio <- apply(dat[,c("adjusted.rate.ratio", "significant")], 1, function(x) 
    {ifelse(x["significant"] == TRUE, HTML(paste0('<span style="font-weight: bold">',x["adjusted.rate.ratio"],'</span>')), x["adjusted.rate.ratio"])})
    
    dat <- dat %>%
      select(-significant)
    dt <- datatable(dat,
                    # Use the column headings set up in  the appropriate container
                    container = comparison.table.container(),
                    escape = F,
                    # Turn off various default features
                    rownames = FALSE,
                    options = list(paging = FALSE,
                                   bInfo = FALSE,
                                   searching = FALSE,
                                   scrollX = TRUE,
                                   ordering= F,
                                   columnDefs = list(
                                     list(
                                       targets = '_all',
                                       defaultContent = '-'
                                     ))
                    )
    ) %>%
      # Make sub-headings bold
      formatStyle(columns = "comparison",
                  fontWeight = styleEqual(comparison.tableData()$subheadings$comparison,
                                          rep("bold",
                                              times = length(comparison.tableData()$subheadings$comparison))
                  )
      ) %>%
      formatStyle(1:ncol(comparison.tableData()$comparisons.2), 1, target = "cell", "borderTop"=styleEqual(comparison.tableData()$subheadings$comparison,
                                                                                                           rep("solid 1px black",
                                                                                                               times=length(comparison.tableData()$subheadings$comparison)))) %>%
      #formatStyle(columns = "adjusted.rate.ratio", fontWeight = styleEqual(comparison.tableData()$comparisons.2$adjusted.rate.ratio,
      #                                                                     ifelse(sig %in% T, "bold", "normal"))) %>%
      formatRound(columns = grep("adjusted.rate.ratio", names(comparison.tableData()$comparisons.1), fixed=T, value=T), digits = 2) %>%
      formatStyle(columns = grep("CI", names(comparison.tableData()$comparisons.1), fixed=T, value=T), color = "grey")
  })
  
  # Render the generated table to the UI
  output$comparison.table <-
    DT::renderDataTable({
      comparison.tableHTML()
    })
  
  
  
  # ========================================== TAB: CHANGES OVER TIME =========================================== 
  
  # All the data for the Timeseries
  timeseriesData <- reactive({
    req(mainData())
    dat <- mainData() %>%
      filter(group == "Total") %>%
      arrange(year)
    dat$year <- as.numeric(dat$year)
    
    if(nrow(dat)>0){
      dat
      #Wa added the below line
      #dat<- dat[rowSums(is.na(dat)<ncol(dat)),]
    }else{
      return(data.frame())
    }
  })
  
  # Render the "Changes over time" title, followed by the correct subtitle 
  output$changeOTInfo <- renderUI({
    req(timeseries.tableData())
    req(nrow(timeseries.tableData()$timeseries.2) > 0)
    div(div(class="chart-title", "Changes over time"),  uiOutput(ns("timeseriesSubtitletable")))
  })
  
  # Determine the correct Subtitle based on whether the indicator is Mean or Prevalence
  output$timeseriesSubtitletable <- renderUI({
    req(mainData())
    req(nrow(mainData())>0)
    isPrev <- mainData()[1,]$estimate.type=="%"
    if(isPrev){
      p(class="chart-subtitle", "This table presents unadjusted results; that is, the prevalence estimates reflect the actual percentage for the specified population in each survey year. Statistically significant differences between years are shown in bold (",
        HTML(paste0("<span class='italic'>p</span> value \u003C .05)."))
      )
    }else{
      p(class="chart-subtitle", "This table presents unadjusted results; that is, the mean estimates for the specified population in each survey year. Statistically significant differences between years are shown in bold (",
        HTML(paste0("<span class='italic'>p</span> value \u003C .05)."))
      )
    }
  })
  
  # Filters timeseries data to fit the indicator chosen
  selected.timeseries <- reactive({
    allData <- G_timeseries %>%
      filter(indicator == indicator_page)
    
    if(all(is.na(allData[which.p.value.columns]))) {
      allPvalueCols <- colnames(allData)[grepl("p.value", colnames(allData))]
      oldPvalueCols <- setdiff(allPvalueCols, which.p.value.columns)
      olderData <- allData %>% select(population,
                                             indicator,
                                             group,
                                             one_of(which.time.trend.columns),
                                             one_of(oldPvalueCols),
                                             sort.order)
      allData <- olderData
    }
    
    
    if(nrow(allData) < 1){
      notesAndDLControl$showTS <- F
    }else{
      notesAndDLControl$showTS <- T
    }
    allData
  })
  
  # The Notes to go below the table
  timeseriesNoteData <- reactive({
    req(mainData())
    req(nrow(mainData())>0)
    isPrev <- mainData()[1,]$estimate.type == "%"
    c(
      "Total response measure of ethnicity was used. People who reported belonging to more than one ethnic group are counted once in each group they reported.",
      "If there is no data for the most recent survey year we have used the second most recent survey year as the reference year for comparisons over time.",
      # As per request from HPA, Heavy drinking note added. Adjust text and ordering as needed
      if(subtopic_page == "Alcohol attitudes" | subtopic_page == "Alcohol cutting back") {
        HTML(
          "Heavy drinking is defined as 5 or more standard drinks for women and 6 or more standard drinkers for men. The definition of a standard drink can be found on 
           <a href='http://www.alcohol.org.nz/help-advice/standard-drinks/whats-a-standard-drink', target='_blank'>
           http://www.alcohol.org.nz/help-advice/standard-drinks/whats-a-standard-drink</a>" 
        )      
      },
      "Dashes indicate that the data is not available."
    )
  })
  
  # Render the Notes to the UI 
  output$timeseriesNotes <- renderUI({
    if(notesAndDLControl$showTS){
      div(
        fluidRow(
          tags$p(style = "color: #9c9c9c; margin: 30px 0",
                 "Source: Health and Lifestyles Survey")
        ),
        fluidRow(
          tagList(
            p("Notes:"),
            htmltools::withTags(
              ul(
                lapply(timeseriesNoteData(), function(note){ li(HTML(note))})
              )
            )
          )
        )
      )
    }
    
  })
  
  # Create a container for the data to be fed into
  timeseries.table.container <- function(indicator.type,colnames=NULL) {
    
    pvaluecols <- colnames[grep("p.value", colnames)]
    shortpyears <- pvaluecols %>% str_remove("p.value") %>% substr(5, nchar(pvaluecols))
    pvalueyears <- G_yearsList[G_yearsList$short.year %in% shortpyears, ]
    
    timetrendcols <- colnames[grep("percent", colnames)]
    shorttrendyears <- timetrendcols %>% str_remove("percent.")
    timetrendyears <- G_yearsList[G_yearsList$short.year %in% shorttrendyears, ]
    
    
    showP <- "p value" %in% input$TSCols
    
    if(showP) {
    latestYear <- G_yearsList[grep(pvalueyears[nrow(pvalueyears), ]$full.year, G_yearsList$full.year) + 1, ]$full.year
    }
    
    
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(style="text-align:left; padding-left: 10px", rowspan = 2, "Population group"),
          th(colspan = length(timetrendcols),
             if (indicator.type == "%") {
               "Unadjusted prevalence"
             }
             else if (indicator.type == "Mean") {
               "Unadjusted mean"
             }
          ),
          if(showP){ th(colspan = length(pvaluecols),
                        HTML(paste0("Statistical significance of difference between years (", span(class="italic", "p"), " value)")),
                        infoLightboxBtn(ns("lb-ts-sig"))
          )}
        ),
        tr(
          lapply(timetrendyears$full.year, th),
          if(showP){ 
              lapply(paste(pvalueyears$full.year, "and", latestYear), th)
          }
        )
      )
    ))
  }
  

  # Returns the indicator that matches the inputted topic, subtopic, and inidicator
  selected.indicator <- reactive({
    req(indicator_page)
    G_indicatorList %>%
      filter(indicator == indicator_page) %>%
      as.list()
    })
  
  
  # Returns the timeseries reactive data
  timeseries.tableData <- reactive({
    
    #Precondition
    req(selected.indicator()$single.year.ages == 1 || selected.indicator()$single.year.ages == 0 ||
          is.na(selected.indicator()$single.year.ages))
    req(selected.timeseries(), selected.indicator())
    options("scipen"=100, "digits"=3)
    
    timeseries.1 <- selected.timeseries() %>%
      mutate(# Add macron to Maori
        group = str_replace_all(group, "Maori", "\u0101ori"),
        heading=0
      ) %>% 
      select(-population,
             -indicator)
  
    
    if(!nrow(timeseries.1)>0){
      return(data.frame())#return empty dataframe so that the HTML will display an error
    }

    # Insert rows that will serve as sub-headings in the table
    # Note the table has a different structure if this is a child indicator for
    # which we are presenting by single year of age
    is.single.year <- TRUE
    if (is.na(selected.indicator()$single.year.ages) || selected.indicator()$single.year.ages == 0) {is.single.year <- FALSE}
    
    if (is.single.year) {
      
      subheadings <- G_timeseriesSubgroupList %>%
        filter(!is.na(subheading.single.year)) %>%
        mutate(group = subheading.single.year,
               group = str_replace_all(group, "Maori", "\u0101ori"),
               heading=1) %>%
        select(group, sort.order, heading)
      
    } else {
      
      subheadings <- G_timeseriesSubgroupList %>%
        filter(!is.na(subheading)) %>%
        mutate(group = subheading,
               group = str_replace_all(group, "Maori", "\u0101ori"),
               heading=1
        ) %>%
        select(group, sort.order, heading)
      
    }
    
    if(!"p value" %in% input$TSCols){
      timeseries.1 <- timeseries.1 %>%
        select(-contains("p.value"))
    }
    
    # Final timeseries data to be used in the table
    timeseries.2 <- timeseries.1 %>%
      bind_rows(subheadings) %>%
      arrange(sort.order) %>%
      filter(!(heading==1 & lead(heading)==1)) %>%
      select(-sort.order, -heading)
    
    # Formatting for Downloading the data
    dlData <- timeseries.2
    dlData[is.na(dlData)] <- -1
    dlData[, grepl( "percent" , names(dlData))] <- format(round(as.data.frame(dlData[, grepl( "percent" , names(dlData))]), digits=3), nsmall = 3)
    dlData[, grepl( "p.value" , names(dlData))] <- format(round(as.data.frame(dlData[, grepl( "p.value" , names(dlData))]), digits=3), nsmall = 3)
    
    
    
    dlData[dlData == "-1.000"] <- ""
    if(selected.indicator()$estimate.type != "%"){
      dlData <- dlData %>%
        rename_at(vars(starts_with("percent.")),
                  toMeanTitle)
    }
    
    # Determine the year columns that will need to be removed based on the Topic
    colsToRemove <- yearsToOmit()$tsCols[yearsToOmit()$tsCols %in% colnames(dlData)]
    if(!is.null(colsToRemove)){
      dlData <- dlData %>% select(-(colsToRemove))
    }
    # If the latest comparison data exist, remove all old ones
    if("p.value.18.16" %in% colnames(dlData)) {
      oldPcols <- colnames(dlData)[(grep("p.value.16", colnames(dlData)))]
      dlData <- dlData %>% select(-oldPcols)
    }
    
    reactiveValues(timeseries.1=timeseries.1, timeseries.2=timeseries.2, subheadings=subheadings, downloadData=dlData)
  })
  
  
  
  
  # Format the Table for timeseries data
  timeseries.tableHTML <- reactive({
    # Validate inputs
    validate(
      need(timeseries.tableData(), "No changes over time available."),
      need(nrow(timeseries.tableData()$timeseries.2) > 0, "No changes over time available.")
    )
  
    filter <- unlist(timeseries.tableData()$subheadings["group"], use.names = FALSE)
    dat <- timeseries.tableData()$timeseries.2
    
    # to avoid the row of group name being replaced by default content "-"
    dat[dat$group %in% filter, -which(names(dat)=="group")] <- " "
    pValueCols <- NULL 
    if("dim_var_sequencing" %in% colnames(dat)){
     dat <- dat %>% select(-dim_var_sequencing)
    }
    
    # Replace p-value columns with formatted p-value data
    dat[,colnames(dat)[grep("p.value", colnames(dat))]] <- lapply(dat[,colnames(dat)[grep("p.value", colnames(dat))]], function(column){
      lapply(column, function(c){
        formatPvalue(c)
      }) %>% unlist()
    }) %>% as.data.frame()
    # Determined at a Topic level, find out the years that should not be shown for that Topic
    colsToRemove <- colnames(dat)[colnames(dat) %in% yearsToOmit()$tsCols]
    # Remove those years. Also, if latest years of comparison data exists, remove all previous ones
    # this may need to be adapted in the event of a 2020 dataset.
    dat <- dat %>% select(-colsToRemove)
    if("p.value.18.16" %in% colnames(dat)) {
      oldPcols <- colnames(dat)[(grep("p.value.16", colnames(dat)))]
      dat <- dat %>% select(-oldPcols)
    }
    # Determine the names of the pvalues and timetrend columns in the adjusted table
    pvalcols <- colnames(dat)[grep("p.value", colnames(dat))]
    timetrendcols <- colnames(dat)[grep("percent", colnames(dat))]
 
    dt <- datatable(dat,
                    # Use the column headings set up in  the appropriate container
                    container = timeseries.table.container(selected.indicator()$estimate.type, colnames(dat)),
                    escape=F,
                    # Turn off various default features
                    rownames = FALSE,
                    options = list(paging = FALSE,
                                   bInfo = FALSE,
                                   searching = FALSE,
                                   scrollX = TRUE,
                                   ordering= F,
                                   columnDefs = list(
                                     list(
                                       targets = '_all',
                                       defaultContent = '-'
                                     ),
                                     list(
                                       targets = 0,
                                       width = '20%' 
                                     ))
                    )
    ) %>%
      # Make sub-headings bold
      formatStyle(columns = "group",
                  fontWeight = styleEqual(timeseries.tableData()$subheadings$group,
                                          rep("bold",
                                              times = length(timeseries.tableData()$subheadings$group))
                  )
      ) %>%
      formatStyle(1:ncol(timeseries.tableData()$timeseries.2), 1, target = "cell", "borderTop"=styleEqual(timeseries.tableData()$subheadings$group,
                                                                                                          rep("solid 1px black",
                                                                                                              times=length(timeseries.tableData()$subheadings$group)))) %>%
      # Make sure prevalences/means display with 1 decimal place
      formatRound(columns = timetrendcols, digits = 1)
    # Bold significant figures
    if("p value" %in% input$TSCols){
      dt <- dt %>%
        formatStyle(columns = pvalcols,
                    color = styleInterval(0.05, c("black", "grey")),
                    fontWeight = styleInterval(0.05, c("bold", "normal")))
    }
    dt
  })
  
  # Render the generated datatable to the UI
  output$timeseries.table <- DT::renderDataTable({timeseries.tableHTML()})

  # Display checkbox to show/hide p-value cols
  output$pValueCheckbox <- renderUI({
     if(!is.null(selected.timeseries()))
     {
       if(nrow(selected.timeseries()) > 0)
       {
         checkboxGroupInput(ns("TSCols"), "Show:", c("p value"), c("p value"))
       }
     }
  })
  
# =================================================== DOWNLOAD ==========================================================
  
  
  # Render the downloaded section to the UI based on which tab is selected
  output$downloads <- renderUI({
    tab <- input$tabset
    if(tab == "Overview"){
      links<-tagList(downloadLink(ns("DLOverviewHTML"), "Charts (zip)")
      )
    }else if(tab=="Prevalence / mean"){
      links<-tagList(downloadLink(ns("DLPrevCSV"), "Data (csv)"))
    }else if(tab=="Subgroups comparison"){
      req(comparison.tableData()$comparisons.2)
      req(nrow(comparison.tableData()$comparisons.2) > 0)
      req(notesAndDLControl$showComp == T)
      links<-tagList(downloadLink(ns("DLCompCSV"), "Data (csv)"))
    }else if(tab=="Changes over time"){
      req(timeseries.tableData())
      req(nrow(timeseries.tableData()$timeseries.2) > 0)
      req(notesAndDLControl$showTS == T)
      links<-tagList(downloadLink(ns("DLChangesCSV"), "Data (csv)"))
    }
    createDownloadArea(links)
  })
  
  
  
  # Format chart HTML for chart download
  formatHTML <- function(x, type) {
    w <- x() #%>% layout(width = "600px")
    w$width <- "600px"
    h <- htmlwidgets::prependContent(w,
                                      tags$style(HTML("
                                                      .dl_html_title {
                                                      text-align: center; font-size: 130%; 
                                                      }
                                                      .dl_html_content {
                                                      text-align: left; margin-top: 1px; margin-bottom: 1px
                                                      }
                                                      ")),
                                     # Add summary details
                                      tags$p(class="dl_html_content", paste0("Topic: ", topic_page)),
                                      tags$p(class="dl_html_content", paste0("Subtopic: ", subtopic_page)),
                                      tags$p(class="dl_html_content", paste("Indicator:", indicator_desc()$short.description)),
                                      tags$p(class="dl_html_content", paste(indicator_desc()$app.definition.intro)),
                                      tags$p(class="dl_html_content", style="margin-bottom: 30px", paste0("Year: ", latestYear())), 
                                      if(type == "Ethnicity (total)") {tags$p(class="dl_html_content", "Ethnicity (total)")},
                                      tags$p(class="dl_html_content", ifelse(!isNotMeanInd(), overviewData()[1,]$estimate.unit, " "))
                                      )

    htmlwidgets::appendContent(h, tags$p("Source: Health and Lifestyles Survey"))
    
    
  }

  # Format filename for chart download
  overviewHTMLFileName <- reactive({
    paste0("_", indicator_desc()$indicator, "_overview")
  })
  
  # Download HTML charts
  output$DLOverviewHTML <- downloadWidgets(function(){overviewHTMLFileName()}, 
                                           "overall-prevalence"=reactive(formatHTML(function(){overviewChartHTML()}, "Prevalence")),
                                           "timetrends"=reactive(formatHTML(function(){overviewTSHTML()}, "Time trends")),
                                           "ethnicity-total"=reactive(formatHTML(function(){ARRHTML()}, "Ethnicity (total)")),
                                           "age-sex"=reactive(formatHTML(function(){distPlotHTML()}, "Age and Sex")))

  
  # Format prevalence/mean data for CSV download
  prevCSV <- reactive ({
    dat <- prevalence.tableData()$prevalences.csv
    if(est.type() == "Mean"){
      dat <- dat %>% select(-contains("estimated"))
    }
    dat <- apply(dat,2,function(x) trimws(x))
    as.data.frame(dat)
  })
  
  # Format comparisons data for CSV download
  compCSV <- reactive({
    dat <- comparison.tableData()$comparisons.2
    dat$adjusted.for <- str_replace_all(dat$adjusted.for, ",", ";") 
    dat <- apply(dat,2,function(x) trimws(x))
    as.data.frame(dat)
  })
  
  # Format timeseries data for CSV download
  tsCSV <- reactive({
    dat <- timeseries.tableData()$downloadData
    # correcting error to ensure that the downloads work for 2016
    # see email from Uli RE: Kupe download fail: Alcohol sub-topics
    if("dim_var_sequencing" %in% colnames(dat)) {
      dat <- dat %>% select(-dim_var_sequencing)
    }
    
    # Epi-interactive: The download section has been adjusted to cope with dynamic columns and column names
    # due to certain years and pvalue years being surpressed.
    
    # First, round all timetrend columns to 1dp
    dat[colnames(dat)[grep("percent", colnames(dat))]] <- 
      dat[colnames(dat)[grep("percent", colnames(dat))]] %>%
      lapply(function(column) { 
        lapply(column, function(c) { formatroundL(c, 1)  }) %>% unlist()
      })
    
    # The round all pvalue years to 3dp
    dat[colnames(dat)[grep("p.value", colnames(dat))]] <-
      dat[colnames(dat)[grep("p.value", colnames(dat))]] %>%
      lapply(function(column) { 
        lapply(column, function(c) { formatroundL(c, 3)  }) %>% unlist()
      })
    dat
  })
  
  # Format prevalence filename
  prevMeanFileName <- reactive({
    paste0("_", indicator_desc()$indicator, "_prevalence-mean")
  })
  
  # Create CSV for Prevalence/Mean
  output$DLPrevCSV <- downloadCsv(function(){prevMeanFileName()}, 
                                  c(function(){"Source: Health and Lifestyles Survey"},
                                    function(){indicator_desc()$app.definition.intro},
                                    function(){paste0("Year: ", ifelse(!is.null(input$prevalYear), input$prevalYear, allyears()[[1]]))}, 
                                    function(){paste0("Subpopulation: ", indicator_desc()$fullpopname)},
                                    function(){paste("Indicator:", indicator_desc()$short.description)},
                                    function(){paste0("Subtopic: ", subtopic_page)},
                                    function(){paste0("Topic: ", topic_page)}), 
                                    function(){prevCSV()},
                                  function(){est.type()}
                                  )
  
  # Format subgroups comparion filename
  subgroupCompFileName <- reactive({
    paste0("_", indicator_desc()$indicator, "_subgroups-comparison")
  })
  
  # Create CSV for Subgroups Comparison
  output$DLCompCSV <- downloadCsv(function(){subgroupCompFileName()},  
                                  c(function(){"Source: Health and Lifestyles Survey"},
                                    function(){indicator_desc()$app.definition.intro},
                                    function(){paste0("Year: ", allyears()[[1]])}, 
                                    function(){paste0("Subpopulation: ", indicator_desc()$fullpopname)},
                                    function(){paste("Indicator:", indicator_desc()$short.description)},
                                    function(){paste0("Subtopic: ", subtopic_page)},
                                    function(){paste0("Topic: ", topic_page)}), 
                                  function(){compCSV()})
  
  # Format changes over time filename
  changeOTFileName <- reactive({
    paste0("_", indicator_desc()$indicator, "_changes-over-time")
  })
  
  # Create CSV for changes over time
  output$DLChangesCSV <- downloadCsv(function(){changeOTFileName()}, 
                                     c(function(){"Source: Health and Lifestyles Survey"},
                                       function(){indicator_desc()$app.definition.intro},
                                       function(){paste0("Subpopulation: ", indicator_desc()$fullpopname)},
                                       function(){paste("Indicator:", indicator_desc()$short.description)},
                                       function(){paste0("Subtopic: ", subtopic_page)},
                                       function(){paste0("Topic: ", topic_page)}), 
                                     function(){tsCSV()})
  
  
    }
