
# Imports 
library(tidyr)
library(dplyr)
library(DT)
library(xml2)


# Server function for pageExploreTopics provides the backend for the Explore Topics UI page.

pageExploreTopics <- function(input, output, session, name, topic_page, subtopic_page) {
  ns <- session$ns
  
  # ====================================== OBSERVE EVENTS ===========================================
  
  localMsg <- reactiveValues(
    topic = topic_page,
    subtopic = subtopic_page
  )
  
    splashPassTopic <- NULL
    splashPassSubTopic <- NULL
    splashPassTopic <- localMsg$topic
    splashPassSubTopic <-  localMsg$subtopic

    topChoices <- (unique(G_indicatorList$topic) %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori"))) %>% sort()
    topSelected <- (G_indicatorList %>% filter(tolower(topic) %in% tolower(str_replace_all(splashPassTopic, "Maori", paste0("M", intToUtf8(0x0101L), "ori")))))$topic  %>% unique()
    
    subtopicChoices <- unique((G_indicatorList %>% filter(tolower(topic) %in% tolower(topSelected)))$subtopic) 
    subSelected <- (G_indicatorList %>%
                      filter(tolower(topic)  %in% tolower(topSelected),
                             tolower(subtopic) %in% tolower(splashPassSubTopic)
                      ))$subtopic %>% unique()
    
    updateSelectInput(session, "subtopic", choices = subtopicChoices, selected = subSelected) 
    updateSelectInput(session, "topic", choices = topChoices, selected = str_replace_all(topSelected,"Maori",  paste0("M", intToUtf8(0x0101L), "ori"))) 
  
  
  # Look for event change on topic drop down
  # If changed find the first subtopic then change page
  onclick("topic", function(event){
    print(input$topic)
    print(topic_page)
    if(input$topic != topic_page){
      topSelected <- ((G_indicatorList %>% 
                         filter(tolower(topic) %in% tolower(input$topic %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori")))))$topic %>% unique())
      subtopicChoices <- unique((G_indicatorList %>% filter(tolower(topic) %in% tolower(topSelected)))$subtopic)
      subSelected <- subtopicChoices[[1]]
      change_page(paste(convert2Query(c(input$topic, subSelected)), collapse = "/"))
    }
    })
  
  # Look for event change on subtopic drop down 
  onclick("subtopic", function(event){
    if(input$subtopic != subtopic_page){
      localMsg$subtopic <- input$subtopic
      print(paste(convert2Query(c(input$topic, input$subtopic)), collapse = "/"))
      change_page(paste(convert2Query(c(input$topic, input$subtopic)), collapse = "/"))
    }
  })
  

  # When bread crumb link pressed
  onclick("breadLinks", {
    if(!is.null(input$breadLinks)){
      shinyjs::runjs("window.scrollTo(0, 0)")
      change_page(convert2Query(input$breadLinks))
    }
  })
  

  # ====================================== PAGE DATA ===========================================
  
  # Full data for indicators in the table. Includes prevalence data
  mainData <- eventReactive({topic_page; input$group},{
    req(subtopic_page, topic_page, input$group)
    
    # Set of indicators matching the chosen subtopic and topic 
    ind <- subset(G_indicatorList, tolower(subtopic) == tolower(subtopic_page) & tolower(topic) == tolower(topic_page), c("indicator", "description",
                                                                                                                            "short.description", "indicator.order",
                                                                                                                            "estimate.type"))
    
    # Merge matching indicators from G_prevalences and G_indicator so we have a full set of data to work with.
    dataset <- subset(G_prevalences, tolower(subtopic) == tolower(subtopic_page) & tolower(group) == tolower(input$group))
    merge(ind, dataset, all.x=TRUE, by="indicator") %>%
      arrange(indicator.order)
  })
  
  # Subtopic Description
  subtopicDesc <- reactive({
    req(topic_page, subtopic_page)
    content <- (G_subtopicDescriptions %>% filter(topic == topic_page, subtopic == subtopic_page))$app.description
    content
  })
  
  # Timeseries Data that is used in the Prevalence and Mean datatables
  timeseriesData <- reactive({
    req(subtopic_page, topic_page, input$groupTbl)
    if(input$groupTbl != "Total"){
      grp <- substr(input$groupTbl, 1, nchar(input$groupTbl)-8)
    }else{
      grp <- "Total"
    }
    
    indicators <- G_indicatorList %>% filter(topic == topic_page, subtopic == subtopic_page)
    timeseries <- indicators %>% left_join(G_timeseries, by="indicator") %>% filter(group == grp)
    
    availableData <- timeseries# %>% 
    # 
    
    
    
    if(nrow(timeseries) == nrow(indicators)){
      allData <- availableData
    }else{
      missingIndicators <- setdiff(indicators$indicator,timeseries$indicator)
      missingData <- getMissingYearData(missingIndicators, grp)
      
      allData <- bind_rows(availableData, missingData)
    }
    
    # If there isn't any data for the p-value columns for the latest year, use the older years data
    if(all(is.na(allData[which.p.value.columns]))){
      allPvalueCols <- colnames(timeseries)[grepl("p.value", colnames(timeseries))]
      oldPvalueCols <- setdiff(allPvalueCols, which.p.value.columns)
      olderData <- allData %>% select(short.description, which.time.trend.columns, oldPvalueCols, estimate.type, group)
      allData <- olderData
    } else {
      allData <- allData %>% select(short.description, which.time.trend.columns, which.p.value.columns , estimate.type, group)
    }
    allData
    
    # Epi-interactive - issue #4: Surpress years with no data
    # ==================================================================
    
    # Supressed years are determined on a Topic level,
    # while the data we currently have is at a Subtopic level
    
    # Get all the indicators for the topic and their timeseries data
    allTopicInds <- G_indicatorList[G_indicatorList$topic == topic_page, ]
    allTopicIndData <- allTopicInds %>% 
      left_join(G_timeseries, by="indicator") %>%
      filter(group == grp)
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
    
    totalColsToOmit <- c(yearColsToOmit, pColsToOmit)
     
    # Since the supressed years is determined at a Topic level and the data displayed is at a Subtopic level, 
    # some columns in colsToOmit may not exist in allData. 
    # Here we just select the columns in colsToOmit that exist in allData
    unneededForThisSubtopic <- totalColsToOmit[totalColsToOmit %in% colnames(allData)]
    # Remove the unneeded cols
    if(!is.null(unneededForThisSubtopic)) {
      allData <- allData %>% select(-unneededForThisSubtopic)
    }
    allData
  })
  
  # Formatted version of timeseriesData to properly display in the table
  tableData <- reactive({
    dat <- timeseriesData()
    if(nrow(dat) > 0){
      datOrder <- G_indicatorList %>% select("indicator.order", "short.description")
      dat <- left_join(dat, datOrder, by="short.description", all.x=T)
      dat <- dat %>% arrange(indicator.order)
    }
    dat <- dat %>% select(-contains("indicator.order")) 
    dat
  })
  
  # Data for Mean datatable. This is the tableData but filtering out the Prevalence indicators
  meanTableData <- reactive({
    tableData() %>%
      filter(estimate.type == "Mean") %>%
      select(-estimate.type)
  })
  
  # Data for Prevalence datatable. This is the tableData but filtering out the Mean indicators
  prevTableData <- reactive({
    validate(need(nrow(tableData()) > 0, "No prevalence for selected subtopic available."))
    tableData() %>%
      filter(estimate.type == "%") %>%
      select(-estimate.type)
  })
  
  # Formats Prevalence table data into CSV form
  prevCSV <- reactive({
    # Remove unneeded group column
    dat <- prevTableData() %>%  select(-matches("group"))
    

    colnames(dat)[colnames(dat)=="short.description"] <- "Indicator"
    
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
        lapply(column, function(c) { formatroundL(c, 1)  }) %>% unlist()
      })

    dat
  })
  
  # Formats Mean table data into CSV form
  meanCSV <- reactive({
    dat <- meanTableData() %>%  select(-matches("group"))
    
    colnames(dat)[colnames(dat)=="short.description"] <- "Indicator"
    
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
        lapply(column, function(c) { formatroundL(c, 1)  }) %>% unlist()
      })
    dat
  })
  
  # ====================================== UTILITY FUNCTIONS AND VALUES ===========================================
  
  # Returns a formatted list of Ethnicities to filter by 
  getGroupChoices <- function(){
    ret <- names(G_ethnicities) %>%
      sapply(function(choice){
        if(choice == "Total"){
          choice <- choice
        }else{
          choice <- paste0(choice, " (total)")
        }
      }, USE.NAMES=F) %>% unname() %>% str_replace_all("Maori", paste0("M", intToUtf8(0x0101L), "ori"))
    
    ret
  }
  
  # Indentifies the column names for timeseries years 
  time.trend.years <- G_yearsList %>%
    filter(show.in.time.trends == 1)
  which.time.trend.columns <- paste0("percent.", time.trend.years$short.year)
  
  # Identifies the column names for p value (changes between) years
  # TODO: these can not be used to change csv headings as they link to the variable names in the rds datasetsets
  p.value.years <- G_yearsList %>%
    filter(show.p.value == 1)
  which.p.value.columns <- paste0("p.value.",
                                  G_current.year$short.year,
                                  ".",
                                  p.value.years$short.year)
  
  # Used by timeSeriesData to find data for indicators that have prevalence data but no timeseries data 
  # so it can be displayed in the table
  getMissingYearData <- function(indicators, grp) {
    indicators <- G_indicatorList %>% 
      filter(indicator %in% indicators) %>% 
      select("indicator", "short.description",  "estimate.type") 
    timeseries <- indicators %>% 
      left_join(G_timeseries, by="indicator")
    template <- timeseries %>% select(short.description, which.time.trend.columns, which.p.value.columns, estimate.type, group)
    prevData <- (G_prevalences %>% filter(indicator %in% indicators$indicator, group == grp)) %>% join(indicators, by ="indicator")
    
    # Create a formatted row for each row of prevData
    empty.frame <- lapply(1:nrow(prevData), function(i){
      year <- substr(prevData[i, ]$year, 3, 4)
      value <- prevData[i, ]$total
      desc <- prevData[i, ]$short.description
      group <- prevData[i, ]$group
      est.type <- prevData[i, ]$estimate.type
      frame <- data.frame(
        short.description=desc,
        year=value,
        group=group,
        estimate.type=est.type,
        stringsAsFactors = F
      )
      colnames(frame) <- c("short.description", paste0("percent.", year), "group", "estimate.type")
      frame
    })
    
    # Combine rows from list to dataframe
    
    df <- bind_rows(empty.frame) 
    df[is.na(df)] <- 0
    
    # Determine which timetrends years exist in the df frame
    available.time.trend.cols <- which.time.trend.columns[which(which.time.trend.columns %in% colnames(df))]
    
    # Flatten data frame
    
    
    final <- df %>% group_by(short.description,estimate.type) %>% summarise_at(.vars = vars(available.time.trend.cols), .funs = funs(sum))
    
    
    data <- as.data.frame(final) %>% join(template)
    
    data
  }
  
  
  # Formats table data with appropirate icons and links
  tableHTML <- function(id, data, unit){
    p <- unlist(which.p.value.columns)
    # Get the latest year of data  - we use this to determine which year we compare p-values to
    yr <- paste0("percent.", G_current.year$short.year)
    cmp <- paste0("percent.", p.value.years$short.year)
    if(!p %in%colnames(data)) {
      p <-  colnames(data)[grepl("p.value", colnames(data))]
    }
    req(nrow(data) > 0)
    tab <- data
    
    # Resolving #5: Check data in the latest year column is all NA.
    # If it is, we need to actually be comparing to the previous column
    if(all(is.na(tab[yr]))) {
      # Get the next latest year, and override the old yr value
      justNumber <- str_remove(yr, "percent.")
      nextLatestYear <- G_yearsList$short.year[grep(justNumber, G_yearsList$short.year) - 1]
      yr <- paste0("percent.", nextLatestYear)
    }
    
    # Resolving #4: Ensure the correct colnames are applied to the table
    # Override comparison years, since this is now dynamic
    cmp <- colnames(tab)[grepl("percent", colnames(tab))]
    
    # Add icons
    tab[,p]<-mapply(function(x, y, z){
      ifelse(is.na(x),
             "-",
             ifelse(x<0.05,
                    # symbols for significant change between years
                    ifelse(y < z,
                           "<i class='fa fa-caret-up fa-lg'></i>",
                           "<i class='fa fa-caret-down fa-lg'></i>"),
                    "&asymp;"
             )
      )
    }, tab[,p], tab[,cmp], tab[yr])

   
    descriptions <- tab %>% select(short.description)
    
    urls <- map(descriptions, function(temp){
      map(temp, function(ind) {
        paste(" '!/", paste(convert2Query(c(topic_page, subtopic_page, ind)), collapse = "/"),"' ", sep="")
        }) %>% 
        unlist() %>% paste(collapse = ", ") %>% {paste("[", .,"]", sep="")}
      })
    
    descriptions <- mapply(function(d){
      d <- map(d,~shiny::a(class="custom-link",href="", `data-toggle`='tab', value=.x, .x) %>% as.character())  %>% unlist()
    }, descriptions)
    tab$short.description <- descriptions

    cb <- JS(sprintf("
            table.on('click.dt', 'td', function(){
              var locations = %s;
              row=table.cell(this).index().row;
              col=table.cell(this).index().column;
              if(col === 0 & row < locations.length) {
                window.scrollTo(0, 0)
                window.location.hash = locations[row];
              }
            })", urls))
    createDataTable(tab, p, unit, cb)
  }
  
  # Creates the actual data table with all the values 
  createDataTable <- function(tab, p, unit, cb){
    
    timeTrendColumns <- colnames(tab)[grepl("percent", colnames(tab))]
    
    datatable(tab %>% select(-group), escape=FALSE,
              container = prevalence.table.header(unit, p, colnames(tab)),
              rownames = F,
              callback = cb,
              options = list(paging = FALSE,
                             bInfo = FALSE,
                             searching = FALSE,
                             ordering = FALSE,
                             scrollX = TRUE,
                             columnDefs = list(
                               list(
                                 targets = '_all',
                                 defaultContent = '-'
                               ))
              )) %>% 
      formatStyle(p, textAlign="center") %>%
      formatRound(columns = timeTrendColumns, digits = 1)
  }
  
  # Creates the headings for the table
  prevalence.table.header <- function(unit, pcols = NULL, colnames){
    
    if(!is.null(pcols)) {
      pColShortYears <- pcols %>% substr(nchar(pcols[1])-1, nchar(pcols[1]))
      pColYears <- p.value.years[p.value.years$short.year %in% pColShortYears, ]
    }
    
    # timetrend years 
    timeTrendCols <- colnames[grepl("percent", colnames)]
    timeTrendShortYears <- timeTrendCols %>% str_remove("percent.")
    timeTrendYears <- G_yearsList[G_yearsList$short.year %in% timeTrendShortYears, ]
    
    pColYears <- pColYears[pColYears$full.year %in% timeTrendYears$full.year, ]
    latestYear <- G_yearsList[grep(pColYears[nrow(pColYears), ]$full.year, G_yearsList$full.year) + 1, ]$full.year
    
    
    htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          # change width from 40% to 30% see what happens with layout
          th(rowspan = 2, "Indicator", width="40%"
          ),
          th(colspan = nrow(timeTrendYears), style="text-align:center",
             ifelse(unit=="%", "Year (%)", "Year"),
             # is this the information icon/link?
             infoLightboxBtn(ns("lb-year"))
          ),
          # 10% from right side of page?
          # number of columns wide is dependent on number of years being compared.
          th(colspan = if(!is.null(pcols)) { length(pcols) } else {nrow(p.value.years)}, style="text-align:center; p, width: 10%",
             "Changes between",
             infoLightboxBtn(ns("lb-changes"))
          )
        ),
        tr(
          # here is where we want two rows being used
          # reducing the px width for these headings also causes them to wrap
          lapply(timeTrendYears$full.year, th, style="text-align: right;"),
          if(!is.null(pcols)) {
            # single line of text at present
            lapply(paste(pColYears$full.year, "and", latestYear), th)
            #(pColYears$full.year,th)<br>
            # , "and", latestYear), th)
          } else {
            lapply(paste(p.value.years$full.year, "and", G_current.year$full.year), th)
          }
          
        )
      )
    ))
  }
  
  # Formatting function to change a prevalence title to a mean one
  toMeanTitle <- function(title){
    return(sub("percent.", "mean.", title))
  }
  
  # Formatting function - decapitalises topic name and replaces spaces with dashes 
  formatTopicname <- function(x) {
    x <- tolower(x)
    x <- gsub(" ", "-", x)
  }
  
  # ====================================== OUTPUTS ===========================================
  
  # Render breadcrumb
  output$breadcrumb <- renderUI({
    req(topic_page, subtopic_page)
    div(id=ns("breadLinks"), class="custom-link-binding",  
        # Breadcrumb Item: Home
        span(class="breadcrumb-item", 
             tags$a(href="", `data-toggle`="tab",
                    tags$i(id = "home_bread", class="fa fa-home")
             )
        ), ">",
        span(class="breadcrumb-item", 
             tags$a(href="", class="custom-link", topic= topic_page,  navto="subtopics-menu", `data-toggle`="tab",
                    paste0(substr(topic_page, 1, 1), substr(tolower(topic_page), 2, nchar(topic_page))) %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori"))
             )
        ), ">",
        span(class="breadcrumb-item breadcrumb-focus", 
             subtopic_page
        )
    )
  })
  

  # Render downloads area
  output$downloads <- renderUI({
    links<-tagList(
      downloadLink(ns("DLTableCSV"), "Prevalence data (csv)"),
      if(nrow(meanTableData())>0){
        tagList(
          downloadLink(ns("DLTableMeanCSV"), "Mean data (csv)")
        )
      }
    )
    createDownloadArea(links)
  })
  
  prevFileName <- reactive({
    paste0("_", formatTopicname(topic_page),"_prevalence")
  })
  
  meanFileName <- reactive({
    paste0("_", formatTopicname(topic_page),"_mean")
  })
  
  # Handle prevalence table data download
  output$DLTableCSV <- downloadCsv(function(){prevFileName()},
                                   c(function(){paste("Source: Health and Lifestyles Survey")},
                                     function(){subtopicDesc()},
                                     function(){paste0("Subgroup: ", input$groupTbl)},  
                                     function(){paste0("Subtopic: ", subtopic_page)},  
                                     function(){paste0("Topic: ", topic_page)}),
                                   function(){prevCSV()})
  
  # Handle mean table data download
  output$DLTableMeanCSV <- downloadCsv(function(){meanFileName()},
                                       c(function(){paste("Source: Health and Lifestyles Survey")},
                                         function(){subtopicDesc()},
                                         function(){paste0("Subgroup: ", input$groupTbl)},  
                                         function(){paste0("Subtopic: ", subtopic_page)},  
                                         function(){paste0("Topic: ", topic_page)}),  
                                       function(){meanCSV()})
  
  # Renders the Group Select dropdown to filter indicator timeseries results by Ethnicity
  output$groupSelect <- renderUI({
    req(topic_page)
    if(!grepl("ori cultural identity",tolower(topic_page)))
    {
      div(class="orange-select", selectInput(ns("groupTbl"), "Show:", choices = getGroupChoices(), selectize = F))
    }else{
      # For Maori Cultural Identity don't display the dropdown but still have it in the DOM
      div(class="orange-select", style="display: none", selectInput(ns("groupTbl"), "Show:", choices = getGroupChoices(), selectize = F))
    }
  })
  
  # Renders the Means datatable
  output$meansTable <- renderUI({
    
    req(meanTableData())
    req(nrow(meanTableData())>0)
    tagList(
      fluidRow(
        div(class="col-sm-12 col-md-6",
            div(class="chart-title", "Mean for selected subtopic"),
            p(class="chart-subtitle", "This table gives the average value (mean) of the specified population.")
        )
      ),
      fluidRow(
        div(style="margin-bottom: 40px",
            dataTableOutput(ns("meansTableDT"))
        )
      )
    )
  })
  
  # Creates the Means datatable
  output$meansTableDT <- renderDataTable({
    tableHTML("meansTableDT", meanTableData(), "")
  })
  
  # Render Subtopic title and description
  output$subtopicDescription <- renderUI({
    req(subtopic_page)
    HTML(
      paste0(
        "<h1 class='subtopic-title'>", subtopic_page, "</h1>",
        "<p>", subtopicDesc(), "</p>"
      )
    )
  })
  
  # Renders the Prevalence datatable
  output$table <- renderDataTable({
    
    validate(need(nrow(prevTableData()) > 0, "No prevalence for selected subtopic available."))
    # Is this where the layout is set?
    tableHTML("table",prevTableData(), "%")
  })
  
  # Render Topic title and Icon
  output$topicHeading <- renderUI({
    topicWithoutMacron <- topic_page %>% str_replace_all(paste0("M", intToUtf8(0x0101L), "ori"), "Maori")
    HTML(
      paste0(
        "<img class='topic-heading-icon' src='img/icons-mono/2.0-RS037-Kupe-icons-", 
        paste(toupper(substr(gsub(" ", "-", topicWithoutMacron), 1, 1)), tolower(substr(gsub(" ", "-", topicWithoutMacron), 2, nchar(gsub(" ", "-", topicWithoutMacron)))), sep="") ,
        ".svg'/><div class='topic-title'>", 
        paste0(substr(topic_page, 1, 1), substr(tolower(topic_page), 2, nchar(topic_page))) %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori")), "</div>"
      )
    )
  })
  
}