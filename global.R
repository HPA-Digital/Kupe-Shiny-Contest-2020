source("source/util/expandingContent.R")
source("source/util/lightbox.R")

source("source/util/pageUtil.R")
source("source/util/downloadHelpers.R")

source("source/pages/exploreIndicators.R")
source("source/pages/exploreIndicators_server.R")

source("source/pages/download.R")
source("source/pages/download_server.R")

source("source/pages/exploreTopics.R")
source("source/pages/exploreTopics_server.R")

source("source/pages/methodology.R")
source("source/pages/methodology_server.R")

source("source/pages/subtopicsMenu.R")
source("source/pages/subtopicsMenu_server.R")

source("source/pages/help_server.R")

source("source/pages/splash.R")
source("source/pages/splash_server.R")

library(RCurl)
library(shiny.router)

# =========================================== APP DATA ===========================================

# Topic and Subtopic card descriptions
G_topicDescriptions <- read_xlsx("data/overviews/topic/topic_appdescriptions.xlsx")
G_topicDescriptions <- G_topicDescriptions %>% arrange(G_topicDescriptions$topic)
G_subtopicDescriptions <- read_xlsx("data/overviews/topic/subtopic_appdescriptions.xlsx")
G_indicatorDescriptions <- read_xlsx("data/overviews/indicator/indicator_appdescriptions.xlsx")

# Prevalence data
G_prevalences <- readRDS("data/prevalences.Rds")
G_prevalenceSubgroupList <- readRDS("data/prevalence_subgroup_list.Rds")

# Comparisons data
G_RateRatios <- readRDS("data/comparisons.Rds")
G_ComparisonSubgroupList <- readRDS("data/comparison_subgroup_list.Rds")

# Timeseries data
G_timeseries <- readRDS("data/timeseries.Rds")
G_timeseriesOld <- readRDS("data/old/timeseries_old.Rds")
G_timeseriesSubgroupList <- readRDS("data/timeseries_subgroup_list.Rds")

# All Inidicators - also format Indicators with macron for Maori
G_indicatorList <- readRDS("data/indicator_list.Rds") 
G_indicatorList$subtopic <-  G_indicatorList$subtopic %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori"))
G_indicatorList$short.description <-  G_indicatorList$short.description %>% str_replace_all( "Maori", paste0("M", intToUtf8(0x0101L), "ori"))
G_indicatorList$short.description <-  paste(toupper(substr(G_indicatorList$short.description, 1, 1)), substr(G_indicatorList$short.description, 2, nchar(G_indicatorList$short.description)), sep="") 

# Extra data for filtering dropdowns
G_yearsList <- readRDS("data/years_list.Rds")
G_ethnicities <- list("Total"="Total", "M\u0101ori"="Maori", "Pacific"="Pacific", "Asian"="Asian", "European/Other"="European/Other")


# Formatting funcions
format.CI <- function(low.CI, high.CI, ...) {
  ifelse(is.na(low.CI) | is.na(high.CI),
         NA,
         paste("(", format(low.CI, ...), "-", format(high.CI, ...), ")"))
}

G_current.year <- G_yearsList %>%
  filter(current.year == 1) %>%
  as.list()

G_first.year <- G_yearsList %>%
  filter(show.in.time.trends == 1) %>%
  arrange(year) %>%
  slice(1) %>%
  as.list()


# Get Icon for summary
getTopicIcon <- function(topic){
  if(is.null(topic) || topic =="") return(NULL)

  paste0("img/icons-color/4.1-RS037-Kupe-icons-", 
         paste(toupper(substr(gsub(" ", "-", topic), 1, 1)), tolower(substr(gsub(" ", "-", topic), 2, nchar(gsub(" ", "-", topic)))), sep="") , 
         ".svg")
}


# Cleans topic, subtopic, and indicator names by removing and replacing certain elements web addresses
convert2Query <- function(input){
  #curlEscape(gsub(' (\\w?)', '\\U\\1', tolower(input), perl=T))
  ws <- gsub(" ", "-", tolower(input)) # replace whitespace
  brackets <- gsub("\\(|\\)|\\'|\\’|\\‘", "", ws) # remove brackets ()
  as <- gsub("\u0101ori", "aori", brackets) # replace maori
  slashes <- gsub("\\/|\\+|\\$", "-", as) # replace /, +, and $
  dashes <- gsub("-(-|–)-", "-", slashes) # replace --- to -
  dashes <- gsub("--", "-", dashes) # replace -- to - 
  curlEscape(dashes) %>% trimws(whitespace = "-") # remove begining and trailing -
}

# wrapper for route_link applying convert2Query before 
route_path <- function(...) {
  items <- lapply(list(...), convert2Query)
  route_link(paste(items, collapse = "/"))
}


# Generate all ui webpages 
generatePages <- function() {
  pages <- list(
    splash=pageSplashUI("splash"),
    methodology=pageMethodologyUI("methodology"),
    subtopicsMenu=pageSubtopicsMenuUI("subtopicsMenu")
  )
  
  
  subtopics_pages <- map(G_subtopicDescriptions$subtopic, 
                         ~ {
                           ns <- convert2Query(.x)
                           print(ns)
                           pageExploreTopicsUI(ns) 
                         })
  names(subtopics_pages) <- convert2Query(G_subtopicDescriptions$subtopic)
  
  indicator_pages <- map(G_indicatorList$indicator,
                         ~{
                           ns <- convert2Query(.x)
                           print(ns)
                           pageExploreIndicatorsUI(ns)
                         })
  
  names(indicator_pages) <- convert2Query(G_indicatorList$indicator)
  
  c(pages, subtopics_pages, indicator_pages)
}

default_pages <- generatePages()

# =========================================== LIGHTBOX DATA ===========================================

# Privacy Lightbox
G_privacyLightbox <- lightboxBody("privacy-modal",
                                  div(class="container-fluid", style="width:90%",
                                      div(class="modal-section",
                                          p(
                                            tags$h3("Privacy Policy"),
                                            p('For you to get the most out of using kupe.hpa.org.nz, the Health Promotion Agency ("we" or "us") 
                                              needs to collect some personal information about you. We will protect your personal information, 
                                              and will only collect, use, store, and share it in accordance with the Privacy Act 1993, including 
                                              (where applicable) the Health Information Privacy Code 1994.'),
                                            p('This Privacy Policy sets out information about:'),
                                            tags$ul(
                                              tags$li("what information is collected by us through your use of kupe.hpa.org.nz;"),
                                              tags$li("how your information will be used, stored, and shared;"),
                                              tags$li("how you can access and change your personal information; and"),
                                              tags$li("for how long we will hold your personal information.")
                                            ),
                                            p("In summary:"),
                                            tags$ul(
                                              tags$li("we collect personal information about you, including your email address and use it to 
                                                      provide you with the services you have asked us for;"),
                                              tags$li("we share your information with our agents who help us provide kupe.hpa.org.nz to you; and"),
                                              tags$li("we store your information securely and will protect it from unauthorised access, use, modification, or disclosure;")
                                            ),
                                            p("This Privacy Policy forms part of the terms of use of kupe.hpa.org.nz. Your use of kupe.hpa.org.nz means that you accept 
                                              and agree to the terms of this Privacy Policy and any changes to it. "),
                                            p("We may change this Privacy Policy from time to time, including if we want to collect more information about you or use or 
                                              share your information in another way. This Privacy Policy was last updated in November 2019."),
                                            p("If you have any questions about this policy, please contact us:"), 
                                            tags$a(href="mailto:kupe@hpa.org.nz", "kupe@hpa.org.nz")
                                            )),
                                      div(class="modal-section",
                                          p(
                                            tags$h4("What information do we collect?"),
                                            p(tags$em("Personal information about you")),
                                            p("We collect your email address and any information you share in the emails you send to ", tags$i("kupe.hpa.org.nz.")),
                                            p(tags$em("Aggregated, anonymous information about visitors to kupe.hpa.org.nz")),
                                            p("We collect anonymous and aggregated information about you and all other visitors to kupe.hpa.org.nz, including:"),
                                            tags$ul(
                                              tags$li("website usage statistics")
                                            )
                                            )
                                      ),
                                      div(class="modal-section",
                                          p(
                                            tags$h4("How do we use your information?"),
                                            p("The personal information that we collect from you via kupe.hpa.org.nz will be used to: "),
                                            tags$ul(
                                              tags$li("provide you with the services you have asked us for.")
                                            ),
                                            p(
                                              'We may use anonymous and aggregated information that we collect about visitors to kupe.hpa.org.nz so that we can analyse, 
                                              evaluate, and improve kupe.hpa.org.nz. For more information, see below under the heading "Use of cookies and tracking technologies".'
                                            )
                                          )
                                      ),
                                      div(class="modal-section",
                                          p(
                                            tags$h4("Will my information be shared with anyone else?"),
                                            p("We may share the personal information that we collect about you as follows:"),
                                            tags$ul(
                                              tags$li("we need to give some employees, contractors, agents and advisers access to the information you provide us to ensure 
                                                      that kupe.hpa.org.nz works properly. This includes our providers that host or maintain data centres where your information is 
                                                      processed or stored, or that operate service platforms and other infrastructure and systems on our behalf; and"),
                                              tags$li("as otherwise permitted by the Privacy Act 1993, and (where applicable) the Health Information Privacy Code 1994. ")
                                            ),
                                            p(
                                              'We may share anonymous and aggregated information about visitors to kupe.hpa.org.nz with public sector 
                                              health agencies and other organisations and individuals that have an interest in this data. '
                                            )
                                          )
                                      ),
                                      
                                      div(class="modal-section",
                                          p(
                                            tags$h4("How is my information stored?"),
                                            p("We take your privacy seriously, and will take all reasonable steps to ensure that your personal information is stored in a 
                                              secure environment and protected from unauthorised access, use, modification, or disclosure."),
                                            p("We store our data (including your personal information) on a secure Microsoft Azure cloud platform and we use Microsoft Office 365 applications. 
                                              We protect our data with all reasonable technical and process controls.")
                                          )
                                      ),
                                      
                                      div(class="modal-section",
                                          p(
                                            tags$h4("How do I access and change my information?"),
                                            p("You can ask us to provide you with the information we hold about you and request corrections to it."),
                                            p("If you would like to see the information we hold about you or request a correction, please contact us at ", tags$a(href="mailto:kupe@hpa.org.nz", "kupe@hpa.org.nz"))
                                          )
                                      ),
                                      
                                      div(class="modal-section",
                                          p(
                                            tags$h4("Use of cookies and tracking technologies"),
                                            p('kupe.hpa.org.nz collects statistical information which is used for making the site faster and easier to use.  Information from your visit is combined with the data from all site visitors and presented as statistical information on overall site use.  To do this, kupe.hpa.org.nz uses "cookies" and other tracking technologies.'),
                                            p('A "cookie" is a file that our webserver may send to your computer when you access this website.  This file is then stored on your computer. kupe.hpa.org.nz may use cookies and certain other passive information collection technologies to make your use of this website easier by allowing kupe.hpa.org.nz to provide better service, customise websites based on consumer preferences, compile statistics, analyse trends, and otherwise administer and improve this website. Certain features of kupe.hpa.org.nz may not work without use of passive information collection technologies.'),
                                            p('kupe.hpa.org.nz uses "session" cookies.  Session cookies are temporary bits of information that are erased once you exit your web browser window or otherwise turn your computer off.  Session cookies are used, for example, to improve navigation on this site, and to collect aggregated statistical information.'),
                                            p('kupe.hpa.org.nz may use "persistent" cookies:  Persistent cookies are more permanent bits of information that are placed on the hard drive of your computer and stay there unless you delete the cookie.  Persistent cookies store information on your computer for a number of purposes, such as helping to determine what areas of the website visitors find most valuable, and customising the website based on your preferences on an ongoing basis. The kupe.hpa.org.nz server only knows that an unidentified visitor with your cookies has returned to the website and not any personally identifiable information.'),
                                            p('kupe.hpa.org.nz may use "web beacons" (also known as internet tags, single-pixel GIFs, clear GIFs, anonymous identifiers and invisible GIFs): A web beacon is a tiny graphic on a web page or in an email message that is used to track pages viewed or messages opened.  Web beacons tell the website server information such as the IP address and browser type related to the visitor’s computer.  Web beacons may be placed on online advertisements that bring people to kupe.hpa.org.nz and on different pages of the website.  Web beacons provide us with information on how many times a page is opened and which information is consulted.'),
                                            p('kupe.hpa.org.nz uses both First Party and Third Party web beacons and cookies.  Third Party web beacons and cookies are used for the purpose of marketing this website to people that may find benefit from the services included on the website.')
                                          )
                                      ),
                                      
                                      div(class="modal-section",
                                          p(
                                            tags$h4("Other tracking technologies"),
                                            p("The following Google Analytics Display Advertising Features are enabled for this website:"),
                                            tags$ul(
                                              tags$li("Remarketing with Google Analytics"),
                                              tags$li("Google Display Network Impression reporting"),
                                              tags$li("Google Analytics Demographics and Interest Reporting"),
                                              tags$li("DoubleClick Platform integrations."),
                                              tags$li("The following Adwords Features are enabled for this website:"),
                                              tags$li("Remarketing"),
                                              tags$li("Affinity audiences"),
                                              tags$li("Custom affinity audiences"),
                                              tags$li("In-market audiences"),
                                              tags$li("Similar audiences"),
                                              tags$li("Demographic and location targeting.")
                                            ),
                                            p("This means that kupe.hpa.org.nz is enabling Google Analytics to collect data about traffic to this site through cookies and anonymous identifiers in addition to data collected through standard Google Analytics implementations."),
                                            p("kupe.hpa.org.nz will not share precise location information with Google without your prior explicit consent.  ")
                                          )
                                      ),
                                      
                                      div(class="modal-section",
                                          p(
                                            tags$h4("Choices you have about kupe.hpa.org.nz’s use of cookies and other tracking technologies"),
                                            p("You can choose to enable or disable the use of some cookies and other tracking technologies on this website.  Cookies that are necessary to the operation of this website and for the provision of the services you have requested will continue to function regardless of whether you have provided consent.  Other types of cookies and tracking technologies will be turned off until you provide explicit consent.  As a result, the functionality of kupe.hpa.org.nz may be reduced until you choose to provide explicit consent."),
                                            p('Some Internet browsers allow you to limit or disable the use of cookies and other tracking technologies.  Please refer to the information provided by your Internet browser for instructions on how to do so (generally found under a "Help" menu).'),
                                            p('You can opt-out of any of the Google Analytics Advertising Features by going to ', tags$a(href='https://tools.google.com/dlpage/gaoptout/', 'https://tools.google.com/dlpage/gaoptout/'), 'which is where users can download the Google Analytics Opt Out Browser Add-On.'),
                                            p('You can also opt out by going to Google Ad Settings, which allows users to turn off the amount and type of data you are sharing.'),
                                            p('More information about opting out can be obtained from ', tags$a(href='http://www.networkadvertising.org/choices/', 'http://www.networkadvertising.org/choices/.'))
                                          )
                                      )
                                      
                                     
                                    )
                           )

# Terms of use Lightbox
G_termsLightbox <- lightboxBody("terms-modal",
                                div(class="container-fluid", style="width:90%",
                                    div(class="modal-section",
                                        tags$h3("COPYRIGHT"),
                                        p("Any Health Promotion Agency (HPA) material on this website may be reproduced provided that:"),
                                        tags$ul(
                                          tags$li(
                                            "the content is not changed"
                                          ),
                                          tags$li(
                                            "it is not sold"
                                          ),
                                          tags$li(
                                            "the material is not used to promote or endorse any product or service"
                                          ),
                                          tags$li(
                                            "it is not used in an inappropriate or misleading context"
                                          ),
                                          tags$li(
                                            "any disclaimers included on the published information are reproduced on the material"
                                          ),
                                          tags$li(
                                            "an acknowledgment of the source and of HPA is included."
                                          )
                                        ),
                                        p(
                                          "Please note, this does not apply to any logos, 
                                          emblems and trade marks on the website, the website’s design elements, 
                                          or any photography or imagery. If you wish to reproduce any of these, 
                                          please contact us to discuss: ",
                                          tags$a(href="mailto:communications@hpa.org.nz", "communications@hpa.org.nz")
                                        )
                                    )
                                    )
                                    )

# Contact Lightbox
G_contactLightbox <- lightboxBody("contact-modal",
                                  div(class="container-fluid", style="width:90%",
                                      div(class="modal-section",
                                          tags$h3("INFORMATION ENQUIRIES"),
                                          p("Email: ",
                                            tags$a(href="mailto:kupe@hpa.org.nz", "kupe@hpa.org.nz")
                                          ),
                                          p(
                                            HTML(
                                              "Health Promotion Agency | Te Hiringa Hauora<br/>
                                              Level 16<br/>
                                              101 The Terrace<br/>
                                              Wellington 6011
                                              "
                                            )
                                            ),
                                          p(
                                            HTML(
                                              "PO Box 2142<br/>
                                               Wellington 6140<br/>
                                               Phone: <a href='tel:049170060'>(04) 917 0060 </a>
                                               "
                                            )
                                          )
                                            ),
                                      div(class="modal-section",
                                          p(
                                            tags$h3("WEBSITE FEEDBACK"),
                                            p("We welcome your comments or suggestions about Kupe."),
                                            p("Please email us: ", tags$a(href="mailto:kupe@hpa.org.nz", "kupe@hpa.org.nz"))
                                          )
                                      ),
                                      div(class="modal-section",
                                          p(
                                            tags$h3("MEDIA ENQUIRIES"),
                                            p("Please direct all media enquiries to:"),
                                            p(tags$a(href="mailto:communications@hpa.org.nz", "communications@hpa.org.nz"))
                                          )
                                      )
                                      )
)

# Feedback Lightbox
G_feedbackLightbox <- lightboxBody("feedback-modal",
                                   div(class="container-fluid", style="width:90%",
                                       tagList(
                                         div(class="modal-section",
                                             p("We welcome your comments or suggestions about Kupe. Please email us:", 
                                              tags$a(href="mailto:kupe@hpa.org.nz", "kupe@hpa.org.nz")
                                             )
                                         )
                                       )
                                   ), "Feedback"
)

# About Lightbox
G_aboutLightbox <- lightboxBody("about-modal", 
  div(class="container-fluid about-content", style="width:90%",
  tagList(
    div(class="modal-section",
        p("We welcome your comments or suggestions about Kupe. Please email us: ", br(),
        tags$a(href="mailto:kupe@hpa.org.nz", "kupe@hpa.org.nz")
        )
    ),
    div(class="modal-section", 
        h3("THE SURVEY"),
        p("Health and Lifestyles Survey (HLS) data was first collected in 2008 by the Health Sponsorship Council (HSC). 
          Before 2008 the HSC had a range of standalone surveys including Smokefree/Auahi Kore Monitor, Gaming and 
          Betting Activities Survey and the Sun Protection Triennial Survey. In 2008, these surveys were combined into one survey - 
          the HLS."),
        p("Face-to-face interviews for the HLS have been completed every two years since 2008."
        ),
        p("The main target populations are adults aged 15 years and over, 
          and parents and caregivers of 5 to 16-year-old children living in permanent private 
          dwellings in New Zealand. Each survey year monitors changes in attitudes, knowledge and behaviours, 
          and tracks changes in views about the social desirability and acceptability of various measures. 
          The HLS includes questions about gambling, tobacco, alcohol, sun exposure, nutrition, mental health, and 
          physical activity. Sections and questions vary across the survey years. "
        )
    ),
    div(class="modal-section", 
        h3("KUPE - THE DATA EXPLORER "),
        p("Kupe presents results from the most recent survey, as well as changes over time for adults aged 15 years and over. Kupe aims to answer the following questions for each reported indicator: "
        ),
        tags$ol(
          tags$li(
            "What is the prevalence (percentage) and estimated number of people affected, or what is the average (mean) for the total population and population subgroups?"
          ),
          tags$li(
            "How do the indicators vary by sex, age, ethnicity, or neighbourhood deprivation? Ratios are used to compare males 
            with females, M\u0101ori with non-M\u0101ori, Pacific with non-Pacific, and people living in most deprived versus least deprived 
            areas. Ratios are adjusted for differences in age, sex, and prioritised ethnicity."
          ),
          tags$li(
            "What has changed over time? Where possible, we present results for all available years of the HLS to identify significant changes over time."
          )
        )
    ),
    div(class="modal-section", 
        h3("ACKNOWLEDGEMENTS"),
        p("Kupe was developed by EPI-interactive and the Health Promotion Agency | Te Hiringa Hauora (HPA). 
          HPA appreciates precedents shared from the Annual Data Explorer, which was co-developed by 
          EPI-interactive and the Health and Disability Intelligence Unit at the Ministry of Health."
        ),
        p("Data used in Kupe would not have been available without the support and enthusiasm of many individuals, 
          including the people who gave their valuable time to participate in the HLS and the interviewers collecting the data."
        )
    ),
    div(class="modal-section",
        h3("IMAGES"),
        p(
          "The landing page image has been compiled from: Māori Star Compass 
          (courtesy of Te Ara: The Encyclopaedia of New Zealand); 
          “Milky Way Wellington. Leica M9” by ", 
          tags$a(href="https://www.flickr.com/photos/andrew_xjy/", target="_blank","Andrew Xu"), 
          " is licensed under ", 
          tags$a(href="https://creativecommons.org/licenses/by/2.0/", target="_blank", style="margin-right: -3px;", "CC BY-SA 2.0"), "; 
          and ",
          tags$a(href="https://collections.tepapa.govt.nz/object/240379", target="_blank", "Illustration of stick chart, Marshall Islands", style="margin-right: -3px;"), ", c1920, by Ethel Richardson. 
          Te Papa (MU000049/008/0003)."
        )
    ),
    div(class="modal-section",
        h3("CITATIONS"),
        p("Example: Health Promotion Agency (HPA) 2018. Kupe 2018: Health and Lifestyles Survey [Data File]. URL: https://kupe.hpa.org.nz/hls-2018/ (Accessed 10 December 2018).")
    ),
    div(class="modal-section",
        h3("COPYRIGHT"),
        p("This work is licensed under a Creative Commons Attribution 4.0 International License. For more information view the",
          lightboxBtn("terms-modal",
            tags$button(class="about-lb-button", onClick="$('#about-modal').modal('toggle');", "HPA's copyright statement", style="margin-right: -3px;")
        ), ".")

    ),
    div(class="modal-section",
        h3("MICRODATA"),
        p("HPA datasets are available for statistical purposes to researchers working within academic institutions, 
          government agencies and the wider health sector, subject to certain conditions. Researchers can apply to access 
          microdata after HPA has released key survey results. For more details please visit ",
          tags$a(href="https://www.hpa.org.nz/our-work/research/accessing-microdata",target="_blank", "Accessing HPA's Microdata for Research purposes", style="margin-right: -3px;")
        ,".")
    )
  )
  ), "About"
)


