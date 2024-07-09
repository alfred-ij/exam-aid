#libraries and other sources
library(shiny)
library(shinydashboard)
#header...long-term actuarial modeling...custom width
custom_width <- 325

header <- dashboardHeader(
  title = tagList(
    icon("gears",class = "pull-right"),
    "Long-Term Actuarial Modeling"
  ),
  titleWidth = custom_width #match custom width in sidebar
)
#sidebar...prob || product || profit...icon defined sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('§1: Overview', icon = icon('list',class = "pull-right"), tabName = 'overview'),
    menuItem("§2: Probability Distributions", icon = icon("chart-line",class = "pull-right"), tabName = "prob"),
    menuItem("§3: Life-Contingent Products", icon = icon("cart-shopping",class = "pull-right"), tabName = "product"),
    menuItem('§4: Policy Values', icon = icon('money-bill-trend-up',class = "pull-right"), tabName = 'profit'),
    menuItem('§5: Notes', icon = icon('note-sticky',class = "pull-right"), tabName = 'notes')
  ),
  width = custom_width #custom width matched with header
)
#body...prob || product || profit...tab defined content
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview",
            h2(
              class = "centralized-h2",
              tagList(
                "Overview",
                icon("list",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    ),
    tabItem(tabName = "prob",
            fluidPage(
              tags$head(
                tags$style(HTML("
                                /* Centralize tabsetPanel tabs */
                                .nav-tabs {
                                    display: flex;
                                    justify-content: center;
                                }
                                .nav-tabs .nav-item {
                                    float: none;
                                }
                                /* Style for dashboardSidebar text */
                                .sidebar-menu li a {
                                    font-size: 16px;
                                    color: #ffffff;
                                }
                                .sidebar-menu li a:hover {
                                    background-color: #444444;
                                }
                                .centralized-h2 {
                                    text-align: center;
                                }
                                "))
              ),
              h2(
                class = "centralized-h2",
                tagList(
                  "Probability Distributions",
                  icon("chart-line",class = "pull-center")
                ),
                class = "heading-with-icon"
              ),
              tabsetPanel(
                tabPanel('§1: overview',
                         'probability distributions intro'
                ),
                tabPanel('§2: force of mortality',
                           sidebarPanel(
                             sliderInput("age", "life age (x)", 1, 130, 65)
                           )
                ),
                tabPanel('§3: curtate future life-time',
                         sidebarPanel(
                           sliderInput("age", "life age (x)", 1, 130, 65)
                         )
                ),
                tabPanel('§4: life tables and selection',
                         sidebarPanel(
                           sliderInput("age", "life age (x)", 1, 130, 65)
                         )
                ),
                tabPanel('§5: notes',
                         'probability distributions notes'
                )
              )
            )
    ),
    
    tabItem(tabName = "product",
            h2(
              class = "centralized-h2",
              tagList(
                "Life-Contingent Products",
                icon("cart-shopping",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    ),
    tabItem(tabName = "profit",
            h2(
              class = "centralized-h2",
              tagList(
                "Policy Values",
                icon("money-bill-trend-up",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    ),
    tabItem(tabName = "notes",
            h2(
              class = "centralized-h2",
              tagList(
                "Notes",
                icon("note-sticky",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    )
  )
)
#ui...header || sidebar || body...accumulate dashboard components
ui <- dashboardPage(
    header,
    sidebar,
    body
)

#server...input || output...accumulate server objects
server <- function(input, output) { }

#launch app
shinyApp(ui, server)