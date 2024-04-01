# library(shinydashboard)
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Contingent Calculator"),
#   dashboardSidebar(sidebarMenu(
#     menuItem("Mortality Assumptions", tabName = "assumptions", icon = icon("chart-line")),
#     menuItem("Contingent Products", tabName = "products", icon = icon("cart-shopping")),
#     menuItem("Contingent Profit", tabName = "profit", icon = icon("money-bill-trend-up"))
#   )
#   ),
#   dashboardBody(tabItems(#dashboard tabs...mortality assumptions|contingent products|contingent profit
#     #mortality assumption content
#     tabItem(tabName = "assumptions",
#             h2("mortality content")
#     ),
#     
#     #contingent products content
#     tabItem(tabName = "products",
#             h2("contingent products")
#     ),
#     #profit content
#     tabItem(tabName = "profit",
#             h2("contingent profit")
#     )
#   ))
# )
# 
# 
# server <- function(input, output) {
#   set.seed(122)
#   histdata <- rnorm(500)
#   
#   output$plot1 <- renderPlot({
#     data <- histdata[seq_len(input$slider)]
#     hist(data)
#   })
# }
# 
# shinyApp(ui, server)

library(shiny)
library(DT)

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      h3("Dataframe with LaTeX Column Names"),
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # Define dataframe with LaTeX column names
  df <- data.frame(
    "$\\alpha$" = 1:5,
    "$\\beta$" = 6:10
  )
  
  # Convert LaTeX column names to HTML
  colnames_html <- lapply(names(df), function(col) gsub("\\$", "", col))
  
  # Set the column names of the dataframe to HTML representations
  colnames(df) <- colnames_html
  
  # Render dataframe with LaTeX column names
  output$table <- renderDataTable({
    datatable(df, options = list(dom = 't'))
  })
}

shinyApp(ui, server)








    





    
  












      



        
      
    
  





    
  

