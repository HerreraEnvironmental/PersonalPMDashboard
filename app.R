#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
#add required packages below
###
library(signal)
library(dplyr)
library(rhandsontable)
library(ggplot2)
library(DT)

example = read.csv("example/example.csv", skip = 2)

APP_TITLE<-'Personal Project Management Tool'
showList = ""
#Define UI
ui<-fluidPage(
#   HTML(paste0('<body class="post-template-default single single-post postid-9227 single-format-standard',
#   'wp-custom-logo fl-builder group-blog single-projects single-stormwater single-sustainability',
#   'single-uncategorized single-washington single-water-quality fl-builder-breakpoint-large">')),
#   #header file for CSS and analytics
    includeHTML('HTML_Helpers/herrera_head.html'),#note that I had to delete the JS scripts and Google analytics
#   #herrera nav bar
    includeHTML('HTML_Helpers/herrera_navbar.Rhtml'),
#   #INSERT APPLICATION TITLE
    HTML(paste0('
               <div class="bg-new-blue std-head">
               <h2 class="px-md-5 text-white"></h2>
               <h2 class="page-title text-white">',APP_TITLE,'</h2>
               </div>
    ')),
        ###APP UI STARTS HERE
    fluidRow(
      column(10,
      offset = 1,
      div(HTML("Welcome to the Personal Project Management Tool (PPMT). This tool is designed to help track project budgets for any projects in Vantagepoint. To get started, please upload a <i> Budget Remaining </i> CSV file.")),
             br()
         )),
    fluidRow(
      sidebarLayout(
        column(3,
        offset = 1,
          sidebarPanel(
            h5('How to Start'),
            div(HTML("Upload the CSV exported from Vantagepoint in the <i> Budget Remaining </i> report.")),
            br(),
            fileInput("VP_IN", "Vantagepoint File Upload", accept = ".csv"),
            selectInput("proj_names", "Select Project to View", label = "Select Project", choices = showList),
            width = 12
          )),
        column(7,
          offset = 1,
          mainPanel(
            h5('My Projects'),
            div(HTML("Once a Vantagepoint file is uploaded, and a project is selected, a table will generate showing the budget columns.")),
            hr(),
            DTOutput("tables"),
            width = 10
            )
          )
        )
      )
  #includeHTML('HTML_Helpers/herrera_footer.Rhtml')
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  my_VP_decifer <- function(VP_INPUT){
    projs       <- VP_INPUT
    projs_list  <- unique(projs$groupHeader1_GroupColumn)
    projs_nums  <- substr(projs_list, 10, 21)
    projs_name  <- substr(projs_list, 23, nchar(projs_list))
    return(list(projs_name, projs_nums))
  }

  df <- reactive({
    file <- input$VP_IN
    ext <- tools::file_ext(file$datapath)
    if (!is.null(input$VP_IN)) {
      df <- example
    } else{
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    df <- read.csv(file$datapath, skip = 2)
    }
    df
  })


  searchResults <- reactive({
    projs_info <- my_VP_decifer(df())
    projs_name <- projs_info[1]
    projs_nums <- projs_info[2]
    unlist(projs_name)
  })

  observe({
    updateSelectInput(session, "proj_names", label = "Select Project to View", choices = searchResults())
  })

  # Insert the right number of plot output objects into the web page
  output$tables <- renderDataTable({
    my_df <- df()[grepl(input$proj_names, df()$groupHeader1_GroupColumn), c(10:11,15:17)]
    colnames(my_df) <- c("Task Description", "Compensation", " JTD Billed", "Remaining Billed", "% Billed")
    datatable(my_df,
              selection = "none",
              options = list(dom = "t", autoWidth = TRUE,
                             buttons = c('copy', 'csv', 'excel', 'print')),
              rownames= FALSE
              )
  })


}

# Run the application
shinyApp(ui = ui, server = server)
