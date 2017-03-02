userTab <- function(){
  tabItem(tabName = "users",
          h2("User Information"),
          fluidRow(
            box(
              height = "300px",
              width = 12,
              h3("Descriptive statistics of forum participants"),
              htmlOutput("descriptive_table"),
              actionButton("descTableHelp", "",icon = icon("question")),
              bsModal(id = "descTableHelpModal", 
                      "Help", 
                      trigger = "descTableHelp", 
                      size = "small",
                      p("Text about table")
              )
            ),
              tags$style(type='text/css', "#descTableHelp {position:absolute;right:0;bottom:0;}")
            ),
          fluidRow(
            box(
              width = 10,
              height = "400px",
              selectInput("of.interest", "Difference of Interest", 
                          list(TriPartite="TriPart",
                               WVS_survey="wvs_ideology",
                               gender="gender",
                               age="age"), 
                          selected = "leftright"),
              checkboxInput('forum.active', 'Forum Participants Only', value=FALSE),
              tabPanel("Distribution of Interest", plotOutput("distribution_plot", height = 250)),
              actionButton("distPlotHelp", "",icon = icon("question")),
              bsModal(id = "distPlotHelpModal", 
                      "Help", 
                      trigger = "distPlotHelp", 
                      size = "small",
                      p("Text about distribution plot")
              )
            ),
            tags$style(type='text/css', "#distPlotHelp {position:absolute;right:0;bottom:0;}"),
            tags$style(type='text/css', "#ofInterestHelp {position:absolute;right:0;bottom:0;}")
          ))
}