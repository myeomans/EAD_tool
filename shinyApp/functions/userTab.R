userTab<-function(){
  tabItem(tabName = "users",
          h2("User Information"),
          fluidRow(
            box(
              height = "300px",
              width = 12,
              h3("Descriptive statistics of forum participants"),
              htmlOutput("descriptive_table")
            )),
          fluidRow(
            box(
              width = 8,
              height = "350px",
              tabPanel("Distribution of Interest", plotOutput("distribution_plot", height = 250)),
              checkboxInput('forum.active', 'Forum Participants Only', value=FALSE)
            ),
            box(
              width = 4,
              h3("Sample Options"),
              selectInput("of.interest", "Difference of Interest", 
                          list(TriPartite="TriPart",
                               WVS_survey="wvs_ideology",
                               gender="gender",
                               age="age"), 
                          selected = "leftright"),
              checkboxInput('usa.only', 'American Students Only', value=TRUE),
              checkboxInput('course.only', 'Exclude Student-Created Threads', value=TRUE),
              checkboxInput('self.posts', 'Include Comments to Own Posts', value=FALSE)
              #checkboxInput('short.yes', 'Exclude \"Short Yes\" Posts', value=TRUE)
            )
          ))
}