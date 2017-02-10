introTab<-function(){
  tabItem(tabName = "instructions",
          h2("Engagement Across Differences"),
          p("Upload forum data to analyse interaction patterns among forum participants."),
          fluidRow(
            box(height = "650px",
                width = 5,
                h2("Upload Datasets"),
                p("CSV files only"),
                fileInputCsv('survey', 'Survey data'),
                fileInputCsv('posts', 'Post data'),
                fileInputCsv('upvotes', 'Upvote data'),
                shiny::tags$hr(),
                h4("Optional (all enrollments)"),
                fileInputCsv('personcourse', 'Person-Course data'),
                fileInputCsv('enrol', 'Enrollment data')
            ),
            box(height = "650px",
                width = 7,
                h2("Forum Settings"),
                textInput('course.name', 'Forum Name', value="American Government"),
                dateInput('start.date', 'Forum Launch Date', value="2015-09-01"),
                textInput('admin.names', 'Admin User Names', value="RachelRueckert1,zacharydavis"),
                p("Separate with commas (e.g. \"Matthew, Mark, Luke, John\")"),
                h3("Filters for Analysis"),
                checkboxInput('course.only', 'Exclude Student-Created Threads', value=TRUE),
                checkboxInput('self.posts', 'Exclude Comments to Own Posts', value=TRUE)
            )
          )
  )
}