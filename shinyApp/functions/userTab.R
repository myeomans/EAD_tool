userTab<-function(){
  tabItem(tabName = "users",
          h2("User Information"),
          fluidRow(
            box(
              height = "300px",
              width = 12,
              h3("Descriptive statistics for sub-populations of forum participants"),
              htmlOutput("descriptive_table"),
              checkboxInput('usa.only', 'American Students Only', value=TRUE)
            )),
          fluidRow(
            box(
              width = 8,
              height = "350px",
              selectInput("of.interest", "Distribution of Interest", 
                          list("leftright","gender","age"), 
                          selected = "leftright"),
              tabPanel("Distribution of Interest", plotOutput("distribution_plot", height = 250))
            )
          ))
}