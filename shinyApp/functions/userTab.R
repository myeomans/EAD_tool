userTab<-function(){
  tabItem(tabName = "users",
          h2("User Information"),
          fluidRow(
            box(
              height = "300px",
              width = 12,
              tabPanel("Descriptive statistics for sub-populations of forum participants", 
                       htmlOutput("descriptive_table"))
            )),
          fluidRow(
            box(
              width = 6,
              height = "300px",
              tabPanel("Distribution of Interest", plotOutput("distribution_plot", height = 250))
            )
          ))
}