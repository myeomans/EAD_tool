forumTab<-function(){
  tabItem(tabName = "forum",
          h2("Forum Activity"),
          fluidRow(
            box(
              height = "600px",
              tabPanel("Activity in Threads", plotOutput("activity_plot" , height = 600))
            ),
            tabBox(
              height = "350px",
              tabPanel( "Distribution of comments",
                        plotOutput("posts_plot", height = 280)),
              tabPanel("Heat map", plotOutput("heat_plot", height = 280))
            ),
            tabBox(
              height = "350px",
              tabPanel( "Distribution of upvotes",
                        plotOutput("upvotes_plot", height = 280)),
              tabPanel("Exp vs Actual", plotOutput("expected_actual_plot", height = 280))
            )
          )
  )
}