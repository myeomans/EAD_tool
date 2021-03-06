forumTab <- function(){
  tabItem(tabName = "forum",
          h2("Forum Activity"),
          fluidRow(
            box(
              height = "700px",
              tabPanel("Activity in Threads", plotOutput("activity_plot" , height = 600),
                       actionButton("activityHelp", "",icon = icon("question")),
                       bsModal(id = "activityHelpModal", 
                               "Help", 
                               trigger = "activityHelp", 
                               size = "small",
                               p("Text activity plot")
                       )
              )
            ),
            tabBox(
              height = "350px",
              tabPanel( "Distribution of comments",
                        plotOutput("posts_plot", height = 280),
                        actionButton("postsHelp", "",icon = icon("question")),
                        bsModal(id = "postsHelpModal", 
                                "Help", 
                                trigger = "postsHelp", 
                                size = "small",
                                p("Text posts plot")
                        )),
              tabPanel("Heat map", plotOutput("heat_post_plot", height = 280),
                       actionButton("heatHelp", "",icon = icon("question")),
                       bsModal(id = "heatHelpModal", 
                               "Help", 
                               trigger = "heatHelp", 
                               size = "small",
                               p("Text heat plot")
                       ))
            ),
            tabBox(
              height = "350px",
              tabPanel( "Distribution of upvotes",
                        plotOutput("upvotes_plot", height = 280),
                        actionButton("upvotesHelp", "",icon = icon("question")),
                        bsModal(id = "upvotesHelpModal", 
                                "Help", 
                                trigger = "upvotesHelp", 
                                size = "small",
                                p("Text upvotes plot")
                        )),
              tabPanel("Heat Map", plotOutput("heat_upvote_plot", height = 280),
                       actionButton("expActHelp", "",icon = icon("question")),
                       bsModal(id = "expActHelpModal", 
                               "Help", 
                               trigger = "expActHelp", 
                               size = "small",
                               p("Text Expected vs Actual plot")
                       )
              )
            ),
            tags$style(type='text/css', "#activityHelp {position:absolute;right:0;bottom:0;}"),
            tags$style(type='text/css', "#postsHelp {position:absolute;right:15px;bottom:20px;}"),
            tags$style(type='text/css', "#heatHelp {position:absolute;right:15px;bottom:20px;}"),
            tags$style(type='text/css', "#upvotesHelp {position:absolute;right:15px;bottom:20px;}"),
            tags$style(type='text/css', "#expActHelp {position:absolute;right:15px;bottom:20px;}")
          )
  )
}