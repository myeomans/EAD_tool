introTab<-function(){
  tabItem(tabName = "instructions",
          h2("Engagement Across Differences"),
          p("Upload forum data to analyse interaction patterns among forum participants."),
          fluidRow(
            box(height = "650px",
                width = 5,
                h2("Upload Data"),
                shiny::tags$hr(),
                fluidRow(
                  column(8,
                         h3("Forum Activity")
                  ),
                  column(4,
                         h2(""),
                         actionButton("postHelp","Help")
                  )
                ),
                fileInputCsv('posts', 'Post data'),
                fileInputCsv('upvotes', 'Upvote data'),
                shiny::tags$hr(),
                fluidRow(
                  column(8,
                         h3("User Data")
                  ),
                  column(4,
                         h2(""),
                         actionButton("userHelp","Help")
                  )
                ),
                fileInputCsv('survey', 'Survey data'),
                fileInputCsv('enrol', 'All Users')
            ),
            box(height = "550px",
                width = 7,
                fluidRow(
                  column(9,
                         h2("Forum Settings")
                  ),
                  column(3,
                         h2(""),
                         actionButton("settingsHelp","Help")
                  )),
                textInput('course.name', 'Forum Name', value="American Government"),
                dateInput('start.date', 'Forum Launch Date', value="2015-09-01"),
                shiny::tags$hr(),
                checkboxInput('course.only', 'Admin-Created Threads Only', value=TRUE),
                textInput('admin.names', 'Admin User IDs', value="7577470,1668688"),
                p("Separate with commas (e.g. \"Matthew,Mark,Luke,John\")"),
                shiny::tags$hr(),
                checkboxInput('usa.only', 'American Students Only', value=TRUE),
                checkboxInput('self.posts', 'Include Comments to Own Posts', value=FALSE)
            )
          ),
          bsModal(id = "postHelpModal", trigger = "postHelp",size = "large",
                  title="Guidelines for Activity Data",
                  h3("Post Data"),
                  p("Each post is identified by a unique post_id, along with the poster's user_id 
                     and the body of the text. Replies should be organized in \"levels\". The top-level 
                     posts (level = 1) may have a title. All other posts (level > 1) should have a 
                     parent_id that points to the post to which it is a direct reply, and a 
                     thread_id that points to its top-level post."),
                  
                  
                  htmlTable(data.frame(cbind(c("post_id","user_id","title","body","time",
                                               "level","parent_user_id","parent_id","thread_id"),
                                             c("unique id for each post","unique id for each user",
                                               "title of thread","text of post","time stamp (YYYY-MM-DD HH:MM:SS)",
                                               "post level (1 = top)","user id of replied-to poster","post id of replied-to post","id for top post in thread"))),
                            header = c("name", "description"),
                            css.cell = "padding-left: 2em; padding-right: 2em;",
                            align="l",rnames = FALSE),
                  
                  shiny::tags$hr(),
                  h3("Upvote Data (optional)"),
                  p("If users can vote (or like) posts, these data can be added here. 
                     Upvotes should be in the same format as lower-level posts, with four variables."),
                  
                  htmlTable(data.frame(cbind(c("post_id","parent_user_id","time","user_id"),
                                             c("post id for upvoted post","user id of upvoted poster",
                                               "time stamp (YYYY-MM-DD HH:MM:SS)","user id of upvoter"))),
                            header = c("name", "description"),
                            css.cell = "padding-left: 2em; padding-right: 2em;",
                            align="l",rnames = FALSE),
                  shiny::tags$hr()
          ),
          bsModal(id = "userHelpModal", trigger = "userHelp", size = "large",
                  title="Guidelines for User Data",
                  h3("Survey Data"),
                  p("The survey can include a number of covariates, including the focal difference (e.g. ideology),
                    as well as general descriptives of the population. The focal difference must be defined in this dataset
                    before uploading, even if it is a tranformation of another variable (e.g. ideology on a 10-point scale or binned in thirds)."),
                  
                  htmlTable(data.frame(cbind(c("user_id","username","gender","certified","bachelors","USA",
                                               "age","teacher","intent","wvs_ideology","TriPart"),
                                             c("unique id for each user","username in forum","gender (0=female, 1=male)",
                                               "completed the class (0/1)", "college degree (0/1)", "From USA (0/1)",
                                               "age (in years)", "Current teacher (0/1)","Intended to complete the class (0/1)",
                                               "Ideology on 10-point WVS scale", "Ideology binned into terciles"))),
                            header = c("name", "description"),
                            css.cell = "padding-left: 2em; padding-right: 2em;",
                            align="l",rnames = FALSE),
                  
                  shiny::tags$hr(),
                  h3("All Users (optional)"),
                  p("In some cases, not every user will take the survey. If there are covariates 
                    that can be measured without the survey, matched to user_id, these can be uploaded here.
                    This allows "),
                  
                  p("In our example, the following data are measured from outside the survey: gender, certified, bachelors,	USA, age."),
                  shiny::tags$hr()
          ),
          bsModal(id = "settingsHelpModal", trigger = "settingsHelp", size = "large",
                  title="Guidelines for Forum Settings",
                  shiny::tags$hr(),
                  h3("Forum Name and Launch Date"),
                  p("These fields are used for labelling graphs, but do not affect any analyses."),
                  p("The launch date format is: YYYY-MM-DD"),
                  shiny::tags$hr(),
                  h3("Admin-Created Threads"),
                  p("In many cases, user-created threads will be very different from the official threads
                    created by the forum administrators (e.g. shorter, off-topic, etc). If you wish to 
                    focus only on official threads, check this box and enter the \"user id\" for all 
                    admin accounts, separated with commas (e.g. \"Matthew,Mark,Luke,John\")"),
                  shiny::tags$hr(),
                  h3("Other Filters"),
                  p("Some measures of difference may be hard to consistently measure across cultures 
                    (e.g. political ideology). Use this toggle to focus on forum activity of US users only."),
                  
                  p("Including users who comment on their own posts can inflate estimates of siloing,
                    because users by definition have the same ideology as themselves. You may filter out
                    self-replies using this checkbox (self-upvoting should be removed before uploading)"),
                  shiny::tags$hr()
          )
  )
}