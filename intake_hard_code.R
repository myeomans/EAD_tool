##########################################################################################
# DATA LOAD
##########################################################################################
source("./intake_functions.R")

course.folder<-"/DATA/spencer/hks101mike/"
posts_ag<-read.csv(paste0(course.folder,"forum.csv"),stringsAsFactors=F)
forum_ag <- read.csv(paste0(course.folder,"HKSforumevents.csv"),stringsAsFactors = F) # for upvotes
enrol_ag<-read.csv(paste0(course.folder,"users.csv"),stringsAsFactors = F)
pc_ag<-read.csv(paste0(course.folder,"person_course.csv"),stringsAsFactors = F)
users_ag<-read.csv(paste0(course.folder,"precourse.csv"),stringsAsFactors = F)

DC<-data.checker(posts_x=posts_ag,
                 forum_x=forum_ag,
                 enrol_x=enrol_ag,
                 pc_x=pc_ag,
                 users_x=users_ag,
                 of.interest="wvsurvey_1",
                 USA.only=TRUE,
                 self.posts=FALSE, 
                 course.threads.only=TRUE,
                 course.name="American Government",
                 start.date="2015-09-01",
                 course.team.names=c("RachelRueckert1","zacharydavis"))
##########################################################################################