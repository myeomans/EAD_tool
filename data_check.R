##########################################################################################
# INFO FROM USER
##########################################################################################
course.folder<-"/DATA/spencer/hks101mike/"
course.start.date<-"2015-09-01"
course.team.names<-c("RachelRueckert1","zacharydavis")
course.name<-"American Government"
course.USA.only<-TRUE
##########################################################################################
# DATA LOAD
##########################################################################################
posts_x<-read.csv(paste0(course.folder,"forum.csv"),stringsAsFactors=F)
forum.dat <- read.csv(paste0(course.folder,"HKSforumevents.csv"),stringsAsFactors = F) # for upvotes

enrol_x<-read.csv(paste0(course.folder,"users.csv"),stringsAsFactors = F)
pc_x<-read.csv(paste0(course.folder,"person_course.csv"),stringsAsFactors = F)
users_x<-read.csv(paste0(course.folder,"precourse.csv"),stringsAsFactors = F)
colnames(users_x)[1:10]<-as.character(unlist(users_x[1,1:10]))
users_x<-users_x[2:nrow(users_x),]
##########################################################################################
# DATA CHECKING
##########################################################################################
# enrol: c("id_map_hash_id","user_id","username",
#        profile_year_of_birth
#        certificate_status=="downloadable"
#        profile_gender
#        profile_level_of_education
#        profile_country
#
# users: "user_id","username",
#        teach "Yes"
#        reason "enough"
#        leftright - as.numeric(substr(users_x$wvsurvey_1,0,2))
#        
# posts: "user_id","username",
#        mongoid, comment_thread_id, parent_ids
#        X_type=="CommentThread"
#        abuse_flaggers
#        created_at #(check for a time)
#        body
#        title
#        author_id
#        comment_count
#        author_username
#        
# forum events: "username"
#        thread_id     # actually mongoid!!! VERY ANNOYING
#        forum_action "upvote" "unvote"