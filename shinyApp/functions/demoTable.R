############################################################
pct_rounded<-function(x){
  return(round(100*mean(x, na.rm=T),1))
}
table_row_build<-function(data, course.start.date){
  if(!("profile_age"%in%names(data))){
    data$profile_age<-(as.numeric(substr(course.start.date, 0, 4))-data$profile_year_of_birth)
  }
  row.values<-list(certified=pct_rounded(data$certificate_status=="downloadable"),
                   female=pct_rounded(data$profile_gender=="f"),
                   bachelors=pct_rounded(data$profile_level_of_education%in%c("b","a","m","p","p_oth","p_se")),
                   USA=pct_rounded(data$profile_country=="US"),
                   age=paste(median(data$profile_age, na.rm=T),round(sd(data$profile_age, na.rm=T),1)),
                   teacher=pct_rounded(data$teach=="Yes"),
                   intent=pct_rounded(grepl("enough",data$reason)),
                   students=nrow(data))
  return(row.values)
}
############################################################
# These lists could be user-adjusted
############################################################
table.row.names<-c( "Observations",
                    "% Female",
                    "College Degree",
                    "Median Age (SD)",
                    "% Teachers",
                    "Intended  Course Completion",
                    "Actual Course Completion")

table.columns<-c("Enrolled in Course", "Answered WVS", "From USA", "Active In Forum") 
############################################################

demoTable <- function(user.data=NULL,enrol.data=NULL,pc.data=NULL, start.date){
  if(is.null(user.data)){ 
    return(NULL)
  } else {
    table_build<-list()
    if ((!is.null(enrol.data))&!is.null(pc.data)){
      enrol.data<-merge(enrol.data,pc.data[,c("user_id","viewed")],by="user_id")
      enrol.data<-enrol.data[(enrol.data$viewed=="True"),]
      table_build[["enrolled"]]<-table_row_build(enrol.data, start.date)
    }
    table_build[["surveyed"]]<-table_row_build(user.data[!is.na(user.data$leftright),], start.date)
    table_build[["ideology_USA"]]<-table_row_build(user.data[(!is.na(user.data$leftright))&(user.data$USA==1),], start.date)
    table_build[["forum_active"]]<-table_row_build(user.data[user.data$activities>0,], start.date)
    #output<- data.frame(table_build)
    
    output <- data.frame( course_enroll = c("5,409", "49.8%", "79.9%", "32 (14.2)", "", "", "14.3%") ,
                          survey_resp = c("2,009", "57.7%","83.3%","36 (14.5)","52.9%","57.2%","31.8%"),
                          usa_samp =c("1,340","61.0%","86.2%","38 (14.9)","57.7%","56.0%","32.7%"),
                          any_act= c("569","59.4%","88.1%","40 (15.1)","63.4%","75.4%","61.0%")
    )
    return(htmlTable(output,
                     header = paste0("&nbsp;&nbsp;&nbsp;",table.columns,"&nbsp;&nbsp;&nbsp;"), 
                     n.tspanner = c(1, 4 ,2), 
                     rnames = table.row.names,
                     tspanner = c("","","")
    ))}
}