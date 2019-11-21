##homework2019.11.15
#library("ProjectTemplate")
#load.project("my_project")
##
#Q1
library(stringr)
path.i <- str_c("data/mooc-dataset-201920/","cyber-security-",i,"_enrolments.csv")
i=1
##
data1 <- read.csv(file = path.i,header = TRUE)
str(data1)

data_fix <- NULL
for (i in 1:7) {
  path.i <- str_c("data/mooc-dataset-201920/","cyber-security-",i,"_enrolments.csv")
  data.i <- read.csv(file = path.i,header = TRUE)
  data_fix <- rbind(data_fix,data.i)
}

str(data_fix)

index<-duplicated(data_fix$learner_id)
data_fix2 <- data_fix[!index,]##Removal of duplicates
str(data_fix2)
data_fix2$enrolled_at <- as.character(data_fix2$enrolled_at)
write.csv(data_fix2,"Q1_1.csv")

#install.packages("lubridate")
library(lubridate)
data_fix2$enrolled_at <- ymd_hms(data_fix2$enrolled_at)
data_fix2$unenrolled_at <- ymd_hms(data_fix2$unenrolled_at)
data_fix2$Total_times <- rep(NA,nrow(data_fix2))

for (i in 1:nrow(data_fix2)) {
  enrolled_at.i <- data_fix2$enrolled_at[i]
  unenrolled_at.i <- data_fix2$unenrolled_at[i]
  if(is.na(enrolled_at.i)==FALSE & is.na(unenrolled_at.i)==FALSE){
    data_fix2$Total_times[i] <- difftime(unenrolled_at.i[1],enrolled_at.i[1],units = "days")##计算时间差
  }
}

##2
for (h in 1:7) {
  fid <- h
  q_resp = read.csv(paste("data/mooc-dataset-201920/cyber-security-",
                          fid,"_question-response.csv", sep = ""))
  str(q_resp)
  ns <- which(q_resp$learner_id=="")
  q_resp <- q_resp[-ns,]
  
  or <- order(q_resp$learner_id,q_resp$quiz_question,q_resp$submitted_at)
  q_resp <- q_resp[or,]
  head(q_resp)
  
  ##Create hash_key
  q_resp$learner_id <- as.character(q_resp$learner_id)
  q_resp$quiz_question <- as.character(q_resp$quiz_question)
  q_resp$hash_key <- paste(q_resp$learner_id,q_resp$quiz_question,sep = "_")
  
  q_new <- matrix(rep(NA,12*length(unique(q_resp$hash_key))),ncol = 12)
  
  colnames(q_new) <- c("hash_key","learner_id","quiz_question",	"Average_time_interval_per_submission(s)","week_number","the_first_try_time",
                       "final_try_time","total_try_times","total_time_costs","true_times","false_times","Accuracy_rate")
  
  q_resp$submitted_at <- ymd_hms(q_resp$submitted_at)
  str(q_new)
  q_new <- as.data.frame(q_new)				
  q_new$hash_key <- unique(q_resp$hash_key)
  
  for (i in 1:nrow(q_new)) {
    key.i <- q_new$hash_key[i]
    q_new$learner_id[i] <- str_split(q_new$hash_key[i],"_")[[1]][1]
    q_new$quiz_question[i] <- str_split(q_new$hash_key[i],"_")[[1]][2]
    w.i <- which(q_resp$hash_key==key.i)
    q_new$week_number[i] <- q_resp$week_number[w.i][1]
    #"the_first_try_time"
    q_new$the_first_try_time[i] <- as.character(q_resp$submitted_at[w.i][1])
    q_new$final_try_time[i] <- as.character(q_resp$submitted_at[w.i][length(w.i)])
    q_new$total_try_times[i] <- length(w.i)
    #Average_time_interval_per_submission(s)
    if(length(w.i)==1 & q_resp$correct[w.i]=="true"){
      q_new$`Average_time_interval_per_submission(s)`[i] <- 0
    }else if(length(w.i)>=2&sum(is.na(q_resp$correct[w.i])==TRUE)==length(w.i)){
      q_new$`Average_time_interval_per_submission(s)`[i] <- NA
    }else{
      t_time <- NULL
      for (j in 1:(length(w.i)-1)) {
        t1.j <- q_resp$submitted_at[w.i][j] 
        t2.j <- q_resp$submitted_at[w.i][j+1]
        t_time[j] <- difftime(t2.j,t1.j,units = "secs")
        q_new$`Average_time_interval_per_submission(s)`[i] <- mean(t_time)
      }
    }
    
    #week number
    
    ##Total time spent
    if(length(w.i)==1){
      q_new$total_time_costs[i] <- 0
    }else{
      q_new$total_time_costs[i] <- difftime(q_resp$submitted_at[length(w.i)],q_resp$submitted_at[1],units = "secs")
    }
    
    T_F <- q_resp$correct[w.i]
    q_new$true_times[i] <- length(which(T_F=="true"))
    q_new$false_times[i] <- length(which(T_F=="false"))
    q_new$`Accuracy_rate`[i] <- round(q_new$true_times[i]/(q_new$true_times[i]+q_new$false_times[i]),2)
  }
  q_new <- q_new[,-1]##The second question is the final result
  NAME <- str_c("Q2_",fid,"_.csv")
  write.csv(q_new,NAME)
}

#3
##After testing, all hash- keys in the third question are not repeated
for (h in 1:7) {
  fid <- h
  q_step = read.csv(paste("data/mooc-dataset-201920/cyber-security-",
                          fid,"_step-activity.csv", sep = ""))
  str(q_step)
  
  or2 <- order(q_step$learner_id,q_step$step_number,q_step$first_visited_at)
  q_step <- q_step[or2,]
  
  ##Get rid of the unfinished problem
  ns <- which(q_step$last_completed_at=="")
  q_step <- q_step[-ns,]
  
  #q_step <- q_step[1:5000,]##Intercept part of the data
  q_step$learner_id <- as.character(q_step$learner_id)
  
  head(q_step)
  q_step$first_visited_at <- ymd_hms(q_step$first_visited_at)
  q_step$last_completed_at <- ymd_hms(q_step$last_completed_at)
  
  q_step$hash_key <- str_c(q_step$learner_id,q_step$step,sep = "_")
  
  q_new2 <- matrix(rep(NA,8*length(unique(q_step$hash_key))),ncol = 8)
  colnames(q_new2) <- c("learner_id","quiz_number","week_number","the_first_answer_time","Final_complete_time","time_cost_for_this_time(s)",
                        "average_time_cost(s)","Total_number_of_completed_questions")
  
  q_new2 <- as.data.frame(q_new2)
  q_new2$the_first_answer_time <- ymd_hms(q_new2$the_first_answer_time)
  q_new2$Final_complete_time <- ymd_hms(q_new2$Final_complete_time)
  
  hash_key <- unique(q_step$hash_key)
  for (i in 1:nrow(q_new2)) {
    hash.i <- hash_key[i]
    q_new2$learner_id[i] <- str_split(hash.i,"_")[[1]][1]
    q_new2$quiz_number[i] <- str_split(hash.i,"_")[[1]][2]
    w.i <- which(q_step$hash_key==hash.i)
    
    q_new2$week_number[i] <- q_step$week_number[w.i][1]
    q_new2$the_first_answer_time[i] <- q_step$first_visited_at[w.i]
    q_new2$Final_complete_time[i] <- q_step$last_completed_at[w.i]
    q_new2$`time_cost_for_this_time(s)`[i] <- difftime(q_new2$Final_complete_time[i],q_new2$the_first_answer_time[i],units = "secs")
    #Analyze each student's information
  }     
  
  n_learner <- unique(q_new2$learner_id)
  
    for (j in 1:length(n_learner)) {
      learner_id.j <- n_learner[j]
      w2.j <- which(q_new2$learner_id==learner_id.j)
      q_new2$Total_number_of_completed_questions[w2.j] <- rep(length(w2.j),length(w2.j))
      q_new2$`average_time_cost(s)`[w2.j] <- rep(mean(q_new2$`time_cost_for_this_time(s)`[w2.j]),length(w2.j)) 
    }
  
  filename <- str_c("Q3_",fid,".1.csv")
  write.csv(q_new2,filename)
}
  