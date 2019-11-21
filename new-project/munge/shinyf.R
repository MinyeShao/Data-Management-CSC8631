sub <- function(feature, input, feature_name){
  input = as.data.frame(t(table(input[which(input[,1] %in% c(feature)),2])))
  names(input) = c(feature_name, "correct_rate", "freq")
  input[feature_name] = as.character(feature)
  input["ratio"] = input["freq"] / sum(input["freq"])
  input["ratio"] = round(input["ratio"], 3)
  return(input)
}


main <- function(fid, feature){
  question.response = list(Q2.1,Q2.2,Q2.3,Q2.4,Q2.5,Q2.6,Q2.7)
  if (fid == "all"){
    tmp = as.data.frame(question.response[1])
    for (i in seq(2,8)){
      tmp = rbind(tmp, as.data.frame(question.response[as.integer(i)]))
    }
  } else {
    tmp = as.data.frame(question.response[as.integer(fid)])
  }
  tmp = merge(user,tmp, by.x = "learner_id", by.y = "learner_id")
  tmp = tmp[, names(tmp) %in% c(feature, names(tmp)[length(names((tmp)))])]
  all_f = unique(tmp[,1])
  print(all_f)
  tmp = do.call(rbind,lapply(as.vector(all_f),sub, input=tmp, feature_name=as.character(feature)))
  return(tmp)
}


