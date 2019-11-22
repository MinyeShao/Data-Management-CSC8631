plot_new =  function(student_info_f){
  f1 = read.csv(file="d:/CSC8631_CW/Data-Management-CSC8631/new-project/data/Q1_1.csv", header=TRUE, sep=",")
  f1 = f1[, c("learner_id", student_info_f)]
  f1 = f1[which(f1[,2] != "Unknown"),]
  n = nrow(f1)
  names(f1) = c("id", "feature")
  f1 = count(f1, feature)
  f1$n = round(f1$n / n, 3)
  names(f1) = c("type", "rate")
  ggplot(data=f1, mapping=aes(x=student_info_f,y=rate,fill=type))+
    geom_bar(stat="identity",width=1,position='stack',size=5)+
    coord_polar("y", start=0)+
    blank_theme +
    geom_text(stat="identity",aes(y=rate, label = scales::percent(rate)), size=5, position=position_stack(vjust = 0.6),col="steelblue")
}
blank_theme =  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=5)
  )
