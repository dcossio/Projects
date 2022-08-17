
 df <- as.data.frame(read.csv("/Volumes/GoogleDrive/My Drive/White matter tract/DSI_figs/Fornix_stats.csv", header = TRUE))
# 
#  ggplot(df, aes(x=QA_.Fornix.R, y=AvgACC))  + 
#   geom_point(size=2) +
#    geom_smooth(method="lm", level=0.95) +
#     ggtitle("Negative QA") +
#      theme(plot.title = element_text(hjust = 0.5))
#  
#  
#  ggplot(df, aes(x=RD_Fornix.R, y=AvgACC)) +
#    geom_point(aes(color = factor(subs))) +
#    geom_smooth(formula = y ~ x, method = "lm")+
#    labs(x= "Radial Diffusivity", y= "Proportion of correct trials" ) +
#    stat_cor(method = "spearman", label.x.npc = 0.8, label.y.npc = 0.9)+
#    stat_regline_equation(label.y = 0.85, label.x = 2, aes(label = ..rr.label..)) +
#    stat_regline_equation(label.y = 0.8,label.x = 2, aes(label = ..eq.label..)) +
#    theme(legend.position = "none") +
#    ylim(0, 1) +
#    xlim(0, 2.5) 

 
 DF_raw <- as.data.frame(read.csv("/Volumes/GoogleDrive/My Drive/White matter tract/Midlife_SNAG/RAW/Behavior_only/JHU-MD_Behav.csv"))
            
 newDF <- cbind(df$subs,df$QA_.Fornix.R)
 newDF <- cbind(newDF,participant_info$AvgAccuracy)     
 newDF <- as.data.frame(newDF)
 
 max(newDF$V2)
 
 ggplot(newDF, aes(x=V2, y=V3)) +
   geom_point(aes(color = factor(V1)), size=4) +
   geom_smooth(formula = y ~ x, method = "lm")+
   labs(x= "Quantitative Anisotropy", y= "Proportion of correct trials" ) +
   theme(legend.position = "none") +
    stat_cor(method = "spearman", label.x.npc = 0.70, label.y.npc = 1) +
    stat_regline_equation(label.y = 0.95, label.x = 6e-05, aes(label = ..rr.label..)) +
   xlim(min(newDF$V2), max(newDF$V2)) +
    font("xlab", size=30)+
    font("ylab", size=30)

 

 
 
 
 tmp <- pcor.test(newDF[,2],newFA[,R+2],newFA[,c("Sex","age")])
 