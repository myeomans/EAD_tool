
getTheme <- function(){
  
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15),
        axis.title = element_text(face="bold", size=15),
        legend.title = element_text(face="bold", size=7),
        legend.text = element_text(size=7),
        legend.background = element_rect(fill="white", size=1, linetype="solid",colour="black"),
        legend.key.size = unit(0.5, "cm"),
        strip.text.x = element_text(size=13, face="bold"),
        panel.grid.major = element_line(colour = "grey10"),
        panel.grid.minor = element_line(colour = "white"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(family="Times"))
}