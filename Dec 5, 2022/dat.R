setwd("G:/Desktop/dat")
dat <- read.table("dat.txt", header = TRUE, sep = '\t' )
library(ggplot2)
library(tidyverse)
library(ggalluvial)
#library(readxl)
#dat <- read_excel("dat.xlsx")

dat$month <- rep(c("Nov-","Dec-"), c(13,4))
dat$date1 <- c((18:30),1:4)
dat <- unite(dat, "date", month, date1, sep = '')
dat3 <- data.frame(select(dat, "date", "Remaining"))
dat4 <- data.frame(x = rep(dat$date, time=2),
                   y = c(dat$New.today,dat$Release),
                   group = rep(c("Today newly added","Release"),each = 17))
dat4$x <- factor(dat4$x, levels = c(dat4$x[1:13],dat4$x[14:17]))

ggplot()+
  geom_bar(dat = dat4,aes(x=x,y=y, fill=group),
           stat = "identity", position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("#1d77b4","#ff7f0c"))+
  geom_line(dat = dat3, aes(x = date, y = Remaining/4.5, group = 1), color = "red", size=1)+
  scale_y_continuous(sec.axis = sec_axis(~.*4.5, name = "Remaining close contects"), expand = c(0.01,0))+
  labs(x="Date", y= "Newly added close contects/ Released close contects", caption = "Date Source: National Health Commission",
       title = "Close Contacts in China Mainland (10,000 people)")+
  theme_bw()+
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(size = rel(1.2), angle = (-30)),
        axis.text.y = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_blank(),
        legend.position = "bottom")+
  geom_text(dat = dat4, aes(x=x, y=y, label = y), vjust=-0.5, color="black", 
            position = position_dodge(width = .7))
  
ggsave("11.pdf", width = 9, height = 5)
        
 