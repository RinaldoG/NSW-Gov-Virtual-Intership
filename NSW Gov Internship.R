library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


nswGov <- read_xlsx("NSWGov.xlsx",sheet=3)

colSums(is.na(nswGov))

table(nswGov$Year)
table(nswGov$Gender)
table(nswGov$`PT/FT`)
table(nswGov$Cluster)

#Each Cluster Mean -  Yearly - by Gender/'PT/FT'
nswGov %>% 
  filter(!Cluster == "Education" & !Cluster == "Health")%>%
  group_by(Year,Cluster,`PT/FT`,Gender) %>%
  summarise("Cluster Sum" = sum(Headcount))%>%
  ggplot(aes(x=Year,y=`Cluster Sum`,colour=interaction(`PT/FT`,Gender),
             group=interaction(`PT/FT`,Gender))) +
  geom_line() +
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(vars(Cluster),ncol=4)


#Total Gender Yearly
nswGov %>%
  group_by(Year,Gender) %>%
  summarise("Gender Total" = sum(Headcount))%>%
  ggplot(aes(x=Year,y=`Gender Total`,group=Gender,colour=Gender,fill=Gender))+
  geom_col(alpha=.75)

nswGov %>%
  group_by(Year,Gender) %>%
  summarise("Gender Headcount" = sum(Headcount))

#Part Time
nswGov %>%
  filter(`PT/FT`=="Part-Time") %>%
  filter(!Cluster == "Education" & !Cluster == "Health")%>%
  group_by(Year,Cluster)%>%
  summarise("Cluster Sum" = sum(Headcount)) %>%
  ggplot(aes(x=Year,y=`Cluster Sum`,group=Cluster,colour=Cluster)) +
  geom_line()+
  geom_point()

nswGov %>%
  filter(`PT/FT`=="Part-Time") %>%
  filter(Cluster == "Education" | Cluster == "Health")%>%
  group_by(Year,Cluster)%>%
  summarise("Cluster Sum" = sum(Headcount)) %>%
  ggplot(aes(x=Year,y=`Cluster Sum`,group=Cluster,colour=Cluster)) +
  geom_line()+
  geom_point()

nswGov %>%
  filter(`PT/FT`=="Part-Time") %>%
  group_by(Year)%>%
  summarise("HC Sum" = sum(Headcount)) %>%
  ggplot(aes(x=Year,y=`HC Sum`)) +
  geom_line()+
  geom_point()


#Part Time Proportion
nswGov %>%
  filter(Year==2018) %>%
  group_by(Cluster,`PT/FT`,Gender) %>%
  summarise("HC Sum" = sum(Headcount)) %>%
  ggplot(aes(x=Cluster,y=`HC Sum`,group=interaction(`PT/FT`,Gender),
             fill=interaction(`PT/FT`,Gender))) +
  geom_col(position="dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(nswGov$Cluster)

#Total Proportions
nswGov %>%
  group_by(Year,`PT/FT`,Gender) %>%
  summarise("HC Sum" = sum(Headcount)) %>%
  ggplot(aes(x=Year,y=`HC Sum`,group=interaction(`PT/FT`,Gender),
             colour=interaction(`PT/FT`,Gender)))+
  geom_line()+
  geom_point()


#Total Proportions Predictions 
OFF <- nswGov %>%
  filter(Gender == "Female") %>%
  filter(`PT/FT`=="Full-Time") %>%
  group_by(Year,`PT/FT`,Gender) %>%
  summarise("HC" = sum(Headcount))

OFP <- nswGov %>%
  filter(Gender == "Female") %>%
  filter(`PT/FT`=="Part-Time") %>%
  group_by(Year,`PT/FT`,Gender) %>%
  summarise("HC" = sum(Headcount))

OMF <- nswGov %>%
  filter(Gender == "Male") %>%
  filter(`PT/FT`=="Full-Time") %>%
  group_by(Year,`PT/FT`,Gender) %>%
  summarise("HC" = sum(Headcount))

OMP <- nswGov %>%
  filter(Gender == "Male") %>%
  filter(`PT/FT`=="Part-Time") %>%
  group_by(Year,`PT/FT`,Gender) %>%
  summarise("HC" = sum(Headcount))

time <- data.frame(c(2019:2025))
colnames(time) <- "Year"

OMFfit <-lm(HC ~ Year,data = OMF)
OMPfit <-lm(HC ~ Year,data = OMP)
OFFfit <-lm(HC ~ Year,data = OFF)
OFPfit <-lm(HC ~ Year,data = OFP)

FFP <- predict(OFFfit,time)
FPP <- predict(OFPfit,time)
MPP <- predict(OMPfit,time)
MFP <- predict(OMFfit,time)

OFF[c(6:12),1] <- time
OFF[,2] <- "Full-Time"
OFF[,3] <- "Female"
OFF[c(6:12),4] <- FFP

OFP[c(6:12),1] <- time
OFP[,2] <- "Part-Time"
OFP[,3] <- "Female"
OFP[c(6:12),4] <- FPP

OMF[c(6:12),1] <- time
OMF[,2] <- "Full-Time"
OMF[,3] <- "Male"
OMF[c(6:12),4] <- MFP

OMP[c(6:12),1] <- time
OMP[,2] <- "Part-Time"
OMP[,3] <- "Male"
OMP[c(6:12),4] <- MPP

Prop_Pred <- rbind(OFF,OFP,OMF,OMP)
plot_col <- c("#e6005c","#ff66a3","#0073e6","#66b3ff")
Prop_Pred %>%
  ggplot(aes(x=Year,y=`HC`,group=interaction(`PT/FT`,Gender),
             colour=interaction(`PT/FT`,Gender)))+
  annotate("rect",xmin=2013.5,xmax=2018.5,ymin=0,ymax=160000,
           fill="#4dff4d",alpha=0.5)+
  annotate("rect",xmin=2018.5,xmax=2025.5,ymin=0,ymax=160000,
           fill="#ff944d",alpha=0.5)+
  ggtitle("Employment Type Prediction (2014-2025)")+
  annotate("segment", x = 2018.5, xend = 2018.5, y= 0, yend = 160000,
           colour = "red", size = .5)+
  annotate("text",x=2018.3,y=55000,label="Prediction Begins",size=2.5,angle=90)+
  labs(y="Head Count",colour="Legend")+
  labs(caption="*F/T: Full-Time    P/T=Part-Time")+
  scale_color_manual(values=c(plot_col),labels=c("F/T Female","P/T Female",
                                                 "F/T Male","P/T Male"))+
  geom_line()+
  geom_point()
  




#Male Full-Time Trends

nswGov %>%
  filter(Gender=="Male")%>%
  filter(`PT/FT`=="Full-Time") %>%
  group_by(Cluster,Year) %>%
  summarise("HC" = sum(Headcount)) %>%
  ggplot(aes(x=Year,y=HC,colour=Cluster,fill=Cluster))+
  geom_col()+
  facet_wrap(vars(Cluster),ncol=5,scales="free_y")+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(strip.text.x = element_blank())+
  theme(axis.text.x = element_text(angle=90))

#Clusters Where we are missing the most:
#Education
#Family
#Finance
#Industry
#Treasury

nswGov %>%
  filter(Gender=="Male")%>%
  filter(`PT/FT`=="Full-Time") %>%
  filter(!Cluster=="Health")%>%
  filter(!Cluster=="Justice")%>%
  filter(!Cluster=="Planning & Environment")%>%
  filter(!Cluster=="Premier & Cabinet")%>%
  filter(!Cluster=="Transport")%>%
  group_by(Cluster,Year) %>%
  summarise("Head Count" = sum(Headcount)) %>%
  ggplot(aes(x=Year,y=`Head Count`,colour=Cluster,fill=Cluster))+
  geom_col()+
  facet_wrap(vars(Cluster),ncol=5)+
  theme(strip.text.x = element_blank())+
  theme(axis.text.x = element_text(angle=90))

MF_Cluster <- nswGov %>%
  filter(Year=="2014" | Year =="2018")%>%
  filter(Gender=="Male")%>%
  filter(`PT/FT`=="Full-Time") %>%
  filter(!Cluster=="Health")%>%
  filter(!Cluster=="Justice")%>%
  filter(!Cluster=="Planning & Environment")%>%
  filter(!Cluster=="Premier & Cabinet")%>%
  filter(!Cluster=="Transport")%>%
  group_by(Cluster,Year) %>%
  summarise("HC" = sum(Headcount)) %>%
  group_by(Cluster) %>%
  summarise("Total HC Change" = head(HC,2)-head(HC,1),
            "Total % Change" = 100-(head(HC,2)/head(HC,1)*100))

MF_Cluster <- MF_Cluster[c(seq(2,10, 2)),]
MF_Cluster
#Clusters with significant decline between years 2014-2018:
#Education
#Family
#Industry

#Education Agency Analysis
MF_Edu <- nswGov %>%
  filter(Gender=="Male") %>%
  filter(`PT/FT`=="Full-Time") %>%
  filter(Cluster == "Education")

MF_Edu %>%
  ggplot(aes(x=Year,y=Headcount,colour=Agency)) +
  geom_line()+
  geom_point()+
  facet_wrap(vars(Agency),ncol=4)

#Education Agency 4 has the highest number of headcount and the greatest
# loss of people over the years

#Family & Community Services Agency Analysis
MF_FCS <- nswGov %>%
  filter(Gender=="Male") %>%
  filter(`PT/FT`=="Full-Time") %>%
  filter(Cluster == "Family & Community Services")

MF_FCS %>%
  ggplot(aes(x=Year,y=Headcount,colour=Agency)) +
  geom_line()+
  geom_point()+
  facet_wrap(vars(Agency),ncol=3)

#Family & Community Services Agency 1 has the highest number of headcount 
# and the greatest loss of people over the years

#Industry Agency Analysis
MF_Ind <- nswGov %>%
  filter(Gender=="Male") %>%
  filter(`PT/FT`=="Full-Time") %>%
  filter(Cluster == "Industry")

MF_Ind %>%
  ggplot(aes(x=Year,y=Headcount,colour=Agency)) +
  geom_line()+
  geom_point()+
  facet_wrap(vars(Agency),ncol=4)

#Industry Agency 7 has the highest number of headcount 
# and the greatest loss of people over the years























































