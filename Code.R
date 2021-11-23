library(tidyverse)
library(tidytuesdayR)
library(ggtext)
tuesdata <- tidytuesdayR::tt_load('2021-11-23')
tuesdata$writers->writers
tuesdata$directors->directors
tuesdata$episodes->episodes
tuesdata$imdb->imdb


glimpse(writers)
glimpse(directors)
glimpse(episodes)
glimpse(imdb)



episodes%>%
  filter(type=="episode")%>%
  filter(season_number<=12)%>%
  select(season_number,episode_number,uk_viewers)%>%
  mutate(uk_viewers=round(uk_viewers,2))%>%
  group_by(season_number,episode_number)%>%
  data.frame()->data


data%>%
  mutate(season_number=fct_relevel(season_number,levels="SEASON 1","SEASON 2","SEASON 3",
                                   "SEASON 4","SEASON 5","SEASON 6","SEASON 7","SEASON 8","SEASON 9",
                                   "SEASON 10","SEASON 11","SEASON 12"))->data


data
data%>%
  mutate(season_number=toupper(recode(season_number,"1"="Season 1","2"="Season 2","3"="Season 3",
                              "4"="Season 4","5"="Season 5","6"="Season 6","7"="Season 7",
                              "8"="Season 8","9"="Season 9","10"="Season 10","11"="Season 11",
                              "12"="Season 12","13"="Season 13")))->data
data%>%
  group_by(season_number)%>%
  mutate(label=ifelse(uk_viewers==max(uk_viewers),paste0("Episode",episode_number, " ~ ",max(uk_viewers),"M"),""))%>%
  mutate(lilt=ifelse(uk_viewers==max(uk_viewers),"#F8766D","gray75"))%>%
    data.frame()->data



ggplot(data, aes(x=as.factor(episode_number),y=uk_viewers,fill=lilt,label=label))+
  geom_col(colour="black")+
  coord_polar() +
  geom_text(aes(label=label,x=factor(episode_number), y=(uk_viewers)+5),size=3,colour="white")+
  labs(fill="Episode number")+
  facet_wrap(~season_number,ncol=4)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_markdown(size=14,face="bold",margin=margin(b=15)),
        plot.subtitle = element_markdown(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=10, colour="white",hjust=0,margin=margin(t=5)))+
   scale_fill_manual(values=c("#F8766D", "gray"))+
  labs(title="<span style='color:white'>DOCTOR WHO: THE 
         <span style='color:#F8766D'>MOST-WATCHED
         <span style='color:white'>EPISODES OF EACH SEASON</span>",
       subtitle = "Each bar represents one episode, its height indicates the number of viewers (in million) in the UK",
       caption="Data: Tidy Tuesday| Design and Analysis: @annapurani93")->plot


ggsave("doctorwho.png",plot,width=9,height=8.3)




