library("tidyverse")
library('ggplot2')

trees <- read.csv('/Users/skyejung/Desktop/STAT 3280/nytrees.csv')
groupedtrees <- read.csv('/Users/skyejung/Desktop/STAT 3280/nytreesclass.csv')

trees <- trees %>%
  drop_na()%>%
  select(health, spc_common,status,boroname,healthnum)

bartrees <- trees%>%
  group_by(boroname,health)%>%
  summarize(count=n())

pietrees <- trees%>%
  group_by(boroname)%>%
  summarize(count=n())%>%
  mutate(percentage=count*100/sum(count))

pietrees$percentage <- round(pietrees$percentage,digits=2)

avgborohealth <- trees %>%
  group_by(boroname)%>%
  summarize(avghealth=mean(healthnum))

toptreestotal <- trees%>%
  group_by(spc_common)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  slice(1:10)%>%
  mutate(spc_common=recode(spc_common,"Callery pear" = "Callery Pear",
                           "ginkgo" = "Ginkgo","honeylocust" = "Honey Locust", 
                           "London planetree"="London Planetree", "pin oak"="Pin Oak",
                           'Norway maple'='Norway Maple',
                           "littleleaf linden"="Littleleaf Linden",'cherry'='Cherry',
                           'Japanese zelkova'='Japanese Zelkova'))

toptrees <- trees%>%
  group_by(boroname,spc_common)%>%
  summarize(count=n())%>%
  arrange(desc(count)) %>% 
  group_by(boroname) %>%
  slice(1:3)%>%
  mutate(spc_common=recode(spc_common,"Callery pear" = "Callery Pear",
                               "ginkgo" = "Ginkgo",
                               "honeylocust" = "Honey Locust", 
                               "London planetree"="London Planetree",
                               "pin oak"="Pin Oak",
                               "red maple"="Red Maple"))
  
## total tree count for NY boroughs AND tree health count per borough
ggplot(bartrees, aes(x=boroname,y=count,fill=health))+ 
  geom_bar(position='fill', stat='identity')+
  scale_fill_manual(values=c("#EEAD0E","#6E8B3D","#CD3333"))+
  labs(y="Trees", x="New York Boroughs",fill="Tree Health",
       title='Tree Health Percentage per NY Borough')

## pie chart of percentage of trees in NY
ggplot(pietrees, aes(x="", y=percentage, fill=boroname)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5))+
  labs(x = NULL, y = NULL, fill = NULL,
       title='Percentage of Total Trees in per NY Borough') +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values=c("#CD3333", "#A2CD5A", "#EEAD0E", "#556B2F","#CD6600"))

## average tree health amongst ALL trees per NY borough
ggplot(avgborohealth, aes(x=boroname,y=avghealth,fill=boroname))+ 
  geom_bar(stat='identity')+
  scale_fill_manual(values=c("#CD3333", "#A2CD5A", "#EEAD0E", "#556B2F","#CD6600"))+
  geom_text(aes(label = signif(avghealth, digits = 3)), nudge_y = 0.1)+
  labs(x = "New York Boroughs", y = "Average Tree Health",
       title="Average Tree Health per NY Borough")+
  theme(legend.position = "none")+coord_flip()   

## distribution of each tree type average healths per NY borough
ggplot(groupedtrees, aes(x=avghealth,fill=boroname))+geom_histogram(bins=24)+
  scale_fill_manual(values=c("#CD3333", "#A2CD5A", "#EEAD0E", "#556B2F","#CD6600"))+
  labs(x = "Average Tree Health", y = "Count",fill="New York Boroughs",
       title="Distribution of Average Tree Healths per NY Borough")

## most common trees per borough in NY
ggplot(toptrees, aes(x=boroname,y=count,fill=spc_common))+
  geom_bar(position='stack', stat='identity')+
  scale_fill_manual(values=c("dark green","#6E8B3D", "#EEAD0E","#A2CD5A","#D2691E","#B22222"))+
  labs(y="Tree Count", x="New York Boroughs",fill="Tree Type",
       title="Top Three Most Common Trees Count per NY Borough")+
  geom_text(aes(label = count), colour = "white", size = 3, 
            vjust = 1.5, position = position_stack(.9))

## most common trees
ggplot(toptreestotal, aes(x=spc_common,y=count))+
  geom_bar(position='stack', stat='identity')+
  scale_fill_manual(values=c("dark green","#6E8B3D", "#EEAD0E","#A2CD5A","#D2691E","#B22222"))+
  labs(y="Tree Count", x="Tree Type",
       title="Top Ten Most Common Trees in NY")+
  geom_text(aes(label = count), colour = "white", size = 3, 
            vjust = 1.5, position = position_stack(.9))+
  theme (axis.text.x = element_text (angle = 110))

toptreestotal
  