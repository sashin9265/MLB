library(Lahman)
library(tidyverse)
library(gganimate)
library(gapminder)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)
theme_set(theme_classic())

top_sal <- merge(x=Salaries, y=Master, by="playerID", all.x=TRUE)
top_sal <- top_sal[, c("yearID", "salary", "nameFirst", "nameLast")]
top_sal$name <- paste(top_sal$nameFirst,top_sal$nameLast)
top_sal <- top_sal[, c("yearID", "salary", "name")]
top_sal$yearID <- as.factor(top_sal$yearID)

a <- top_sal %>%
  group_by(yearID) %>%
  top_n(n = 10, wt = salary) %>% arrange(-desc(yearID))

resultis <- list()
for (i in 1985:2016) {
  p <- top_sal  %>%  filter (yearID == i) %>% arrange(desc(salary)) %>% top_n(10) %>% mutate(Rank = min_rank(-salary)*1, Value_rel = salary/salary[Rank==1], Value_lbl = paste0(" ",salary))
  resultis[[i]] <- p
}

b <- as.data.frame(do.call("rbind", resultis))


p <- b  %>%
  # build rank, labels and relative values
  group_by(yearID) %>% ungroup() 

# plot

p2<-  ggplot(p, aes(-Rank,Value_rel, fill = name, label = name)) +
  geom_col(width = 0.8, position="identity") +
  coord_flip() + 
  geom_text(aes(-Rank,y=0,label = name,hjust=0)) +       #country label
  geom_text(aes(-Rank,y=Value_rel,label = Value_lbl, hjust=0)) + # value label
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1, size = 30),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  # animate along Year
  transition_states(yearID,1,1) +
  #add
  scale_color_viridis_d(name="")+
  scale_fill_viridis_d(name="")+
  theme_tufte(14,"Avenir")+
  guides(color=F,fill=F)+
  labs(title = 'Year: {closest_state}',  x = "",y="Salary compared to Rank 1's")

animate(p2, 100, fps = 25, duration = 40, width = 800, height = 600)
