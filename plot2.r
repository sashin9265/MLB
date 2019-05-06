#The relationship between team performace and the salary gap between players
#Considering palyers salary whose AB is more than 400 after 2010 season

a2010 <- subset(Salaries, yearID > 2010)
b2010 <- subset(Batting, yearID > 2010)

c2010 <- merge(a2010, b2010, by = "playerID")
c2010$teamyear <- paste(c2010$teamID.x, c2010$yearID.x, sep="")
c2010 <- subset(c2010, AB > 400)

func_2 <- function(c2010){return(data.frame(sd = sd(c2010$salary)))}
team_sd <- ddply(c2010, .(teamyear), func_2)
d2010 <- subset(Teams, yearID > 2010)
d2010$WP <- d2010$W / d2010$G
d2010$teamyear <- paste(d2010$teamID, d2010$yearID, sep = "")
e2010 <- merge(team_sd,d2010,by="teamyear")

data <- e2010[, c("sd", "WP")]
fit <- lm(WP~sd, data)
plot(e2010$sd, e2010$WP)
lines(smooth.spline(e2010$sd, e2010$WP))

p <- plot_ly(data, x = ~sd, color = I("black")) %>%
  add_markers(y = ~WP, text = rownames(data), showlegend = FALSE) %>%
  add_lines(y = ~fitted(loess(WP ~ sd)),
            line = list(color = '#07A4B5'),
            name = "Loess Smoother", showlegend = TRUE) %>%
  layout(xaxis = list(title = '
                      Difference in annual salary (By team)'),
         yaxis = list(title = 'Winning Rate'),
         legend = list(x = 0.80, y = 0.90))

chart_link = api_create(p, filename="line-model")