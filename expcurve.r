# Expiration curve over two week globex session high - low from period to perdiod end
# load expcurve data
expcurved <- read.delim("C:/Trading/Research/Statistical Studies/Weekly Options/2wkgxcountdown/expcurve.txt")

# load ggplot2
library (ggplot2)

# create basic plot object
expcurvep <- ggplot(expcurved, aes(factor(expcurved$Period, levels=c('E9R', 'E8R','E7R','E6R','E5R','E4R','E3R','E2R','E1R','E0R')), expcurved$ESrange)) +
  opts(axis.title.x = theme_blank()) +
  opts(axis.title.y = theme_blank())

# create final boxplot with aesthetics
expcurvep + geom_boxplot(aes(fill = expcurved$Period),outlier.size=0) +
  geom_jitter(position=position_jitter(width=0.2), size=5, shape="*") +
  scale_y_continuous(breaks=seq(0,200,5)) + opts(panel.grid.minor=theme_blank()) +
  stat_summary(fun.y=mean, geom="point", shape=7, size=5,color="white") +
  scale_fill_hue(name=" ES Globex Range\n Per Two Week\n Observation", breaks=c("E9R", "E8R", "E7R", "E6R", "E5R", "E4R", "E3R","E2R", "E1R","E0R"), labels=c("Mon_1", "Tues_1", "Wed_1", "Thurs_1", "Fri_1", "Mon_0", "Tues_0", "Wed_0", "Thurs_0", "Fri_0")) +
  opts(title="ES Globex Range Contraction per Globex Session Over Two Weeks  \n Box Plot of 51 Two-Week Observations (08.08.2010 - 07.19.2012)") +
  opts(plot.title = theme_text(size=18, lineheight=1, face="bold"))


# create stat summary functions
q50 <- function(x) {quantile(x,probs=0.5)} 
q84 <- function(x) {quantile(x,probs=0.84)} 
q975 <- function(x) {quantile(x,probs=0.975)}

# create plot
ggplot(expcurved, aes(factor(expcurved$Period, levels=c('E9R', 'E8R','E7R','E6R','E5R','E4R','E3R','E2R','E1R','E0R')), expcurved$ESrange,color=(expcurved$Period), size=1.5)) +
  geom_jitter(position=position_jitter(width=0.15)) +
  scale_size(guide="none") +
  opts(axis.title.x = theme_blank()) +
  opts(axis.title.y = theme_blank()) +
 
  
  # add stat summary functions
  stat_summary(fun.y=q975, colour="red", geom="crossbar", size = 1, ymin=0, ymax=0) +
  stat_summary(fun.y=q84, colour="blue", geom="crossbar", size = 1, ymin=0, ymax=0) +
  stat_summary(fun.y=q50, colour="black", geom="crossbar", size = 1, ymin=0, ymax=0) +
  
  # change the legend to have a white background
  opts(legend.key=theme_rect(fill="white",colour="white")) +
  opts(legend.background = theme_blank()) +
  # scale_colour_hue(name = " ES Globex Range\n Per Two Week\n Observation") +
  scale_colour_hue(name=" ES Globex Range\n Per Two Week\n Observation", breaks=c("E9R", "E8R", "E7R", "E6R", "E5R", "E4R", "E3R","E2R", "E1R","E0R"), labels=c("Mon_1", "Tues_1", "Wed_1", "Thurs_1", "Fri_1", "Mon_0", "Tues_0", "Wed_0", "Thurs_0", "Fri_0")) +  
  
  # set scale
  scale_y_continuous(breaks=seq(0,200,5)) +
  
  # set background lines
  opts(panel.grid.minor=theme_blank()) +
  
  # add annotation
  annotate("text", x=9.5, y=170, label="   Red Line:  97.5%", col = "Red", cex=8) +
  annotate("text", x=9.5, y=160, label="Blue Line:  84%", col = "Blue", cex=8) + 
  annotate("text", x=9.5, y=150, label=" Black Line:  50%", col= "Black", cex=8) +
  
  opts(title="ES Globex Range Contraction per Globex Session Over Two Weeks  \n 51 Two-Week Observations (08.08.2010 - 07.19.2012)") +
  opts(plot.title = theme_text(size=18, lineheight=1, face="bold"))
