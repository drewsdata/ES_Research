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
  scale_fill_hue(name="Globex\nSession Close", breaks=c("E9R", "E8R", "E7R", "E6R", "E5R", "E4R", "E3R","E2R", "E1R","E0R"), labels=c("Mon_1", "Tues_1", "Wed_1", "Thurs_1", "Fri_1", "Mon_0", "Tues_0", "Wed_0", "Thurs_0", "Fri_0"))