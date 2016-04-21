install.packages(dplyr)
install.packages("gridExtra")

devtools::install_github("hadley/babynames")

library(dplyr)
library(babynames)
library(gridExtra)

baby=babynames
baby$sex=as.factor(baby$sex)

str(baby)
summary(baby)


#little mermaid - 1989
ariel <- baby %>%
  filter(name == "Ariel", sex == "F")

#beauty and the beast - 1991
belle <- baby %>%
  filter(name == "Belle", sex == "F")


#alladin - 1992
jasmine <- baby %>%
  filter(name == "Jasmine", sex == "F")


#the princess and the frog - 2009
tiana <- baby %>%
  filter(name == "Tiana", sex == "F")

#brave - 2012
merida <- baby %>%
  filter(name == "Merida", sex == "F")


#frozen - 2013
elsa <- baby %>%
  filter(name == "Elsa", sex == "F")


## defining multiplot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### 


# separate graph for each name - N
a = ggplot(ariel, aes(x=year, y=n)) + 
  geom_line(col="red") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Ariel") + 
  geom_segment(aes(x = 1989, y = 0, xend = 1989, yend = 500), arrow = arrow(length = unit(0.1, "cm")))
a                    


b = ggplot(belle, aes(x=year, y=n)) + 
  geom_line(col="blue") +  
  xlab("") + 
  ylab("") +
  ggtitle("Belle") +
  geom_segment(aes(x = 1991, y = 200, xend = 1991, yend = 30), arrow = arrow(length = unit(0.1, "cm")))
b

c = ggplot(jasmine, aes(x=year, y=n)) + 
  geom_line(col="green") +  
  xlab("") + 
  ylab("") + 
  ggtitle("Jasmine") +
  geom_segment(aes(x = 1992, y = 5000, xend = 1992, yend = 10000), arrow = arrow(length = unit(0.1, "cm")))
c

d = ggplot(tiana, aes(x=year, y=n)) + 
  geom_line(col="orange") +  
  xlab("") + 
  ylab("") + ggtitle("Tiana") +
  geom_segment(aes(x = 2009, y = 0, xend = 2009, yend = 460), arrow = arrow(length = unit(0.1, "cm")))
d


e = ggplot(merida, aes(x=year, y=n)) + geom_line(col="purple") +  
  xlab("") + 
  ylab("") + ggtitle("Merida") +
  geom_segment(aes(x = 2012, y = 0, xend = 2012, yend = 15), arrow = arrow(length = unit(0.1, "cm")))
e

f = ggplot(elsa, aes(x=year, y=n)) + geom_line(col="brown") +  
  xlab("") + 
  ylab("") + ggtitle("Elsa") +
  geom_segment(aes(x = 2013, y = 0, xend = 2013, yend = 500), arrow = arrow(length = unit(0.1, "cm")))
f

# all graphs together
multiplot(a, b, c, d, e, f, cols=2)


### creating .PNG files

### all plots together

png("DisneyNamesALL.png")
multiplot(a, b, c, d, e, f, cols=2)
dev.off()


### separate plots

png("Ariel.png")
a
dev.off()

png("Belle.png")
b
dev.off()

png("Jasmine.png")
c
dev.off()

png("Tiana.png")
d
dev.off()

png("Merida.png")
e
dev.off()

png("Elsa.png")
f
dev.off()

