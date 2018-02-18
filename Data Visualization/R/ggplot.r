rm(list = ls())
options(stringsAsFactors = FALSE)

library(readr) #could use base read.csv here
library(ggplot2)


test_data <- read.csv("Test Data for SIOP R Tutorial w Ethnicity w SAT.csv", header = TRUE)
 
# ... after exploring data

# we are setting up the canvas and define x and y axis
ggplot(test_data, aes(x=Gender, y=GPA)) 

#now that x and y axes are mapped we can add another layer
ggplot(test_data, aes(x=Gender, y=GPA)) +
  geom_point() 

# a better plot is here a box and whiskers plot
ggplot(test_data, aes(x=Gender, y=GPA)) +
  geom_boxplot()

# or we can do both
ggplot(test_data, aes(x=Gender, y=GPA)) +
  geom_boxplot() + 
  geom_point()

# there are many other geoms, some of which only make sense for certain types of data
# for categorical data by continuous data we could use a violin plot
ggplot(test_data, aes(x=Gender, y=GPA)) +
  geom_violin()


# let's assume we want to see the distribution of Height and Weight
head(test_data)
ggplot(test_data, aes(x=Height, y=Weight)) + 
  geom_point()

# difference between men and women
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender)) + 
  geom_point()


ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point()

ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender, size=Age)) + 
  geom_point()

# let's go back to geom point, but use argument to make it bigger
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) # the size argument will allow setting sizes

# other changes to the canvas
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + # the size argument will allow setting sizes
  coord_flip() #swaps (flips) the coordinates

# so far we are using default for colors and the canvas. ggplot makes a lot of
# assumptions which makes the quick design of a graph easy. The use can override 
# all of these assumptions
# changes to the axes
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) #+
  #xlim(0, max(test_data$Height, na.rm = TRUE) )

# manipulating the axes on a categorical variable 
ggplot(test_data, aes(x=Gender, y=GPA)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("M", "F"),
                   labels = c("Male", "Female"))

# customizing scale breaks for continuous axes
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75))

# Labels
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) 


# changes to the colors. So far we have been using default color palettes
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) +
  scale_colour_brewer(palette ="Dark2")

library(RColorBrewer)
display.brewer.all()

# changes to the colors. So far we have been using default color palettes
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) +
  scale_colour_manual(values = c("green", "#9e1c25"))

# to fill histograms use scale_fill_manual() and scale_fill_brewer() etc.

# Adding another layer: Lines of best fit


ggplot(test_data, aes(x=Height, y=Weight)) + 
  geom_point() +
  geom_smooth(method = lm)

ggplot(test_data, aes(x=Height, y=Weight)) + 
  geom_point() +
  geom_smooth(method = lm, level = 0.99)

ggplot(test_data, aes(x=Height, y=Weight)) + 
  geom_point() +
  geom_smooth(method = lm, se=FALSE)

ggplot(test_data, aes(x=Height, y=Weight)) + 
  geom_point() +
  geom_smooth(method = loess, se=FALSE)


# histograms =================================================================

ggplot(test_data, aes(x=GPA)) + 
  geom_histogram() 

ggplot(test_data, aes(x=GPA)) + 
  geom_histogram(binwidth = 0.05, fill="red", colour="black") 


ggplot(test_data, aes(x=GPA)) + 
  geom_histogram(binwidth = 0.05, fill="red", colour="black") + 
  geom_density()

#facets!
ggplot(test_data, aes(x=GPA)) + 
  geom_histogram(binwidth = 0.05, fill="red", colour="black") +
  facet_grid(Ethnicity ~ .)

ggplot(test_data, aes(x=GPA)) + 
  geom_histogram(binwidth = 0.05, fill="red", colour="black") +
  facet_grid(Gender ~ .)





# use of themes to change the appearance of the plot


#we are going back to one of the more customized plots we created early
#how about a title?
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) +
  scale_colour_manual(values = c("green", "#9e1c25"))+
  ggtitle("This is the plot title")



ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) +
  scale_colour_manual(values = c("green", "#9e1c25")) +
  ggtitle("This is the plot title")+
  #theme_classic()
  # theme_dark()
  theme_minimal()



ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) +
  scale_colour_manual(values = c("green", "#9e1c25")) +
  ggtitle("This is the plot title") + 
  theme(
    panel.grid.major.x = element_line(colour="red"), #we have to write a function that "draws" the lines
    panel.grid.minor.x = element_line(colour="blue"), #we have to write a function that "draws" the lines
    panel.grid.major.y = element_line(colour="green")
  )

#lets get rid of the ugly grey background
ggplot(test_data, aes(x=Height, y=Weight, shape=Gender, colour=Gender)) + 
  geom_point(size=3.5) + 
  ylim(0, max(test_data$Weight, na.rm = TRUE) ) +
  scale_x_continuous(breaks = c(65, 70, 71,  75)) +
  xlab("Height (inches)") +
  ylab("Weight (lbs)" ) +
  scale_colour_manual(values = c("green", "#9e1c25")) +
  ggtitle("This is the plot title") + 
  theme(
    panel.grid.major.x = element_line(colour="red"), #we have to write a function that "draws" the lines
    panel.background = element_blank(),
    plot.title = element_text(colour = "blue", hjust = 0.5)
    
  )
