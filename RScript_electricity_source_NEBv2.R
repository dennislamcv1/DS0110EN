# Fireside Analytics Inc.
# Instructor: Shingai Manjengwa (Twitter: @tjido)
# National Energy Board of Canada - NEB Open Data

###########################################################################
#                  Coding Notes and Instructions
###########################################################################
#                  Some notes before we start coding
###########################################################################

# 1.a. The ‘#’ precedes comments or notes, it tells R that this is not a command for execution, it is information for you, the learner.
# 1.b. Notes here are to assist you in the exercises. Answer the questions in the notes, your facilitator can help you.
# 1.c. Read every line, the notes as well as the code lines to follow the lesson

# 2.a. To run a command in R, place your cursor next to the line you wish to run and hit the ctrl+enter button (Windows) or command+enter (Mac)
# 2.b. You may also highlight the entire row or section you wish to run and click on the 'run' button in the toolbar.
# 2.c. Your cursor may be anywhere along the line you wish to run when you hit ctrl+enter or click on 'run'

# 3. R is case sensitive!! If you type a small letter where R expects a capital letter, the code will not run or it will produce an error.###########################################################################

###########################################################################

# when you run this script for the first time, you will need to install packages - these are predefined groups of code that perform common instructions
# once you have installed the packages, you may continue with your work until your next sessions
install.packages("ggplot2")
install.packages("dplyr")
install.packages("glue")

###########################################################################

# once you have installed the packages, load the libraries
library(ggplot2)
library(dplyr)
library(glue)

# before loading the data, check which working directory you are in to make it easy to access your files when you save or try to open them
getwd()

# see the file path below: this tells you the file path where RStudio will go if you click on 'File/Open'
# to change this file path to a different location e.g., your desktop, you may use the command
# setwd() and enter your desired file path
# you may also access this option by clicking on 'Session/Set Working Directory/Choose Directory' in your toolbar above

# loading electricity Generation File as a dataset (put your own file path inside the commas)
electricity_gen <- read.csv("2018-10-26_ElectricityGeneration excl all.csv")

# let's look at the data

# what are the overall dimensions of the data set?
dim(electricity_gen)
# these dimensions will also be visible in the top right-hand quadrant under 'Data'

# we may look at the first few lines
head(electricity_gen)

# we may look at the last few lines
tail(electricity_gen)

# what are the column names?
colnames(electricity_gen)

# what are the different 'provinces' in the data?
unique(electricity_gen$province)

# what are the different 'sources' in the data?
unique(electricity_gen$source)

# what are the different 'scenario' in the data?
unique(electricity_gen$scenario)

# how many years are in the data?
unique(electricity_gen$year)

# what are the different units in the data?
unique(electricity_gen$unit)
# we see GW.h, known as Gigawatt-hours or Gigawatt per hour
# a GW.h is a unit of energy representing one billion (1 000 000 000) watt-hours and is equivalent to one million kilowatt hours. 

###########################################################################
# ABOUT THE DATA SET
# The data set explores how possible energy futures might unfold for Canadians over the long term
# It considers four scenarios that explore how future prices and technological development might impact Canada’s energy system

# REFERENCE
# The Reference Case provides a baseline outlook with a moderate view of energy prices and economic growth, 
# and climate and energy policies announced at the time of analysis

# TECHNOLOGY
# The Technology Case considers higher carbon prices than the Reference Case and greater
# adoption of select emerging production and consumption energy technologies

# HIGH PRICE
# A price case with higher oil and natural gas prices captures some of the 
# uncertainty related to future energy prices

# LOW PRICE
# A price case with lower oil and natural gas prices captures some of the 
# uncertainty related to future energy prices

# Source: National Energy Board of Canada (NEB)
###########################################################################

# below, we will create subsets of the data using different scenarios
# we will begin our analysis using scenario = reference however, 
# you may run different scenario analyses for yourself

# creating a subset of the electricity_gen dataset where scenario = reference
electricity_total_ref <- electricity_gen[electricity_gen$scenario == 'reference', c("year", "value", "source")]
# creating a subset of the electricity_gen dataset where scenario = technology
electricity_total_tech <- electricity_gen[electricity_gen$scenario == 'technology', c("year", "value", "source")]
# creating a subset of the electricity_gen dataset where scenario = high
electricity_total_high <- electricity_gen[electricity_gen$scenario == 'high', c("year", "value", "source")]
# creating a subset of the electricity_gen dataset where scenario = low
electricity_total_low <- electricity_gen[electricity_gen$scenario == 'high', c("year", "value", "source")]
# plotting source vs Year data where size of dot depends on value

###########################################################################

# for scenario = reference
electricity_total_ref_1 <- ggplot(electricity_total_ref, aes(x = source, y = year)) + 
  geom_point(aes(colour = source, size = value)) + 
  labs(subtitle = "Source = Total, Scenario = Reference", 
       y = "Year", 
       x = "Source", 
       title = "Electricity Generation in Canada 1") 

plot(electricity_total_ref_1)

########################

# the x axis in our plot has overlapping words, let's fix it
# change the angle of the text and re-run the chart for yourself

# for scenario = reference
electricity_total_ref_2 <- ggplot(electricity_total_ref, aes(x = source, y = year)) + 
  geom_point(aes(colour = source, size = value)) + 
  labs(subtitle = "Source = Total, Scenario = Reference", 
       y = "Year", 
       x = "Source", 
       title = "Electricity Generation in Canada 2") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot(electricity_total_ref_2)

########################

# we can change the orientation of the chart and we can remove the source = total
# showing the 'total' source together with the other sources may eclipse smaller values

# for scenario = reference
electricity_total_ref_3 <- ggplot(electricity_total_ref, aes(x = source, y = year)) + 
  geom_point(aes(colour = source, size = value)) + 
  labs(subtitle = "Source = Total, Scenario = Reference", 
       y = "Year", 
       x = "Source", 
       title = "Electricity Generation in Canada 3") +
coord_flip()

plot(electricity_total_ref_3)

########################

# we will also remove the source = total
# showing the 'total' source together with the other sources may eclipse smaller values
# for scenario = reference

electricity_total_ref <- subset(electricity_total_ref, electricity_total_ref$source != "total")
electricity_total_ref_4 <- ggplot(electricity_total_ref, aes(x = source, y = year)) + 
  geom_point(aes(colour = source, size = value)) + 
  labs(subtitle = "Source = Total, Scenario = Reference", 
       y = "Year", 
       x = "Source", 
       title = "Electricity Generation in Canada 4") +
  coord_flip()

plot(electricity_total_ref_4)

# learn more about the 'subset' function by running the function preceded by a question mark '?'
# information about the function will appear in the bottom right-hand quadrant
?subset

########################

# what do we learn from the previous chart 'plot(electricity_total_ref_4)'?
# are the coal dots getting bigger or smaller over time? 
# what does the change in dot sizes mean?
# there are other data visualizations that help us quickly 'learn' more about the data

# this chart may be the a better way to represent the same information
electricity_total_ref <- subset(electricity_total_ref, electricity_total_ref$source != "total")
electricity_total_ref_5 <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_ref,
                          stat = "identity") + labs(title = "Electricity Generation in Canada 5", subtitle = "Province = All, Scenario = Reference")

plot(electricity_total_ref_5)

# immediately, we can see the overall upward trend and other details in the data

# if we need to, this is how we remove scientific notation from the axis 'options(scipen = 5)'

########################

# there are a number of ways to change the color scheme 
# let's change the colors using the 'themes' package
# note that when you use a package for the first time, you must install it and call the library
install.packages("ggthemes")
library(ggthemes)

# for scenario = reference and theme = theme_economist()
electricity_total_ref_6 <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_ref,
                                         stat = "identity") + labs(title = "Electricity Generation Canada 6", subtitle = "Province = All, Scenario = Reference")  + theme_economist()
plot(electricity_total_ref_6)

########################

# for scenario = reference and theme = theme_tufte()
electricity_total_ref_7 <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_ref,
                                               stat = "identity") + labs(title = "Electricity Generation Canada 7", subtitle = "Province = All, Scenario = Reference")  + theme_tufte()
plot(electricity_total_ref_7)

# try theme = theme_stata() and theme = theme_hc()

########################
# we may also use the RColorBrewer package
install.packages("RColorBrewer")
library(RColorBrewer)

# for scenario = reference and scale_fill_brewer(palette = "Set1")
electricity_total_ref_8 <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_ref,
                                               stat = "identity") + labs(title = "Electricity Generation Canada 8", subtitle = "Province = All, Scenario = Reference")  + scale_fill_brewer(palette = "Set1")
                                             
plot(electricity_total_ref_8)

########################
# for scenario = reference and scale_fill_brewer(palette = "Spectral")
electricity_total_ref_9 <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_ref,
                                               stat = "identity") + labs(title = "Electricity Generation Canada 9", subtitle = "Province = All, Scenario = Reference")  + scale_fill_brewer(palette = "Spectral")

plot(electricity_total_ref_9)

########################
# RColorBrewer has many palettes, here they are
par(mar = c(3,4,2,2))
display.brewer.all()
# you may zoom into the image to see the names of the colors

# try some other palettes
# for scenario = reference and scale_fill_brewer(palette = "Oranges")
electricity_total_ref_10 <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_ref,
                                               stat = "identity") + labs(title = "Electricity Generation Canada 10", subtitle = "Province = All, Scenario = Reference")  + scale_fill_brewer(palette = "Oranges")

plot(electricity_total_ref_10)

#####################
# as we analyse this data set with these visualizations, let's apply the Pyramid Principle
# the first insight we see is the upward trend in total electricity generation
# next, let's note any differences that stand out in the electricity sources

# what other notable trends can you observe?

#####################
# analyze the hydro trend

electricity_total_hydro <- subset(electricity_total_ref, electricity_total_ref$source == "hydro")
electricity_total_hydro <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_hydro,
                                           stat = "identity") + labs(title = "Electricity Generation Canada 10", subtitle = "Source = Hydro, Scenario = Reference")
plot(electricity_total_hydro)

#####################
# analyze the coal trend

electricity_total_coal <- subset(electricity_total_ref, electricity_total_ref$source == "coal")
electricity_total_coal <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_coal,
                                               stat = "identity") + labs(title = "Electricity Generation Canada 10", subtitle = "Source = Coal, Scenario = Reference")
plot(electricity_total_coal)

#####################
# analyze the solarWindGeoThermal trend

electricity_total_solarWindGeothermal <- subset(electricity_total_ref, electricity_total_ref$source == "solarWindGeothermal")
electricity_total_solarWindGeothermal <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_solarWindGeothermal,
                                           stat = "identity") + labs(title = "electricity Generation Canada 11", subtitle = "Source = SolarWindGeothermal, Scenario = Reference")
plot(electricity_total_solarWindGeothermal)

#####################
# we see distinct changes in both coal and solarWindGeothermal
# let's analyze the trends in the same visualization

electricity_total_contrast <- subset(electricity_total_ref, electricity_total_ref$source == "solarWindGeothermal" | electricity_total_ref$source == "coal")
electricity_total_contrast <- ggplot() + geom_bar(aes(y = value, x = year, fill = source), data = electricity_total_contrast,
                                                          stat = "identity") + labs(title = "electricity Generation Canada 12", subtitle = "Source = Coal & SolarWindGeoThermal, Scenario = Reference")
plot(electricity_total_contrast)

#####################
# DATA INSIGHTS
#####################

# from these visualizations, we learn that Canada's overall electricity generation is increasing
# we also learn that over time, Canada expects to replace coal with solarWindGeoThermal sources of electricity

#####################
# Congratulations, you have created visualizations in R using Open Data!
# See if you can find these visualizations in the data visualizations by the National Energy Board - Exploring Canada's Energy Future!
# https://www.neb-one.gc.ca/nrg/ntgrtd/ftr/index-eng.html

# Never stop learning!

###########################################################################

# Fireside Analytics Inc.
# Instructor: Shingai Manjengwa (Twitter: @tjido)
# National Energy Board of Canada - NEB Open Data
# Course: Data Science with Open Data - CognitiveClass.ai
