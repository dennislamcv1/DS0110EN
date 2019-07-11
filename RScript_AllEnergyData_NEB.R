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

# 3. R is case sensitive!! If you type a small letter where R expects a capital letter, the code will not run or it will produce an error.
# 4. You will enter instructions in this quadrant (top left), and you will see them and their output in the bottom left quadrant below.
# 5. Ctrl+Enter runs a line of the script. You will hit ‘Ctrl+Enter’ along each of the lines in this script to follow the lesson.
# 6. Feel free to experiment, change some of the scripts and see what happens.
# 7. You can always go back to (download) the original file.
# 8.a. In the toolbar above, the page icon with a green circle and a white cross in it will open a new ‘sheet’ for you.
# 8.b. You can copy paste code from this R file into a new sheet, a new .R file. Remember to save changes if you wish to access that new file again.

# 9.a. If you need help in R at any time, type in, “?” + “the thing you need help with”, as a command then Ctrl+Enter to ‘run’ it as an instruction to R
# 9.b. Try using ‘help’ in R with the ‘head’ command below. The results will appear in the bottom right-hand quadrant.
?head

# 10. The bottom left quadrant can get very busy if you want to clear it, type Ctrl+L. Your session will still be active.
# 11.a. Plots appear in the bottom right-hand quadrant. Make sure you are in the ‘Plots’ tab to see your charts.
# 11.b. If you do not see 4 quadrants when you start in R, you can click on ‘View’ in the toolbar, and ‘Panes’, ‘Show all panes’
# 11.c. If you do not see 4 quadrants when you start in R, you can also hit Ctrl+Shift+0

# 12. Lastly, is it “data is” or “data are”? we don’t mind what you use. Happy learning!

###########################################################################
# when you run this script for the first time, you will need to install packages - these's are predefined groups of code that perform common instructions
# once you have installed the packages, you may continue with your work until your next sessions
install.packages("ggplot2")
install.packages("dplyr")
install.packages("glue")
install.packages("magrittr")

###########################################################################

# once you have installed the packages, you must load the libraries
library(ggplot2)
library(dplyr)
library(glue)
library(magrittr)

# check the working directory - this is the default location that files will be saved to and opened from
getwd()

# you may set the working directory by using the 'setwd' command e.g., setwd("/resources/rstudio")

# load the data files
# the files are Comma Separated Values (CSV) 
# we load each .csv file into a an object we've called dfX i.e., df1, df2, df3, df4 etc. 
df1 <- read.csv('2018-10-16_NaturalGasProduction.csv')
df2 <- read.csv('2018-10-26_ElectricityGeneration.csv')
df3 <- read.csv('2018-10-26_EnergyDemand.csv')
df4 <- read.csv('2018-10-31_CrudeOilProduction.csv')

# let's look at one of the objects we've created, we can see the first x lines of the csv. file using the 'head' command
head(df1,10)

# you may simply type the name of the object to look at more rows of data e.g., df1
# the process of creating charts/plots in R is a very structured process
# we create or modify R code instructions in the RScript file - read each of the following code lines and note the differences

# default plots that come with R
plot(df1$year,df1$value, col = 2)
# in the chart above, we are plotting the year and value from the df1 object

# change the color number above and see what happens e.g., col = 3

# advanced visualizations with ggplot2. Notice the function changed from 'plot' to 'qplot'
Plot2 <- qplot(df1$year, df1$value)
Plot2

Plot3 <- qplot(df1$year, df1$value, color = "red")
Plot3

# add axis labels and a heading
Plot4 <- qplot(df1$year, df1$value, color = "red", main = "Heading", xlab = "x-axis label", ylab = "y-axis label")
Plot4

# ggplot2 uses geometric modifiers or 'geoms' to alter the chart object, rather than a long list of specifications
Plot5 <- qplot(df1$year, df1$value, main = "Heading", xlab = "x-axis label", ylab = "y-axis label", geom = "blank") + geom_point(size = 3, shape = 8, color = "blue")
Plot5

# use the ggplot2 package to plot the NaturalGasProduction values per year for each province
# each different scenario will have a different color
# in addition, save the plot to file under the name NaturalGasProduction.jpeg 
# we may generate multiple plots and then import them all into Powerpoint for further analysis and presentation

df1 %>% 
  ggplot(aes(year,value,group=scenario)) + 
  geom_line(size=0.8,aes(color=factor(scenario))) + 
  facet_wrap(~province,scales='free') + 
  theme_bw() + 
  labs(title='Natural Gas Production')+
  theme(strip.text.y = element_text(angle = 0,size=12),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=12), 
        axis.title.y=element_text(size=12), 
        plot.title=element_text(size=12)
  ) +
  ggsave(filename = 'NaturalGasProduction.jpeg',width = 16, height = 9)

# use ggplot2 package to plot the ElectricityGeneration values per year for each province
# different scenarios will have different colors
# for this data, we have a 3rd dimension, source.
# in order to add it, we use a 'for loop' and will create the same plot as above (NaturalGasProduction)
# each time with different source (i.e. bio, coal, hydro etc)
# in addition, save the plot to file under the name NaturalGasProduction.jpeg

# get list of unique sources
sources <- unique(df2$source)

for(s in sources){
  
  cat(glue('creating plot for source {s}'),'\n')
  
  # subset the data for specific source
  df <- subset(df2,source==s)
  df %>% 
    ggplot(aes(year,value,group=scenario)) + 
    geom_line(size=0.8,aes(color=factor(scenario))) + 
    facet_wrap(~province,scales='free') + 
    theme_bw() + 
    labs(title='Electricity Generation',subtitle = glue('source={s}'))+
    theme(strip.text.y = element_text(angle = 0,size=12),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.text=element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=12), 
          axis.title.y=element_text(size=12), 
          plot.title=element_text(size=12)
    ) +
    ggsave(filename = glue('ElectricityGeneration_source_{s}.jpeg'),width = 16, height = 9)
  
}

# use the ggplot2 package to plot the EnergyDemand values per year for each province
# different scenarios will have different colors
# for this data, we have another 2 dimensions, source and sector
# in order to add it, we use a 'for loop' and will create the same plot as above (ElectricityGeneration)
# each time with different source and sector (i.e. bio+commercial etc)
# in addition, save the plot to file under the name NaturalGasProduction.jpeg


# get list of unique sources
sources <- unique(df3$source)
sectors <- unique(df3$sector)

for(s in sources){
  
  for(sec in sectors){
    
    cat(glue('creating plot for source={s} and sector={sec}'),'\n')
    
    # subset the data for specific source
    df <- subset(df3,source==s & sector==sec)
    
    # make sure there is actual data for each combination.
    if(nrow(df)!=0){
      
      df %>% 
        ggplot(aes(year,value,group=scenario)) + 
        geom_line(size=0.8,aes(color=factor(scenario))) + 
        facet_wrap(~province,scales='free') + 
        theme_bw() + 
        labs(title='Energy Demand',subtitle = glue('source={s}, sector={sec}'))+
        theme(strip.text.y = element_text(angle = 0,size=12),
              legend.position = 'top',
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.x=element_text(size=12), 
              axis.title.y=element_text(size=12), 
              plot.title=element_text(size=12)
        ) +
        ggsave(filename = glue('EnergyDemand_source_{s}_sector_{sec}.jpeg'),width = 16, height = 9)
      
    }
    
    
  }
  
}




# use the ggplot2 package to plot the CrudeOilProduction values per year for each province
# different scenarios will have different colors
# in addition, save the plot to file under the name NaturalGasProduction.jpeg
df4 %>% 
  ggplot(aes(year,value,group=scenario)) + 
  geom_line(size=0.8,aes(color=factor(scenario))) + 
  facet_wrap(~province,scales='free') + 
  theme_bw() + 
  labs(title='Crude Oil Production')+
  theme(strip.text.y = element_text(angle = 0,size=12),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=12), 
        axis.title.y=element_text(size=12), 
        plot.title=element_text(size=12)
  )+
  ggsave(filename = 'CrudeOilProduction.jpeg',width = 16, height = 9)


#Congratulations, you have created your first visualizations in R using Open Data!
#Never stop learning!

###########################################################################

# Fireside Analytics Inc.
# Instructor: Shingai Manjengwa (Twitter: @tjido)
# National Energy Board of Canada - NEB Open Data
# Course: Data Science with Open Data - CognitiveClass.ai
