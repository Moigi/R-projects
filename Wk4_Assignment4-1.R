#Step 1: Find path of data file and copy path (if using windows)
#Step 2: In the console below, type readClipboard()
#Step 3: Copy and paste R's path to the line below in quotes

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT FILE DIRECTORY OF DATA:
myDataLocation <- "/Users/mfager/Dropbox/Adjunct-NU/BAN600/Assignments/Week Four"

#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
setwd(myDataLocation)

#IMPORT DATA AND PUT INTO DATAFRAME
myData <- read.csv(file = "student-mat.csv", header = TRUE)

#-------------------------------------------------------------------------

## Load and install required packages
library(mosaic)
# Any other packages?

