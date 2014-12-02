########################################################################
#
#  plot4.R
#
#  Author: Regis Coaueret. 
#
#  Loads power consumption data from a household and 
#  plots multiple graphs of global active power, voltage, energy sub metering,
#  and global active power, over two days of the week
#
########################################################################

# ===============================================================
# Function to load the data set (with default values for all parameters)
# ===============================================================

get.data <- function(data.file='household_power_consumption.txt', 
                     source.url='http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip',
                     start.date='2007-02-01', end.date='2007-02-02') {
    
    # Check if data file is present, and download and extract if needed
    if(!file.exists(data.file)) {
        # Create temporary file, download .zip and unzip into the working directory
        temp <- tempfile()
        download.file(source.url, temp)
        unzip(temp)
        unlink(temp)
    }
    
    # Use data.table for optimized reading and subsetting of data
    require(data.table) # Will load the package if not present
    data.table <- fread(data.file, na.strings='?', colClasses=rep('character', 9))
    # 2075259 obs. of  9 variables
    
    # Subset the data based on start and end dates
    data.table <- data.table[(strptime(Date, format='%d/%m/%Y') >= as.POSIXct(start.date) 
                              & strptime(Date, format='%d/%m/%Y') <= as.POSIXct(end.date)), ]
    #str(data.table)
    #Classes ‘data.table’ and 'data.frame':    2880 obs. of  9 variables:
    #$ Date                 : chr  "1/2/2007" "1/2/2007" "1/2/2007" "1/2/2007" ...
    #$ Time                 : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
    #$ Global_active_power  : chr  "0.326" "0.326" "0.324" "0.324" ...
    #$ Global_reactive_power: chr  "0.128" "0.130" "0.132" "0.134" ...
    #$ Voltage              : chr  "243.150" "243.320" "243.510" "243.900" ...
    #$ Global_intensity     : chr  "1.400" "1.400" "1.400" "1.400" ...
    #$ Sub_metering_1       : chr  "0.000" "0.000" "0.000" "0.000" ...
    #$ Sub_metering_2       : chr  "0.000" "0.000" "0.000" "0.000" ...
    #$ Sub_metering_3       : chr  "0.000" "0.000" "0.000" "0.000" ...
    
    # Data Frame can now be used as data is much smaller
    data.frame <- as.data.frame(data.table)    
    #View(data.frame)
    
    # Conversion:
    # Transform Date and Time to POSIX format into a new column
    # All other columns are numeric
    data <- data.frame(
        DateTime=strptime(paste(data.frame$Date, data.frame$Time), 
                          format='%d/%m/%Y %H:%M:%S'), 
        apply(data.frame[, 3:9], 2, as.numeric))
    #View(data)
    return(data)
}

# set the working directory to where the data file should be
#setwd("/Users/regis/Documents/_rct_perso/courses/coursera/Data Science spec/4 Exploratory Data Analysis/workingdir/ExData_Plotting1")

# Get the data only if needed
if(!exists('loaded.data')) loaded.data <- get.data()

# Open the PNG graphic device with transparent background
png(file = "plot4.png", bg="transparent")

# Create the plot:

par(mfcol=c(2,2))

# same as plot2 except ylabel
plot(loaded.data$DateTime, loaded.data$Global_active_power, type="l",
     ylab = "Global Active Power", xlab="")
#same as plot3
plot(loaded.data$DateTime, loaded.data$Sub_metering_1, type="l",
     ylab = "Energy sub metering", xlab="")
lines(loaded.data$DateTime, loaded.data$Sub_metering_2, col="red")
lines(loaded.data$DateTime, loaded.data$Sub_metering_3, col="blue")
legend(x="topright", 
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("Black","Red","Blue"),
       lwd=1,
       bty="n")
# Voltage / DateTime
plot(loaded.data$DateTime, loaded.data$Voltage, type="l",
     ylab = "Voltage", xlab="datetime")
# Global_reactive_power / DateTime
plot(loaded.data$DateTime, loaded.data$Global_reactive_power, type="l",
     ylab = "Global_reactive_power", xlab="datetime")

#Close the graphic device.
dev.off()
