## Data Science Class 4 Assingment 1
# Exploring graphical data
# 
# This program will generate a set of test graphs to illustrate various techniques for presenting data in 
# a graphical fashion.
#
# Source this file then execute the function.  The plots will be saved to disk.
# Must provide a single parameter: data file location

c4w1_ass1 <- function(datafile){
    print("Reading data...")
    #Assume the data is in the current working directory    
    power <- read.csv(datafile,colClasses = c("character", "character", "character", "character", "character", "character", "character", "character","character"),sep=";",header=TRUE)
    
    print("Filtering out missing data...")
    ## remove ? data sets (checked all columns, any ? gives a row of ?):
    power2 <- power[power$Global_active_power!="?",]
    
    print("Converting date/time columns to a proper date object...")
    #Add dateTime column:
    power2$datetime <- strptime(paste(power2$Date,power2$Time, sep=" "),"%d/%m/%Y %H:%M:%S")
    
    print("Tranforming characters to numerics...")
    # convert characters to numeric
    power2 <- transform(power2,Global_active_power=as.numeric(Global_active_power),Global_reactive_power=as.numeric(Global_reactive_power),Voltage=as.numeric(Voltage),Global_intensity=as.numeric(Global_intensity),Sub_metering_1=as.numeric(Sub_metering_1),Sub_metering_2=as.numeric(Sub_metering_2),Sub_metering_3=as.numeric(Sub_metering_3))
    
    print("Isolating data set to desired month...")
    #isolate feb
    d1 <- strptime("01/02/07 00:00:00","%d/%m/%y %H:%M:%S")
    d2 <- strptime("01/03/07 00:00:00","%d/%m/%y %H:%M:%S")
    power3 <- power2[power2$datetime >= d1 & power2$datetime < d2,]
    
    
    print("Grabbing 2 days in February...")
    ## grab just 2 days
    d1 <- strptime("01/02/07 00:00:00","%d/%m/%y %H:%M:%S")
    d2 <- strptime("03/02/07 00:00:00","%d/%m/%y %H:%M:%S") 
    
    power4 <- power3[power3$datetime >= d1 & power3$datetime < d2,]
    
    print("Plotting graph 1 to PNG...")
    # plot 1
    png("plot1.png")
    hist(power4$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red")
    dev.off()
    
    print("Plotting graph 2 to PNG...")
    # plot2
    png("plot2.png")
    plot(power4$datetime,power4$Global_active_power, type="l", xlab="",ylab="Global Active Power (kilowatts)")
    dev.off()
    
    print("Plotting graph 3 to PNG...")
    # plot3
    png("plot3.png")
    plot(power4$datetime,power4$Sub_metering_1, type="n", ylab="Energy sub metering")
    lines(power4$datetime,power4$Sub_metering_1,col="black")
    lines(power4$datetime,power4$Sub_metering_2,col="red")
    lines(power4$datetime,power4$Sub_metering_3,col="blue")
    legend("topright",col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lwd ="1")
    dev.off()
    
    print("Plotting graph 4 to PNG...")
    # plot4
    png("plot4.png")
    par(mfcol=c(2,2))
    #get plot 1
    plot(power4$datetime,power4$Global_active_power, type="l", xlab="",ylab="Global Active Power (kilowatts)")
    #get plot 2
    plot(power4$datetime,power4$Sub_metering_1, type="n", ylab="Energy sub metering")
    lines(power4$datetime,power4$Sub_metering_1,col="black")
    lines(power4$datetime,power4$Sub_metering_2,col="red")
    lines(power4$datetime,power4$Sub_metering_3,col="blue")
    legend("topright",col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lwd ="1")
    
    # get plot 3
    plot(power4$datetime, power4$Voltage,type="l", ylab="Voltage", xlab="datetime")
    #get plot 4
    plot(power4$datetime, power4$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime")
    dev.off()
    
    print("Plotting complete. Have a nice day!")
}