quartz<-function(width,height){windows(width, height)}
library(wholebrain)

folder="C:/Users/Mangana/Dropbox (Dumitriu Lab)/Dumitriu Lab Team Folder/Alessia/Rinki/Brain1/"
#Load dataset and regi (THIS WILL OVERWRITE ANY EXISTING regi OR dataset NAMED OBJECTS)
setwd(folder)
Rdatafiles<-list.files(pattern = "*.Rdata")
length(Rdatafiles)

load(file=Rdatafiles[1])

##check regi$outputfile to see where your undistorted/distorted images are, and change it
regi$outputfile<-paste0(folder,"output_",tools::file_path_sans_ext(basename(c)),"/",basename(regi$outputfile))

##tryout the schematic plot function
schematic.plot(dataset)
##make schematic plot without, title, without millimeter grid and with a 1 mm scale bar.
schematic.plot(dataset, title = FALSE, mm.grid = FALSE, scale.bar = TRUE)

##tryout the plot.registration() function
plot.registration(regi)
##change outline colors to orange and also draw transformaition grid in purple
par(bg='black')
plot.registration(regi, border = 'orange', draw.trans.grid = TRUE, grid.color = 'purple')

##plot the outlines in tissue space
plot.outlines(regi, plot = TRUE)
##add the neurons
points(dataset$x, dataset$y, pch = 16, col = dataset$color)

##make a plot where we use the intensity of each cell to make heat color ramp on log2 scale
color<-heat.colors(100)[as.numeric(cut(log2(dataset$intensity), breaks = 100))]
plot.outlines(regi, plot = TRUE)
points(dataset$x, dataset$y, pch = 16, col = color)

#plot the cells in ROI i.e. thalamus
points(TH$x, TH$y, pch = 20, col = TH$color)

#plot the original atlas plate outline in real scale for right and left hemisphere

for (d in 1:96) {
  lapply(d, function(x){
    polygon(regi$atlas$outlines[[x]]$xrT,regi$atlas$outlines[[x]]$yrT, border = "red")})
}

for (d in 1:96) {
  lapply(d, function(x){
    polygon(regi$atlas$outlines[[x]]$xlT,regi$atlas$outlines[[x]]$ylT, border = "red")})
}
