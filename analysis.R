#Analysis and data plot

install.packages("dplyr")


library(dplyr)
library(tidyr)
library(ggplot2)
library(colorspace)
library(data.table)


#converting your data in a data table;set row.names and col.names to TRUE if the names in the original csv are correct
write.table(data, file = "data.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
#rename columns
names(data)<- c("area","frequency")
# Rename a column if you know the old name/or the nr
colnames(data)[colnames(data)=="old_name"] <- "new_name"
names(data)[3]<-"new_name"

#creation of master.dataset and removing NA fields
master.data<-rbind(dataset1,dataset2,dataset3)
master.data <- master.data[!(master.data$color == '#000000'),]
#Creat group col for both phenothype and exp and populate with the correct value
master.data$group<- NA
master.data$group[master.data$animal=="D9"]<-"def"
master.data$exp<- NA
master.data$exp[master.data$animal=="D9"]<-"ESARE"
#if one specific subset of the dataset is NA and you want to replace with a value
master.data$animal[is.na(master.data$animal)] <- "D2"

#to remove duplicate from a data.table(not dataset)
data<-unique(data, by = "colname")
#to order alphabetically the brain area
data<-data[order(data$acronym),]

#Create a table of coord for each section if you didn't segment so some dataset are missing
table.coord<-table(1:length(Rdatafiles))
table.coord<-rbind(table.coord, Rdatafiles)
ii=0
for (i in Rdatafiles[1:length(Rdatafiles)]) {load(i); ii=ii+1; table.coord[1,ii]<-regi$coordinate}
table.coord<-data.table(table.coord)
table.coord<-transpose(table.coord)
colnames(table.coord)<-c("Coord","slice")

#ROI plotting for animal and group (ex-> BLA and TH)
dataplot<- roi.cell.count(master.data, rois = c('BLAa','TH'))
cell_count_group1$animal->id #copy the column with animal name from whatever dataset
cell_count_group1$group->group #copy the column with animal group from whatever dataset
dataplot<-add.group(dataplot,subjectID =id, group =group)
df1<-data.summary(dataplot, varname='cell.count', groupnames=c('acronym','group'))
df1<-rename(df1, c('acronym' = 'region'))
p <- ggplot(df1, aes(x=region, y=cell.count, fill=group)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=cell.count-err, ymax=cell.count+err), width=.2, position=position_dodge(.9))
p + scale_fill_brewer(palette="Paired") + theme_classic()


#subset your data with grep(i.e take only data from animal CTR in dataplot)
control_test <- grep("CTR", dataplot$group)


###for heat maps
library(heatmap.plus)
library(RColorBrewer)
#first function list of colours for condition
condition_colors<- unlist(lapply(dataplot$group, function(x){
  if(grepl("CTR", x)) '#E74C3C'
  else if(grepl("DEF",x)) '#2980B9'
  
  }))

for (i in list_seg[1:length(list_seg)]) {
  seg_somax<-append()
}


###cell_count rerun segmentation and load previous regi
setwd(folder)
i=62
for (c in filename[89:114]) {
  #rm(seg, regi)
  i=i+1
  load(file=Rdatafiles[i])
  seg<-segment(c)
  plot(seg$soma$x, seg$soma$y, ylim=rev(range(seg$soma$y)), asp=1)
  #save output files
  regi$outputfile<-paste0(folder,"output_",tools::file_path_sans_ext(basename(c)),"/",basename(regi$outputfile))
  dataset<-inspect.registration(regi, seg, soma = TRUE, forward.warps = TRUE, batch.mode = TRUE)
  #dev.copy(pdf, paste0(tools::file_path_sans_ext(basename(c)), '.pdf'))
  #save(file= paste0(tools::file_path_sans_ext(basename(c)), '.Rdata'), seg, regi, dataset,datasets)
  if (exists('dataset_VTA')) {
    print("it exist!")
    dataset_VTA<-rbind(dataset_VTA, dataset)
  }else{
    dataset_VTA<- dataset
  }
  dev.off()
}
##i.e.VTA study
cell.count.VTA <- table(dataset_VTA$acronym)
cell.count.VTA <- sort(cell.count.VTA)
write.table(dataset_VTA, file='soma_count_VTA.csv', sep=',', row.names =FALSE)

for (i in master.counted_sum$region) {
  master.counted_sum$parent[grepl(i, master.counted_sum$region)]<- atlasOntology$parent[atlasOntology$acronym==i]
}

