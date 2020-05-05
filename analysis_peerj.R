library(rvest)
PeerJField<-NULL
PeerJEditor<-NULL
PeerJPaper<-NULL
Author_country<-NULL
Paper_time<-NULL

## collecting data
Paper_keyword<-list()
for (i in 1:350) {
  url<-paste("https://peerj.com/articles/?journal=peerj&discipline=biology&q=&page=",i,sep = "")
  myPeerj<-read_html(url)
  peerjfield<-myPeerj%>%
    html_nodes(css = ".main-search-item-subjects")%>%
    html_text()
  peerjeditor<-myPeerj%>%
    html_nodes(css = ".main-search-item-editor .search-item-show-link")%>%
    html_text()
  peerjpaper<-myPeerj%>%
    html_nodes(css = "#wrap .span5")%>%
    html_text()
  peerjeditor<-gsub(pattern = "E\n",replacement = "",peerjeditor)
  peerjeditor<-gsub(pattern = "\n",replacement = "",peerjeditor)
  peerjeditor<-gsub(pattern = " ",replacement = "",peerjeditor)
  peerjfield<-gsub(pattern = "\\[",replacement = "",peerjfield)
  peerjfield<-gsub(pattern = "\\]",replacement = "",peerjfield)
  peerjfield<-gsub(pattern = "\"",replacement = "",peerjfield,fixed = TRUE)
  peerjpaper<-gsub(pattern = "\n",replacement = "",peerjpaper)
  peerjpaper<-gsub(pattern = " ",replacement = "",peerjpaper)
  peerjpaper<-gsub(pattern = ":[0-9]+.[0-9]+",replacement = "",peerjpaper)
  peerjpaper<-gsub(pattern = "[A-Za-z]+/peerj.",replacement = "",peerjpaper)
  peerjpaper<-gsub(pattern = "[A-Za-z]+-",replacement = "",peerjpaper)
  peerjpaper<-gsub(pattern = "[A-Za-z]+.",replacement = "",peerjpaper)
  peerjpaper<-gsub(pattern = "[\u00C0-\u00FF]+",replacement = "",peerjpaper)
  PeerJField<-c(PeerJField,peerjfield)
  PeerJEditor<-c(PeerJEditor,peerjeditor)
  PeerJPaper<-c(PeerJPaper,peerjpaper)
}
for (j in 1:length(PeerJPaper)) {
  url2<-paste("https://peerj.com/articles/",PeerJPaper[j],sep = "")
  myPeerj<-read_html(url2)
  author<-myPeerj%>%
    html_nodes(css = ".country")%>%
    html_text()
  time<-myPeerj%>%
    html_nodes(css = "#article-information time")%>%
    html_text()
  keywords<-myPeerj%>%
    html_nodes(css = ".kwd")%>%
    html_text()
  keywords<-gsub(pattern = "  ",replacement = "",keywords)
  keywords<-gsub(pattern = "\n",replacement = "",keywords)
  Paper_keyword[[j]]<-keywords
  Author_country<-c(Author_country,author)
  Paper_time<-rbind(Paper_time,time)
}
TotalPaperdata<-data.frame(Field=PeerJField,editor=PeerJEditor,Accepted=Paper_time[,2],Received=Paper_time[,3])
PeerJField_list<-strsplit(PeerJField,",")

## Review time analysis
BIEditor_list<-list()
i<-1
while (i<=nrow(TotalPaperdata)) {
  field_editor<-strsplit(as.character(TotalPaperdata[i,1]),",")[[1]]
  if(length(intersect(c("Bioinformatics","Data Science"),field_editor))>=1){
    BIEditor_list[[as.character(TotalPaperdata[i,2])]]<-c(BIEditor_list[[as.character(TotalPaperdata[i,2])]],
                                                          as.numeric(as.Date(TotalPaperdata[i,3])-as.Date(TotalPaperdata[i,4])))}
  i=i+1
}
plotBIEditor_list<-list()
i<-1
while (i<=length(BIEditor_list)) {
  if(length(BIEditor_list[[i]])>=3){
    plotBIEditor_list<-c(plotBIEditor_list,BIEditor_list[i])
  }
  i=i+1
}
BIEditor_name<-NULL
BIEditor_time<-NULL
BIEditor_checkpaper<-data.frame()
for (i in 1:length(plotBIEditor_list)) {
  BIEditor_name<-names(plotBIEditor_list[i])
  for(j in 1:length(plotBIEditor_list[[i]])){
    BIEditor_time<-data.frame(name=BIEditor_name,time=plotBIEditor_list[[i]][j])
    BIEditor_checkpaper<-rbind(BIEditor_checkpaper,BIEditor_time)
  }
}
library(ggplot2)
p<-ggplot(data = BIEditor_checkpaper,mapping = aes(x=BIEditor_checkpaper$name,y=BIEditor_checkpaper$time,fill=factor(BIEditor_checkpaper$name)),)+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 75, hjust = 0.5, vjust = 0.5,size = 8))+
  guides(fill=FALSE)+
  labs(title="Edit review time in BI&DM article",x=NULL,y=NULL)+
  theme(text = element_text(color = "Black"),
        plot.title = element_text(size=15,color = "Black"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## Research direction related network analysis
Networkfield<-NULL
i<-1
while (i<=length(PeerJField_list)) {
  netfield<-NULL
  for (j in 1:(length(PeerJField_list[[i]])-1)) {
    for (k in (j+1):length(PeerJField_list[[i]])) {
      field<-c(as.character(PeerJField_list[[i]][j]),as.character(PeerJField_list[[i]][k]))
      netfield<-rbind(netfield,field)
    }
  }
  Networkfield<-rbind(Networkfield,netfield)
  i=i+1
}
Networkfield<-data.frame(Networkfield)
library(networkD3)
library(DMwR)
networkfield<-na.omit(Networkfield)
simpleNetwork(networkfield[1:2100,],fontSize = 14,nodeColour = "red",
              linkDistance=300,zoom = T)
simpleNetwork(networkfield)

## Keyword Analysis
BIkeyword_list<-list()
i<-1
NewTotalPaperdata<-data.frame()
while (i<=nrow(TotalPaperdata)) {
  if(as.numeric(as.Date(TotalPaperdata[i,4])-as.Date("2016-01-01"))>=0){
    NewTotalPaperdata<-rbind(NewTotalPaperdata,TotalPaperdata[i,])
  }
  i=i+1
}
i<-1
while (i<=length(Paper_keyword)) {
  field_editor<-strsplit(as.character(NewTotalPaperdata[i,1]),",")[[1]]
  if(length(intersect(c("Bioinformatics","Data Science"),field_editor))>=1){
    BIkeyword_list<-c(BIkeyword_list,Paper_keyword[i])}
  else{BIkeyword_list<-BIkeyword_list}
  i=i+1
}
BIkeywd_list<-list()
BIkeyword_table<-NULL
for (i in 1:length(BIkeyword_list)) {
  keyword<-BIkeyword_list[[i]]
  for (j in 1:length(keyword)) {
    keywd<-strsplit(keyword[j]," ")[[1]]
    for (k in 1:length(keywd)) {
      BIkeywd_list[[keywd[k]]]<-c(BIkeywd_list[[keywd[k]]],1)
    }
  }
}
BIkeywd<-names(BIkeywd_list)
BIkeywdN<-NULL
for (i in 1:length(BIkeywd)) {
  BIkeywdN<-c(BIkeywdN,sum(BIkeywd_list[[BIkeywd[i]]]))
}
BIkeyword_table<-data.frame(keyword=BIkeywd,number=BIkeywdN)
plotBIkeyword_table<-BIkeyword_table[BIkeyword_table$number>3,]
library(wordcloud2)
wordcloud2(plotBIkeyword_table,size = 1,shape = 'dimond')




## Volume Analysis
TotalField<-data.frame()
totalfield_list<-list()
i<-1
while (i<=nrow(TotalPaperdata)) {
  field<-strsplit(as.character(TotalPaperdata[i,1]),",")[[1]]
  for (j in 1:length(field)) {
    fd<-field[j]
    totalfield_list[[fd]]<-c(totalfield_list[[fd]],1)
  }
  i=i+1
}
field_name<-names(totalfield_list)
field_number<-NULL
for (i in 1:length(totalfield_list)) {
  field_number<-c(field_number,sum(totalfield_list[[i]]))
}
TotalField<-data.frame(field=field_name,number=field_number)
plotField<-TotalField[TotalField$number>4,]
library(ggplot2)
pic<-ggplot(data = plotField,mapping = aes(x=field,y=number,fill=field),)+
  geom_bar(stat = "identity",width = 1)+
  guides(fill=FALSE)+coord_flip()+
  labs(x=NULL,y=NULL)+
  theme(text = element_text(color = "Black"),
        axis.text.x=element_text(face="bold",size=10,angle=0,color="Black"),
        axis.text.y=element_text(face="bold",size=5,angle=0,color="Black"),
        plot.title = element_text(size=10,color = "Black"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Author nationality 
author_country<-Author_country
for(j in 1:length(author_country)){
  if(grepl(pattern = "China",author_country[j])){
    author_country[j]<-"China"
  }
  else if(grepl(pattern = "Taiwan",author_country[j])){
    author_country[j]<-"China"
  }
  else if(grepl(pattern = "America",author_country[j])){
    author_country[j]<-"USA"
  }
  else if(grepl(pattern = "United States",author_country[j])){
    author_country[j]<-"USA"
  }
  else if(grepl(pattern = "United Kingdom",author_country[j])){
    author_country[j]<-"UK"
  }
  else if(grepl(pattern = "Uk",author_country[j])){
    author_country[j]<-"UK"
  }
}
author_country<-gsub(pattern = " ",replacement = "",author_country)
authorcontry_list<-list()
i<-1
while (i<=length(author_country)) {
  country<-author_country[i]
  authorcontry_list[[country]]<-c(authorcontry_list[[country]],1)
  i=i+1
}
Nation<-names(authorcontry_list)
AUTHor<-NULL
for (j in 1:length(authorcontry_list)) {
  AUTHor<-c(AUTHor,sum(authorcontry_list[[j]]))
}
Author_data<-data.frame(national=Nation,author_number=AUTHor)
plotAuthor_data<-Author_data[Author_data$author_number>30,]
library(ggplot2)
pic<-ggplot(data = plotAuthor_data,mapping = aes(x=national,y=author_number,fill=national),)+
  geom_bar(stat = "identity",width = 0.9,)+
  guides(fill=FALSE)+coord_flip()+
  labs(x=NULL,y=NULL)+
  theme(text = element_text(color = "Black"),
        axis.text.x=element_text(face="bold",size=10,angle=0,color="Black"),
        axis.text.y=element_text(face="bold",size=8,angle=0,color="Black"),
        plot.title = element_text(size=10,color = "Black"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#平均每篇文章的作者数量
PeerJAuCu<-data.frame()
for(i in 1:length(PeerJPaper)){
  print(i)
  url3<-paste("https://peerj.com/articles/",PeerJPaper[i],sep = "")
  mypj<-read_html(url3)
  peerjaucu<-mypj%>%
    html_nodes(css = ".article-authors , .country")%>%
    html_text()
  peerjaucu<-peerjaucu[1:2]
  peerjcu<-peerjaucu[2]
  if(grepl(pattern = "China",peerjcu)){
    peerjcu<-"China"
  }else if(grepl(pattern = "P. R. China",peerjcu)){
    peerjcu<-"China"}else if(grepl(pattern = "America",peerjcu)){
      peerjcu<-"USA"}else if(grepl(pattern = "United States",peerjcu)){peerjcu<-"USA"}else if(grepl(pattern = "United Kingdom",peerjcu)){
        peerjcu<-"UK"}else if(grepl(pattern = "Uk",peerjcu)){
          peerjcu<-"UK"}
  peerjau<-strsplit(peerjaucu[1],", ")[[1]]
  peerjau<-gsub(pattern = "[0-9]+",replacement = "",peerjau)
  peerjau<-gsub(pattern = ",",replacement = "",peerjau)
  peerjau<-gsub(pattern = "\n",replacement = "",peerjau)
  peerjaunumber<-length(peerjau)
  peerj_aucu<-data.frame(Author_number=peerjaunumber,national=peerjcu)
  PeerJAuCu<-rbind(PeerJAuCu,peerj_aucu)
}
PeerJ_AuCu<-PeerJAuCu[PeerJAuCu$national %in% 
                        c("China","Japan","USA","Australia","Russia","UK","Germany","Finland","India",
                          "Italy","Canada","Hungary","France"),]
library(ggplot2)
p<-ggplot(data = PeerJ_AuCu,mapping = aes(x=national,y=Author_number,))+
  geom_boxplot(aes(fill=factor(national)),width=0.5)+guides(fill=FALSE)+
  theme(text = element_text(color = "Black"),
        axis.text.x=element_text(size=10,angle=0,color="Black"),
        axis.text.y=element_text(size=10,angle=0,color="Black"),
        plot.title = element_text(size=10,color = "Black"))



