#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if(!require(data.table))
    install.packages("data.table")
library(data.table)

if(!require(dplyr))
    install.packages("dplyr")
library(dplyr)

if(!require(tidyr))
    install.packages("tidyr")
library(tidyr)

if(!require(stringi))
    install.packages("stringi")
library(stringi)

if(!require(ggplot2))
    install.packages("ggplot2")
library(ggplot2)

if(!require(ggraph))
    install.packages("ggraph")
library(ggraph)

if(!require(tidygraph))
    install.packages("tidygraph")
library(tidygraph)

if(!require(rtweet))
    install.packages("rtweet")
library(rtweet)

if(!require(leaflet))
    install.packages("leaflet")
library(leaflet)

if(!require(leafletCN))
    install.packages("leafletCN")
library(leafletCN)

if(!require(maptools))
    install.packages("maptools")
library(maptools)

if(!require(sf))
    install.packages("sf")
library(sf)

if(!require(rsconnect))
    install.packages("rsconnect")
library(rsconnect)

if(!require(dygraphs))
    install.packages("dygraphs")
library(dygraphs)

if(!require(RMeCab))
    install.packages("RMeCab", repos = "https://rmecab.jp/R") 
library(RMeCab)

write.csv(emojis,"Emojis.csv",row.names = F)
ED <-
    fread("Emojis.csv") %>%
    rename(Unicode=code) %>%
    mutate(Uni=gsub("U\\+","U\\\\+",Unicode)) %>%
    left_join(emojis) %>%
    filter(grepl("<U",Unicode))

ggColorHue <- function(n, l=65) {
    hues <- seq(15, 375, length=n+1)
    hcl(h=hues, l=l, c=100)[1:n]
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("ツイート数の推移"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("wd",
                      "抽出する単語",
                      "雨"),
            actionButton("button1","抽出開始")
            # submitButton()
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            column(6,dygraphOutput("Mdy"),
                   dygraphOutput("Hdy")),
                   # plotOutput("Dline0")),
            column(6,plotOutput("ggraph",height = "800px")),
            # column(6,plotOutput("Hline")),
            # plotOutput("Dline"),
            # plotOutput("Mline"),
            width = 12
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    refreshPlot0 <- reactiveTimer(intervalMs = 1)
    # refreshPlot <- reactiveTimer(intervalMs = 60000)
    
    TDC <- data.frame()
    dc=""
    wd="コロナ"
    wd="雨"
    wd="雨 コロナ"
    wd="大雨"
    wd="神奈川　コロナ"
    
    WD <- eventReactive(input$button1,{
        input$wd
    })
    observeEvent(input$button1, {
        if(file.exists("TDC.csv"))
            file.remove("TDC.csv")
        if(file.exists("dc.txt"))
            file.remove("dc.txt")
    })
    
    observe({
        wd=WD()
        refreshPlot0()
        if(file.exists("TDC.csv"))
            TDC <- fread("TDC.csv") %>%
            data.frame()
        if(file.exists("dc.txt"))
            dc <- as.character(fread("dc.txt")$V1)
        # print(Sys.time())
        if(as.numeric(format(Sys.time(),"%S"))>5)
            return()
        print(Sys.time())
        # refreshPlot()
        td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)#,retryonratelimit = T)
        
        if(nrow(td)==0)
            return()
        
        tds <-
            td %>%
            mutate(JTime=as.POSIXct(format(created_at, tz="Japan"))) %>%
            mutate(Tweet=gsub("　","",text)) %>%
            mutate(Tweet=stri_trans_nfkc(Tweet)) %>%
            # mutate(Tweet=gsub("\"","",Tweet)) %>%
            mutate(Tweet=gsub("http[[:print:]]{,18}","",Tweet)) %>%
            mutate(Tweet=gsub("@[[:alnum:][:punct:]]+","",Tweet)) %>%
            # mutate(Tweet=gsub("<U\\+0001","<U\\+1",Tweet)) %>%
            mutate(Tweet=gsub("!+","!",Tweet)) %>%
            mutate(Tweet=gsub("\\?+","\\?",Tweet)) %>%
            mutate(Tweet=gsub("-+","-",Tweet)) %>%
            mutate(Tweet=gsub("\\(+","\\(",Tweet)) %>%
            mutate(Tweet=gsub(")+",")",Tweet)) %>%
            mutate(Tweet=gsub("\"+","",Tweet)) %>%
            mutate(Year=year(JTime)) %>%
            mutate(Month=month(JTime)) %>%
            mutate(Day=mday(JTime)) %>%
            mutate(Hour=hour(JTime)) %>%
            mutate(Minute=minute(JTime)) %>%
            mutate(M=floor(Minute/10)*10) %>%
            arrange(desc(status_id)) %>%
            mutate(ID=paste0("Row",1:n())) %>% 
            # mutate(Tweet2=iconv(Tweet,from="UTF-8",to="CP932","")) %>%
            data.frame()
        
        day=format(max(tds$JTime),"%Y%m%d_%H%M") #-24*60*60
        mid=max(tds$status_id)
        print(paste(min(tds$JTime),max(tds$JTime),nrow(tds)))
        
        write_as_csv(tds,paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"),fileEncoding = "CP932")
        
        # te <-
        #     tds%>%
        #     distinct(status_id,text,Tweet)
        
        # print(length(dc))
        # print(nrow(tds %>% filter(!status_id %in% dc)))
        
        tdc <-
            tds %>%
            filter(!status_id %in% dc) %>%
            count(Year,Month,Day,Hour,Minute,RT=is_retweet) %>%
            mutate(RT=as.logical(RT)) %>%
            complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
            # mutate(M=floor(Minute/10)*10) %>%
            group_by(Year,Month,Day,Hour,Minute) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M"))
        
        TDC <-
            TDC %>%
            rbind(tdc) %>%
            group_by(Year,Month,Day,Hour,Minute,RT) %>%
            summarise(n=sum(n)) %>%
            ungroup() %>%
            complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
            group_by(Year,Month,Day,Hour,Minute) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M")) %>%
            filter(floor(as.numeric(JTime)/60)<floor(as.numeric(Sys.time())/60))
        
        
        print(head(TDC %>% arrange(desc(JTime))))
        
        write.csv(TDC,"TDC.csv",row.names = F)
        dc <- unique(c(tds$status_id,sort(dc,decreasing = T)))
        dc <- dc[1:min(length(dc),3000)]
        write(dc,"dc.txt")
        
        # print(list.files("Tweet_data"))
        
        output$Mdy <- renderDygraph({
            Comp <- 
                # data.frame(JTime=(max(TDC$JTime)-60*60):(max(TDC$JTime))) %>%
                data.frame(JTime=rep(seq(min(TDC$JTime),max(TDC$JTime),60),each=2),
                           RT=c(F,T))
            
            TDCS <-
                Comp %>%
                left_join(TDC) %>%
                complete(JTime,RT,fill=list(n=0)) %>%
                mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
                select(JTime,RTs,n) %>%
                spread(RTs,n) %>%
                select(Retweet,Origin)
            
            # TDCS <-
            #     TDC %>%
            #     filter(!RT) %>%
            #     select(Origin=n,Total=total)
            
            rownames(TDCS) <- unique(Comp$JTime)
            
            dygraph(TDCS,main = paste0(max(TDC$JTime)-3*60*60,"～",max(TDC$JTime))) %>%
                dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                          axisLabelFontSize = 30,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
                dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$JTime)-3*60*60,max(TDC$JTime))) %>%
                # dyHighlight(highlightCircleSize = 3,
                #             highlightSeriesBackgroundAlpha = 0.5,
                #             highlightSeriesOpts=list(),
                #             hideOnMouseOut = T) %>%
                # dyLegend(show = "always",
                #          width = 100,
                #          showZeroValues = TRUE, labelsDiv = NULL,
                #          labelsSeparateLines = T, hideOnMouseOut = TRUE) 
                dyLegend(width = 175)
            
        })
        
        output$Hdy <- renderDygraph({
            TDCH <-
                TDC %>%
                mutate(M=floor(Minute/10)*10) %>%
                group_by(Year,Month,Day,Hour,M,RT) %>%
                summarise(n=sum(n)) %>%
                ungroup() %>%
                complete(Year,Month,Day,Hour,M,RT,fill=list(n=0)) %>%
                group_by(Year,Month,Day,Hour,M) %>%
                mutate(total=sum(n)) %>%
                ungroup() %>%
                mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,M),format="%Y %m %d %H %M")) %>%
                filter(JTime<Sys.time())
            
            Comp <- 
                data.frame(JTime=rep(seq(min(TDCH$JTime),max(TDCH$JTime),60*10),each=2),
                           RT=rep(c(F,T)))
            TDC2 <-
                Comp %>%
                left_join(TDCH) %>%
                mutate(n=ifelse(is.na(n),0,n)) %>%
                mutate(total=ifelse(is.na(total),0,total))
            
            TDCS <-
                Comp %>%
                left_join(TDC2) %>%
                complete(JTime,RT,fill=list(n=0)) %>%
                mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
                select(JTime,RTs,n) %>%
                spread(RTs,n) %>%
                select(Retweet,Origin)
            
            # TDCS <-
            #     TDC %>%
            #     filter(!RT) %>%
            #     select(Origin=n,Total=total)
            
            rownames(TDCS) <- unique(Comp$JTime)
            
            dygraph(TDCS,main = paste0(max(TDC$JTime)-24*60*60,"～",max(TDC$JTime))) %>%
                dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                          axisLabelFontSize = 30,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
                dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$JTime)-24*60*60,max(TDC$JTime))) %>%
                # dyHighlight(highlightCircleSize = 3,
                #             highlightSeriesBackgroundAlpha = 0.5,
                #             highlightSeriesOpts=list(),
                #             hideOnMouseOut = T) %>%
                # dyLegend(show = "always",
                #          width = 100,
                #          showZeroValues = TRUE, labelsDiv = NULL,
                #          labelsSeparateLines = T, hideOnMouseOut = TRUE) 
                dyLegend(width = 175)
            
        })
        
        
        output$ggraph <- renderPlot({
            # TDS <-
            #   tds %>%
            #   mutate(Tweet2=iconv(Tweet,from="UTF-8",to="",""))
            # 
            # Encoding(TDS$Tweet)
            # Encoding(TDS$Tweet2)
            
            TDS <- 
                fread(paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv")) %>%
                # filter(!is_retweet) %>%
                mutate(ID=paste0("Row",1:n())) %>%
                # mutate(Tweet2=Tweet)
                mutate(Tweet=gsub("<"," <",Tweet)) %>%
                mutate(Tweet2=gsub(">","> ",Tweet))
            # mutate(Tweet2=iconv(Tweet2,from="UTF-8",to="",""))
            
            write.csv(TDS,"TDS.csv",row.names = F,fileEncoding = "CP932")
            TDS <- fread("TDS.csv")
            
            # for (i in 1:nrow(TDS)) {
            #   for (n in 1:nrow(Emoji)) {
            #     if(grepl(Emoji$Uni[n],TDS$Tweet[i])){
            #       TDS$Tweet2[i] <- gsub(Emoji$Uni[n],Emoji$code[n],TDS$Tweet2[i])
            #       print(c(i,Emoji$Unicode[n],Emoji$code[n]))  
            #     }
            #   }
            # }
            # 
            # TDS2 <-
            #   TDS %>%
            #   mutate(Tweet2=iconv(Tweet2,from="UTF-8",to="",""))
            
            
            TF0 <- docDF(TDS, col = 100, type = 1, N = 1, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","動詞","副詞","名詞","接頭詞","連体詞"))
            
            # TF <- docDF(TDS[46,], col = 100, type = 1, N = 10, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic")
            
            TF0S <-
                TF0 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0)
            
            TF0S2 <-
                TF0S %>%
                count(N1,POS1,POS2) %>%
                mutate(CF=N1>=0|N1 %in% ED$Unicode) %>%
                filter(!CF)
            
            TF1 <- docDF(TDS, col = 100, type = 1, N = 3, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","動詞","副詞","名詞","接頭詞","連体詞"))
            
            TF1S <-
                TF1 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                mutate_at(vars(N1,N2,N3),funs(ifelse(. %in% TF0S2$N1,"",.))) %>%
                filter(N1!="") %>%
                left_join(tds %>% select(Tweet,one_of(colnames(tds)))) %>%
                mutate(word=ifelse(grepl("^接頭詞-",POS1) & !grepl("<U",N2),paste0(N1,N2),N1)) %>%
                mutate(word=ifelse(grepl("^[[:alnum:]*]+-接尾-",POS2) & 
                                       !grepl("^記号-接尾-",POS2) & nchar(N2)<3,paste0(N1,N2),word)) %>%
                mutate(word=ifelse(grepl("^接頭詞-",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-接尾",POS2) &　
                                       !grepl("^[[:alnum:]*]+-記号-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word)) %>%
                mutate(word=ifelse(grepl("^[[:alnum:]*]+-接尾-接尾",POS2) & 
                                       !grepl("^記号-接尾-接尾",POS2),paste0(N1,N2,N3),word)) %>%
                mutate(word=ifelse(grepl("^数-数-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word)) %>%
                mutate(word=ifelse(grepl("^数-数-数",POS2),paste0(N1,N2,N3),word)) %>%
                mutate(word=ifelse(grepl("^数-記号-数",POS2),paste0(N1,N2,N3),word)) %>%
                select(N1,N2,N3,POS1,POS2,word,one_of(colnames(.)))
            
            TF1S2 <-
                TF1S %>%
                select(NS=N1,word,ID,n) %>%
                rbind(TF0S %>% filter(!N1 %in% TF0S2$N1) %>% select(NS=N1,word=N1,ID,n)) %>%
                distinct(NS,ID,.keep_all = T) %>%
                group_by(word) %>% #,POS1,POS2
                summarise(Freq=sum(n)) %>%
                ungroup() %>%
                mutate(Rank=frank(-Freq,ties.method="dense")) %>%
                arrange(Rank)
            
            TF2 <- docDF(TDS, col = 100, type = 1, N = 6, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","動詞","副詞","名詞","接頭詞","連体詞"))
            TF2S <-
                TF2 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                mutate_at(vars(N1,N2,N3,N4,N5,N6),funs(ifelse(. %in% TF0S2$N1,"",.))) %>%
                filter(N1!="") %>%
                group_by(N1,N2,N3,N4,N5,N6,POS1,POS2) %>%
                summarise(ID=min(ID),n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(Tweet,one_of(colnames(tds)))) %>%
                # filter(status_id=="x1354629424977592322") %>%
                mutate(word1=ifelse(grepl("^接頭詞-[[:alnum:]*]+-",POS1) & !grepl("<U",N2),paste0(N1,N2),N1)) %>%
                mutate(word1=ifelse(grepl("^[[:alnum:]*]+-接尾-",POS2) & 
                                        !grepl("^記号-接尾-",POS2) & nchar(N2)<3,paste0(N1,N2),word1)) %>%
                mutate(word1=ifelse(grepl("^接頭詞-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-記号-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word1)) %>%
                mutate(word1=ifelse(grepl("^[[:alnum:]*]+-接尾-接尾",POS2) & 
                                        !grepl("^記号-接尾-接尾",POS2),paste0(N1,N2,N3),word1)) %>%
                mutate(word1=ifelse(grepl("^数-数-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word1)) %>%
                mutate(word1=ifelse(grepl("^数-数-数",POS2),paste0(N1,N2,N3),word1)) %>%
                mutate(word1=ifelse(grepl("^数-記号-数",POS2),paste0(N1,N2,N3),word1)) %>%
                # mutate(word1=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^数-記号-数-記号-数",POS2),paste0(N1,N2,N3,N4,N5),word1)) %>%
                # mutate(word1=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^数-数-数-数",POS2),paste0(N1,N2,N3,N4),word1)) %>%
                
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-接頭詞-[[:alnum:]*]+-",POS1) & !grepl("<U",N3),paste0(N2,N3),N2)) %>%
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-接尾-",POS2) & 
                                        !grepl("^[[:alnum:]*]+-記号-接尾-",POS2) & nchar(N3)<3,paste0(N2,N3),word2)) %>%
                # select(N1,N2,N3,N4,N5,N6,word1,word2,POS1,POS2,one_of(colnames(.)))
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-接頭詞-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-記号-接尾",POS2) & nchar(N4)<3,paste0(N2,N3,N4),word2)) %>%
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-接尾-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-記号-接尾-接尾",POS2),paste0(N2,N3,N4),word2)) %>%
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-数-数-接尾",POS2) & nchar(N4)<3,paste0(N2,N3,N4),word2)) %>%
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-数-数-数",POS2),paste0(N2,N3,N4),word2)) %>%
                mutate(word2=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-数-記号-数",POS2),paste0(N2,N3,N4),word2)) %>%
                
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-接頭詞-[[:alnum:]*]+-",POS1) & !grepl("<U",N4),paste0(N3,N4),N3)) %>%
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾-",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-記号-接尾-",POS2) & nchar(N4)<3,paste0(N3,N4),word3)) %>%
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-接頭詞-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+記号-接尾",POS2) & nchar(N5)<3,paste0(N3,N4,N5),word3)) %>%
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-記号-接尾-接尾",POS2),paste0(N3,N4,N5),word3)) %>%
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-数-数-接尾",POS2) & nchar(N5)<3,paste0(N3,N4,N5),word3)) %>%
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-数-数-数",POS2),paste0(N3,N4,N5),word3)) %>%
                mutate(word3=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-数-記号-数",POS2),paste0(N3,N4,N5),word3)) %>%
                
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接頭詞-[[:alnum:]*]+-",POS1) & !grepl("<U",N5),paste0(N4,N5),N4)) %>%
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾-",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-記号-接尾-",POS2) & nchar(N5)<3,paste0(N4,N5),word4)) %>%
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接頭詞-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-記号-接尾",POS2) & nchar(N6)<3,paste0(N4,N5,N6),word4)) %>%
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-接尾-接尾",POS2) & 
                                        !grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-記号-接尾-接尾",POS2),paste0(N4,N5,N6),word4)) %>%
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-数-数-接尾",POS2) & nchar(N6)<3,paste0(N4,N5,N6),word4)) %>%
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-数-数-数",POS2),paste0(N4,N5,N6),word4)) %>%
                mutate(word4=ifelse(grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+",POS1) & grepl("^[[:alnum:]*]+-[[:alnum:]*]+-[[:alnum:]*]+-数-記号-数",POS2),paste0(N4,N5,N6),word4)) %>%
                group_by(N1,N2,N3,N4,N5,N6,status_id) %>%
                mutate(word2=ifelse(!grepl(word2,word1),word2,ifelse(!grepl(word3,word1),word3,word4))) %>%
                ungroup() %>%
                mutate(word1=ifelse(grepl("20../",word1),substring(word1,1,4),word1)) %>%
                # mutate(tf=word==word2) %>%
                select(N1,N2,N3,N4,N5,N6,word1,word2,word3,word4,POS1,POS2,one_of(colnames(.)))
            
            TF2S2 <-
                TF2S %>%
                # filter(word1!="U") %>%
                # filter(word2!="U") %>%
                # filter(!(word1 %in% LETTERS & grepl("[[:digit:]]",word2))) %>%
                # filter(!(word2 %in% LETTERS & grepl("[[:digit:]]",word1))) %>%
                group_by(word1,word2) %>% #,POS1,POS2
                summarise(freq=sum(n)) %>%
                ungroup() %>%
                mutate(rate=freq/sum(freq)) %>%
                filter(word1!=word2) %>%
                filter(word1!="RT") %>%
                mutate(Ranks=frank(-freq,ties.method="min")) %>%
                arrange(Ranks) %>%
                mutate(word=paste0(word1,word2)) %>%
                ungroup() %>%
                filter(freq>1)
            
            TF2S3 <- TF2S2
            for (n in 1:max(nchar(TF2S2$word))) {
                TF2S3 <-
                    TF2S3 %>%
                    mutate(w=substring(word,n+1,nchar(word)))
                
                TF2S3 <-
                    TF2S3 %>%
                    # mutate(word %in% TF2S3$w) %>%
                    filter(!word %in% TF2S3$w)
                print(n)
            }
            
            TF2S4 <-
                TF2S3 %>%
                mutate(rate=freq/sum(freq)) %>%
                filter(word1!=word2) %>%
                filter(word1!="RT") %>%
                filter(word1!=":",word2!=":") %>%
                mutate(Ranks=frank(-freq,ties.method="min")) %>%
                inner_join(TF1S2,by=c("word1"="word")) %>%
                inner_join(TF1S2,by=c("word2"="word")) %>%
                mutate(rate1=freq/Freq.x) %>%
                mutate(rate2=freq/Freq.y) %>%
                mutate(word=paste(word1,word2,sep = "")) %>%
                # filter(!(n==1 & mr==1)) %>%
                arrange(Ranks,desc(rate1),desc(rate2))
            
            TFS <-
                TF2S %>%
                inner_join(TF2S4)
            
            rk=100
            cln=10
            
            TFS0 <-
                TFS %>%
                distinct(word1,word2,freq,rate,Ranks,rate1,rate2,word) %>%
                arrange(Ranks,desc(rate1),desc(rate2)) %>%
                # filter(freq>1) %>%
                filter(Ranks<=rk) %>%
                arrange(desc(rate1),desc(rate2)) %>%
                # filter(rate1>=0.5|rate2>=0.5) %>%
                # filter(!(Ranks==max(Ranks)&rate1<0.1)) %>%
                mutate(num=1:n()) %>%
                filter(num<=100|Ranks<=rk/2)
            
            TFSS <-
                TFS %>%
                filter(word %in% TFS0$word)
            
            k=length(unique(TFSS$Tweet))
            
            TFS1 <-
                rbind(TFSS %>%
                          select(word=word1,Tweet,n)) %>%
                rbind(TFSS %>%
                          select(word=word2,Tweet,n)) %>%
                distinct(word,Tweet,.keep_all = T) %>%
                spread(word,n,fill=0)
            
            TFS2 <-
                TFS1 %>%
                select(-Tweet) %>%
                distinct() %>%
                t()
            
            TFS2_d <- dist(TFS2)
            TFS2_hc <- hclust(TFS2_d, method = "ward.D2") 
            # plot(TFS2_hc)
            
            TFS2_cl <- cutree(TFS2_hc, k=min(k,cln)) %>%
                data.frame() %>%
                rename(Cluster=".") %>%
                add_rownames("word")
            
            TFS3 <-
                TFS %>%
                filter(word %in% TFS0$word) %>%
                # filter((rate1>=rt)|(rate2>=rt)) %>%
                arrange(JTime,Tweet) %>%
                # filter(word1!=word2) %>%
                inner_join(TFS2_cl,by=c("word1"="word")) %>%
                inner_join(TFS2_cl,by=c("word2"="word")) %>%
                mutate(Stime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H"))
            
            k=length(unique(TFS3$Tweet))
            
            TFS00 <-
                TFS3 %>%
                distinct(word1,word2,freq,rate,Ranks,rate1,rate2,word) %>%
                arrange(Ranks)
            
            TFS3_cl <-
                data.frame() %>%
                rbind(TFS3 %>%
                          select(word=word1,Freq=Freq.x,Cluster=Cluster.x)) %>%
                rbind(TFS3 %>%
                          select(word=word2,Freq=Freq.y,Cluster=Cluster.y)) %>%
                distinct() %>%
                mutate(Cluster=factor(Cluster))
            
            g <- as_tbl_graph(TFS00, directed = T) %>%
                left_join(TFS3_cl,by=c("name"="word")) %>%
                left_join(ED,by=c("name"="Unicode")) %>%
                mutate(name=ifelse(!is.na(code),code,name))
            
            G <- data.frame(g)
            MN=max(G$Freq)
            keta=nchar(MN)-1
            br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
            if(length(br)==1){
                keta=keta-1
                br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
            }
            #####    
            p<-
                g %>%
                ggraph(layout ="nicely") +
                # geom_edge_link(aes(alpha = rate1,width = rate),color="royalblue", #
                #                arrow = arrow(length = unit(5,'mm')), end_cap = circle(10,'mm'),force_flip = F) +
                geom_edge_link(aes(width = rate, alpha = rate1),color="royalblue", #
                               arrow = arrow(length = unit(3,'mm')), end_cap = circle(5,'mm'),force_flip = F) +
                geom_node_point(aes(col = Cluster, size = Freq)) +
                geom_node_text(aes(label = name), repel = F, size=7.5) +
                ggtitle(paste0("",min(TFS3$JTime),"~",max(TFS3$JTime),"\n",nrow(TFS00),"ルール")) + #,k,"ツイート　"
                theme_graph(title_size = 30) +
                scale_edge_alpha(range = c(0,1)) +
                scale_size_continuous(range = c(5,30),breaks = c(min(br),max(br))) + #,breaks = seq(0,floor(mn/keta)*keta,by = keta)
                theme( legend.text =  element_text(size = 20), # 凡例
                       legend.title = element_text(face = "bold", size = 20, hjust = 0)) +
                guides(alpha = guide_legend(title = "LEFT", title.position = "left")) +
                guides(colour = guide_legend(order=1 , override.aes = list(size=10)),
                       size   = guide_legend(order=2),
                       edge_width = F,
                       edge_alpha = F) +
                scale_colour_manual(values = ggColorHue(max(TFS3$Cluster.x,TFS3$Cluster.y)),drop=F,breaks=unique(sort(G$Cluster)))
            
            
            plot(p)
            
        })
        
        output$Dline0 <-  renderPlot({
            TDCH <-
                TDC %>%
                mutate(M=floor(Minute/10)*10) %>%
                group_by(Year,Month,Day,Hour,M,RT) %>%
                summarise(n=sum(n)) %>%
                ungroup() %>%
                complete(Year,Month,Day,Hour,M,RT,fill=list(n=0)) %>%
                group_by(Year,Month,Day,Hour,M) %>%
                mutate(total=sum(n)) %>%
                ungroup() %>%
                mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,M),format="%Y %m %d %H %M")) %>%
                filter(JTime<Sys.time())
            
            Comp <- 
                data.frame(JTime=rep(seq(max(TDCH$JTime)-24*60*60,max(TDCH$JTime),60*10),each=2),
                           RT=rep(c(F,T),24*6+1))
            TDC2 <-
                Comp %>%
                left_join(TDCH) %>%
                mutate(n=ifelse(is.na(n),0,n)) %>%
                mutate(total=ifelse(is.na(total),0,total))
            
            keta <-nchar(max(TDC2$total))-1
            if(floor(max(TDC2$total)/(10^keta))<2)
                keta <- keta-1
            
            p <-
                TDC2 %>%
                # filter(total>0) %>%
                mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
                ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
                geom_area(col="black") +
                # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
                labs(x="",y="",fill="") +
                scale_x_datetime(date_breaks="1 hours",date_labels = "%H時") +
                scale_y_continuous(breaks = seq(0,10000000,10^keta),limits = c(0,max(TDC2$total)+10^(keta-1))) +
                ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
                theme(legend.position = "bottom") +
                theme(text = element_text(size=30)) +
                theme(axis.title.x = element_blank())
            
            plot(p)
        })
        
        output$Dline <-  renderPlot({
            TDCH <-
                TDC %>%
                group_by(Year,Month,Day,Hour,RT) %>%
                summarise(n=sum(n)) %>%
                ungroup() %>%
                complete(Year,Month,Day,Hour,RT,fill=list(n=0)) %>%
                group_by(Year,Month,Day,Hour) %>%
                mutate(total=sum(n)) %>%
                ungroup() %>%
                mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H")) %>%
                filter(JTime<Sys.time())
            
            Comp <- 
                data.frame(JTime=rep(seq(max(TDCH$JTime)-24*60*60,max(TDCH$JTime),60*60),each=2),
                           RT=rep(c(F,T),24+1))
            TDC2 <-
                Comp %>%
                left_join(TDCH) %>%
                mutate(n=ifelse(is.na(n),0,n)) %>%
                mutate(total=ifelse(is.na(total),0,total))
            
            keta <-nchar(max(TDC2$total))-1
            if(floor(max(TDC2$total)/(10^keta))<2)
                keta <- keta-1
            
            p <-
                TDC2 %>%
                # filter(total>0) %>%
                mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
                ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
                geom_area(col="black") +
                # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
                labs(x="",y="",fill="") +
                scale_x_datetime(date_breaks="1 hours",date_labels = "%H時",
                                 date_minor_break="1 hours") +
                scale_y_continuous(breaks = seq(0,10000000,10^keta),limits = c(0,max(TDC2$total)+10^(keta-1))) +
                ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
                theme(legend.position = "bottom") +
                theme(text = element_text(size=30)) +
                # theme(axis.text.x =  element_text(size=30)) +
                # theme(axis.text.x =  element_text(size=20,angle = 90,vjust = 0.5)) +
                theme(axis.title.x = element_blank())
            
            plot(p)
        })
        
        output$Mline <-  renderPlot({
            TDCM <-
                TDC %>%
                group_by(Year,Month,Day,RT) %>%
                summarise(n=sum(n)) %>%
                ungroup() %>%
                complete(Year,Month,Day,RT,fill=list(n=0)) %>%
                group_by(Year,Month,Day) %>%
                mutate(total=sum(n)) %>%
                ungroup() %>%
                mutate(JTime=as.POSIXct(paste(Year,Month,Day),format="%Y %m %d")) %>%
                filter(JTime<Sys.time())
            
            Comp <- 
                data.frame(JTime=rep(seq(max(TDCM$JTime)-31*24*60*60,max(TDCM$JTime),24*60*60),each=2),
                           RT=rep(c(F,T),32))
            TDC2 <-
                Comp %>%
                left_join(TDCM) %>%
                mutate(n=ifelse(is.na(n),0,n)) %>%
                mutate(total=ifelse(is.na(total),0,total))
            
            keta <-nchar(max(TDC2$total))-1
            if(floor(max(TDC2$total)/(10^keta))<2)
                keta <- keta-1
            
            p <-
                TDC2 %>%
                # filter(total>0) %>%
                mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
                ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
                geom_area(col="black") +
                # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
                labs(x="",y="",fill="") +
                scale_x_datetime(date_breaks="1 days",date_labels = "%d日") +
                scale_y_continuous(breaks = seq(0,10000000,10^keta),limits = c(0,max(TDC2$total)+10^(keta-1))) +
                ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
                theme(legend.position = "bottom") +
                theme(text = element_text(size=30)) +
                theme(axis.title.x = element_blank())
            
            plot(p)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
