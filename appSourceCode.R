library(shiny)
library(rtweet)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(plotly)
library(DT)
library(tm)
library(wordcloud2)
library(stringr)
library(qdapRegex)
library(shinyWidgets)
library(rsconnect)
library(igraph)
library(ggraph)
library(syuzhet)
library(reshape2)
library(plot.matrix)
library(corrplot)
library(factoextra)
library(ggbiplot)
library(devtools)
library(plotly)
library(SnowballC)
library(magrittr)

# Rtweet API key and secret, can be obtained by applying for Twitter developer account
appname<- "xxxxxxx"
key<- "xxxxxxxxxxxxxxxxxx"
secret<- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token<- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_secret<- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)



ui <- fluidPage(
  setBackgroundColor("lightgray", gradient = "radial"),
  titlePanel("Welcome to our hashtag analysis app!"),
  tags$style(
    ".first-p {
      color: black;
    
    }
    #element {
      color: white;
    }
    "
  ),
  p(class = "first-p","An in-development online tool for twitter sentiment and text analysis created by Pavle Kerkez and Adela Pospisilova."),
  
  sidebarLayout(
    sidebarPanel(
      textInput("hashtag", label= "Select hashtag", value="", width = NULL, placeholder = "#enter your hashtag here!"),
      sliderInput("number", label = "Choose the number of tweets!", min = 1000, max = 10000, step = 1000, value = 1000),
      selectInput("rts", "Do you want to include retweets in your query?",
                  c("Yes"= 1,
                    "No"= 0), multiple=FALSE),
      actionButton("results", label = "Get data!")
      
    ),
    mainPanel(tabsetPanel(
      id = "tabset",
      tabPanel("Intro",
               h2("Introduction to the app"),
               p("Welcome Twitter Text Mining app!"),
               p("This is an in-development app meant for text mining and exploration of tweet content."),
               p("The using of the application is simple. To the left, you will find a set of options that decide on what kind
                 and how many recent tweets you will collect trough Rtweet API. You can choose your hashtag (or hashtags, or any other
                 word for that matter), select the number of recent tweets and decide on whether you want to include retweets or not."),
               p("All tweets collected are in English, since lexicons the app is using for sentiment analysis
               and text cleaning are designed specifically for English language."), 
               p("Keep in mind that the more tweets you choose, the more time will be needed for analysis to finish; whether you include retweets
                 or not sometimes changes the analysis results significantly; and if you select a hashtag that does not exist in
                 the most recent tweets, you will receive an error message."),
               p("Also, Rtweet API can collect up to 15 000 tweets per 15 minutes, so be aware of that as well."),
               p("Once the collection of tweets is over, you will receive a notification that you can proceed with the analysis."),
               p("Above this text you can see a few tabs. The Overview tab will produce a few general analyses of the whole corpus
                 of collected tweets, including: a line plot visualizing changes in tweets 'afinn' sentiment over time; a barplot depicting
                 emotion-specific 'nrc' sentiment of the collected tweets; and wordcloud which will present the most frequently used words."),
               p("N-grams tab gives options for extracting n-grams from the collected texts and produces a network with vertices representing
                 words, edges representing words appearing together, and their weights representing the frequency of them appearing together."),
               p("Clustering tab gives options for k-means clustering of tweets. First, it creates a Bag of Words as a method of
                 extracting text features, and then performs k-means clustering on the resulting document term matrix, followed by
                 PCA and then visualizes the clusters with a scatterplot."),
               p("Finally, in the Clustered content you will find options to select one of the resulting clusters and apply n-grams and
                 emotion sentiment analysis on them. Also, you will get a table with raw clustered tweets text so you can find the
                 tweets containing keywords and have a more direct insight into them."),
               div(id = "element", "")),
      
      tabPanel("Overview",
               h2("Sentiment analysis and wordcloud"),
               h4("Click the button below to obtain fresh sentiment analysis and wordcloud for the entire corpus of collected tweets."),
               actionButton("results1", label = "Get plots!"),
               plotlyOutput("sentiment"),
               plotlyOutput("emotions"),
               wordcloud2Output("wordcloud", width = "120%", height = "800px")),
      tabPanel("n-grams",
               h2("N-grams"),
               h4("Use the slider below to choose the number of words in your n-grams, and then press the 'ngrams' button to get results. 
               Additionally, you can modify the number of ngrams to be included in the outputs, as well as choose the network layout that you prefer."),
               sliderInput("n", label= "Set the number of words for n-grams", min = 2, max = 4, step = 1, value = 2),
               numericInput("num", "How many ngrams do you want to include in the report?", value = 50),
               selectInput("layout", "Which network layout do you want?",
                           c("Kamada-kawai"= 1,
                             "Fruchterman-reingold"= 2,
                             "Circular"= 3,
                             "Grid"= 4),multiple=FALSE),
               actionButton("results4", label = "N-grams"),
               plotOutput("network"),
               textOutput("betweenness0"),
               textOutput("betweenness"),
               textOutput("dgr0"),
               textOutput("dgr"),
               DT::dataTableOutput("ngrams")),
      tabPanel("Clustering",
               h2("Bag of Words Kmeans clustering"),
               h4("This part of the analysis has three steps. The first one is creating Bag of Words (BoW)in order to extract
                  text features. The second part is performing Principal Component Analysis (PCA) on the dimensions of BoW
                  (or, specific words that serve as columns in BoW). The third part is k-means clustering of the data in
                  BoW."),
               h4("As a result, you will get a PCA plot that depicts the first two principal components and the % of variability
                  of the data that they explain as well as how each of the BoW dimensions contributes to each PC. Below that,
                  you will get a cluster plot that depicts clusters, their centers and the scattering of the texts across 
                  first two principal comonents. Finally, you will get a table with  the cluster centers."),
               h4("Using the numeric input you can determine the term frequency bounds untill you reach the optimal
                  result in the cluster plot below. With slider input you set the number (k) of centers for the k-means
                  clustering algorithm. The resulting scatterplot will use the first two principal components created from
                  multiple dimensions of Bag of Words as x and y axis."),
               h4("Remember, this part of the analysis takes some trial-and-error and
                  playing with parameters, so take your time untill you get clearly separated clusters and principal components
                  that explain a good deal of variability in the data."),
               numericInput("bow_bounds","Term frequency bounds", value = 200),
               sliderInput("centers", "How many centers do you want for your clustering?", min = 2, max = 10, step = 1, value = 2),
               actionButton("results5", label = "Get clusters!"),
               plotlyOutput("pca"),
               plotlyOutput("clusters"),
               DT::dataTableOutput("centroids")),

              # numericInput("clustN", "Choose for which cluster do you want to see Ngrams", value = 1),
              # actionButton("results6", label = "Cluster ngrams"),
              # sliderInput("n1", label= "Set the number of words for n-grams", min = 2, max = 4, step = 1, value = 2),
             #  numericInput("num1", "How many ngrams do you want to include in the report?", value = 50),
               #selectInput("layout1", "Which network layout do you want?",
                #           c("Kamada-kawai"= 1,
                #             "Fruchterman-reingold"= 2,
                #             "Circular"= 3,
                #             "Grid"= 4),multiple=FALSE),
              # plotOutput("clustNet")),
      tabPanel("Clustered content",
               h2("Cluster content analysis"),
               h4("Enter the number of one of the clusters from kmeans analysis and explore ngrams network, 
                  sentiment and raw content of this cluster."),
                numericInput("clustN", "Choose which cluster do you want to explore", value = 1),
                actionButton("results6", label = "Get plots!"),
                sliderInput("n1", label= "Set the number of words for n-grams", min = 2, max = 4, step = 1, value = 2),
                numericInput("num1", "How many ngrams do you want to include in the report?", value = 50),
                selectInput("layout1", "Which network layout do you want?",
                          c("Kamada-kawai"= 1,
                            "Fruchterman-reingold"= 2,
                            "Circular"= 3,
                            "Grid"= 4),multiple=FALSE),
                plotOutput("clustNet"),
               #actionButton("results7", label = "Get tweets text"),
               #numericInput("clustTweet", "Choose for which cluster do you want Tweets content to be displayed", 
                         #   value = 1),
               #DT::dataTableOutput("sentVals"),
               plotlyOutput("clustEmo"),
               DT::dataTableOutput("tweetsTxt"))
      
    )
    )
  )
)

# i have decided to keep the action buttons because i do really think some users will want
# to use only certain analyses (when I use the app, sometimes i just want to see ngrams or sentiment)
# and there is no need to wait for others. That, and for ngrams there is more customization so i don't
# think it would make sense to run the whole analysis for each change.




server <- function(input, output) {
  
  r1 <- reactiveValues(my_hashtag_sentiment = NULL) #reactive values for hashtag to be displayed as text denoting for which hashtag the outputs are for
  r2 <- reactiveValues(my_hashtag_emotions =  NULL)
  r3 <- reactiveValues(my_hashtag_wordcloud2 = NULL)
  r4 <- reactiveValues(my_hashtag_ngrams = NULL)
  
  observeEvent(input$results1,{r1$my_hashtag_sentiment<- input$hashtag})
  observeEvent(input$results1,{r2$my_hashtag_emotions<- input$hashtag})
  observeEvent(input$results3,{r3$my_hashtag_wordcloud2<- input$hashtag})
  observeEvent(input$results4,{r4$my_hashtag_ngrams<- input$hashtag})
  
  
  # For some reason, tho, the reactive value "r3$my_hashtag_wordcloud2" won't display on "wordcloud" tab and I haven't been able to figure out why.
  
  myData<- eventReactive(input$results,{
    showNotification("Your data is being collected. Depending on how many tweets you chose, 
                     this can take a while. Please be patient. You will receive a notification when the download is complete.", 
                     type = "message", duration = 10)
    search_term<-input$hashtag
    
    tryCatch({tweets<- search_tweets(search_term, 
                                     n = input$number,
                                     include_rts = if (input$rts== 1){
                                       TRUE
                                     }else if (input$rts== 0){
                                       FALSE
                                     },
                                     lang= "en")},
             error= function(e){
               showNotification("An error has occured. It is likely that there is a problem with the hashtag you entered. Please try again with a different hashtag.", 
                                type = "error", duration = NULL)
             })
    
    showNotification("Your data has been collected. You can proceed with the analysis.", 
                     type = "message", duration = 60)
    
    tryCatch({tweets<- tweets}, ########
             error= function(e){
               showNotification("An error has occured. It is likely that there is a problem with the hashtag you entered. Please try again with a different hashtag.", 
                                type = "error", duration = NULL)
             }) #i have added another tryCatch at this place because, when I was doing some testing and looking at errors, some of them seemed to happen in this part of the code
    
  })
  
  
  observeEvent(input$results, myData())
  
  sentiment<-eventReactive(input$results1,{
    tweets<- myData()[3:5]
    
    sentiment <- tweets %>% tidytext::unnest_tokens(output = 'word', input = 'text')
    #sentiment_dataset <- tidytext::get_sentiments("afinn")
    #saveRDS(object = sentiment_dataset, file = "afinn")
    sentiment_dataset<- readRDS("afinn")
    sentiment_dataset <- dplyr::arrange(sentiment_dataset, -value)
    sentiment <- merge(sentiment, sentiment_dataset, by = 'word')
    
    sentiment$word <- NULL
    sentiment$screen_name <- NULL
    sentiment$hour <- format(round(sentiment$created_at, units="hours"), format="%H:%M")
    
    pivot <- sentiment %>%
      dplyr::group_by(hour) %>%
      dplyr::summarise(sentiment = mean(value))
    
  })
  
  emotions<- eventReactive(input$results1,{
    tweets<- myData()[,5]
    
    sentiment <- tweets %>% tidytext::unnest_tokens(output = 'word', input = 'text')
    #sentiment_dataset <- tidytext::get_sentiments("nrc")
    #saveRDS(object = sentiment_dataset, file = "nrc")
    sentiment_dataset<- readRDS("nrc")
    sentiment <- merge(sentiment, sentiment_dataset, by = 'word')
    sentiment$word<- NULL
    sentiment$created_at<- NULL
    sentiment$screen_name<- NULL
    emoTab<- t(table(sentiment))
    emoTab<- data.frame(emoTab)
    emoTab$Var1<- NULL
    emoTabNew<- emoTab[-6:-7,]
    rownames(emoTabNew)<- NULL
    emoTabNew
    
  })
  
  wordcloud<- eventReactive(input$results1,{
    
    text<- str_c(myData()$text, collapse= "")
    text <- 
      text %>%
      str_remove("\\n") %>%                   # remove linebreaks
      rm_twitter_url() %>%                    # Remove URLS
      rm_url() %>%
      str_remove_all("#\\S+") %>%             # Remove any hashtags
      str_remove_all("@\\S+") %>%             # Remove any @ mentions
      removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
      removeNumbers() %>%
      stripWhitespace() %>%
      removeWords(c("amp"))                   # Final cleanup of other small changes
    
    textCorpus <- 
      Corpus(VectorSource(text)) %>%
      TermDocumentMatrix() %>%
      as.matrix()
    
    textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
    textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
  })
  
  ngrams<- eventReactive(input$results4,{
    tweetsText<- myData()[,5]
    
    dat<- tweetsText
    dat<-gsub('http\\S+\\s*', '', dat) #
    dat<-tolower(dat)
    dat<-gsub("[[:digit:]]", "", dat) 
    dat<-gsub("[[:punct:]]", " ", dat) 
    dat<-gsub("\ $", "", dat) 
    dat<-gsub("^\ ", "", dat) 
    dat<-dat[!dat%in%""]
    dat<-gsub(" *\\b[[:alpha:]]{1}\\b *", "", dat)
    dat<- removeWords(dat, stopwords("english"))
    tweetsText<- dat
    tweetsText<- data.frame(tweetsText)
    
    tweets4Ngram<- unnest_tokens(tweetsText, ngram, tweetsText, token = "ngrams", n = input$n)
    
    
    
    tweets4NgramPairs<- separate(tweets4Ngram,ngram, c("word1", "word2", "word3", "word4"), sep = " ")
    
    
    tweets4NgramPairsCount<-dplyr::count(tweets4NgramPairs,word1, word2, word3, word4, sort = TRUE)
    if  (input$n==3){
      tweets4NgramPairsCount$word4<- NULL
    } else if (input$n==2){
      tweets4NgramPairsCount$word4<- NULL
      tweets4NgramPairsCount$word3<- NULL
    }
    
    tweets4NgramPairsCount[1:input$num,] #the one i need
  })
  
  clusters1<- eventReactive(input$results5,{
    tweet_content<-myData()
    tweet_content<- tweet_content$text
    
    corpus = VCorpus(VectorSource(tweet_content))
    
    control_list_words = list(
      tokenize = words,
      language="en",
      bounds = list(global = c(input$bow_bounds, Inf)),
      weighting = weightTfIdf,
      tolower = TRUE,
      removePunctuation = TRUE,
      stopwords = TRUE,
      stemming = TRUE
    )
    
    dtm_words = DocumentTermMatrix(corpus, control=control_list_words)
    #rowTotals <- apply(dtm_words , 1, sum)
    #dtm.new   <- dtm_words[rowTotals> 0, ]
    m_words = as.matrix(dtm_words)
    m_words
#    smthng<- lapply(post_content, get_nrc_sentiment)
#    smthng1<- smthng[["text"]]
#    media_names<- myData()[,2]
#    smthng1<- cbind(smthng1,media_names)
#    smthng1<- smthng1#[,-8:-10]
#    smthng2 <- aggregate(x = smthng1[ , colnames(smthng1) != "status_id"],
#                         by = list(smthng1$status_id), 
#                         mean)  
#    rownames(smthng2)<- smthng2$Group.1
#    smthng2<- smthng2[,-1]
#    smthng2
  })
  
  clusters2<- eventReactive(clusters1(),{
    
    set.seed(1245)
    kmeans<- kmeans(clusters1(), centers = input$centers, iter.max = 10, nstart = 50)
    kmeans
    })
  
  dd<- eventReactive(clusters2(),{
    #aggregate(clusters1(), by=list(cluster=clusters2()$cluster), mean)
    cl<- clusters2()$cluster
    form<- clusters1()
    dd <- cbind(form, cl)
    dd
  })
  
  clustN<- eventReactive(input$results6,{
    obj1<- myData()$text
    obj2<- cbind(obj1, dd()[,max(ncol(dd()))])
    obj2<- data.frame(obj2)
    #obj2<- cbind(obj1, clusters2$cluster)
    clusteredText<- obj2 %>% filter(V2 == input$clustN)
    #clusteredText<-obj2[obj2$`dd()[,max(ncol(dd()))]`==input$clustN,]
    clusteredText<- clusteredText[,1]
    clusteredText<- data.frame(clusteredText)
    clusteredText
    })
  
  clustN1<-eventReactive(input$results6,{
    
    tweetsText<- clustN()
    
    dat<- tweetsText
    dat<-gsub('http\\S+\\s*', '', dat) #
    dat<-tolower(dat)
    dat<-gsub("[[:digit:]]", "", dat) 
    dat<-gsub("[[:punct:]]", " ", dat) 
    dat<-gsub("\ $", "", dat) 
    dat<-gsub("^\ ", "", dat) 
    dat<-dat[!dat%in%""]
    dat<-gsub(" *\\b[[:alpha:]]{1}\\b *", "", dat)
    dat<- removeWords(dat, stopwords("english"))
    tweetsText<- dat
    tweetsText<- data.frame(tweetsText)
    
    tweets4Ngram<- unnest_tokens(tweetsText, ngram, tweetsText, token = "ngrams", n = input$n)
    
    
    
    tweets4NgramPairs<- separate(tweets4Ngram,ngram, c("word1", "word2", "word3", "word4"), sep = " ")
    
    
    tweets4NgramPairsCount<-dplyr::count(tweets4NgramPairs,word1, word2, word3, word4, sort = TRUE)
    if  (input$n1==3){
      tweets4NgramPairsCount$word4<- NULL
    } else if (input$n1==2){
      tweets4NgramPairsCount$word4<- NULL
      tweets4NgramPairsCount$word3<- NULL
    }
    
    tweets4NgramPairsCount[1:input$num1,]
  })
  
  clustEmotions<- eventReactive(input$results6,{
    tweets<- clustN()
    
    sentiment <- tweets %>% tidytext::unnest_tokens(output = 'word', input = 'clusteredText')
    #sentiment_dataset <- tidytext::get_sentiments("nrc")
    #saveRDS(object = sentiment_dataset, file = "nrc")
    sentiment_dataset<- readRDS("nrc")
    sentiment <- merge(sentiment, sentiment_dataset, by = 'word')
    sentiment$word<- NULL
    sentiment$created_at<- NULL
    sentiment$screen_name<- NULL
    emoTab<- t(table(sentiment))
    emoTab<- data.frame(emoTab)
    emoTab$Var1<- NULL
    emoTabNew<- emoTab[-6:-7,]
    rownames(emoTabNew)<- NULL
    emoTabNew
    
  })
  
#  clustN2<- eventReactive(input$results7,{
  #  obj1<- myData()[,5]
  #  obj2<- cbind(obj1, dd()[,8])
  #  clusteredText<-obj2[obj2$`dd()[, 8]`==input$clustTweet,]
  #  clusteredText<- clusteredText[,1]
  #  clusteredText<- data.frame(clusteredText)
  #  clusteredText
  #})
  
  pca<- eventReactive(input$results5,{
    smthng2.pca <- prcomp(clusters1(), center = TRUE,scale. = TRUE)
    smthng2.pca
    })
  
  
  
  output$sentiment<- renderPlotly({
    gtData<- sentiment()
    plot<- ggplot(gtData, aes(x = hour, y = sentiment)) + geom_line(group = 1) + geom_point() + theme_minimal()+ ggtitle(paste("Showing output for hashtag:", r1$my_hashtag_sentiment))
    ggplotly(plot)
  })  #i have removed labels and i will need to reinsert them, after i find a way to put them in while this is reactive
  
  output$emotions<- renderPlotly({
    print(emotions())
    qplot(sentiment, data= emotions(),weight= Freq, geom="bar",fill=sentiment, main = paste("Showing output for hashtag:", r2$my_hashtag_emotions))
  })
  
  
  output$wordcloud<- renderWordcloud2({
    terms<- wordcloud()
    wordcloud2(data = terms, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
  })
  
  output$ngrams<- DT::renderDataTable({
    ngrams()
  })
  
  output$network<- renderPlot({
    graph_ngram<-graph_from_data_frame(ngrams())
    ggraph(graph_ngram, layout = if (input$layout== 1){
      "kk"
    }else if (input$layout== 2){
      "fr"
    }else if (input$layout== 3){
      "circle"
    }else if (input$layout== 4){
      "grid"
    }) +
      geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                     arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                     end_cap = circle(1, "mm")) +
      geom_node_point(color = "pink", size = 3) +
      geom_node_text(aes(label = name), size = 4) +
      theme_void()

  })
  
  output$betweenness0<- renderText({
    if (is.data.frame (ngrams())== TRUE){
      print("The vertices with highest betweenness centrality:")}
  })
  
  output$betweenness<- renderText({
    graph_ngram<-graph_from_data_frame(ngrams())
    degri<- V(graph_ngram)$name[betweenness(graph_ngram)==max(betweenness(graph_ngram))]
    #print("The vertices with highest betweenness centrality:")
    print(degri)
  })
  
  output$dgr0<- renderText({
    if (is.data.frame (ngrams())== TRUE){
      print("The vertices with highest degree:")}
  })
  
  output$dgr<- renderText({
    graph_ngram<-graph_from_data_frame(ngrams())
    degrii<- V(graph_ngram)$name[degree(graph_ngram)==max(degree(graph_ngram))]
    #print("The vertices with highest degree:")
    print(degrii)
  })
  
  output$txt1<- renderText({
    print(paste("Showing output for hashtag:", r1$my_hashtag_sentiment))
  })
  #output$txt2<- renderText({
  #  print(paste("Showing output for hashtag:", r2$my_hashtag_emotions))
  #})
  output$txt3<- renderText({
    print(paste("Showing output for hashtag:", r3$my_hashtag_wordcloud2)) 
  })        #for completely unknown reasons it just won't display the "r3$my_hashtag_wordcloud2" for this one in particular
  #output$txt4<- renderText({
  # print(paste("Showing output for hashtag:", r4$my_hashtag_ngrams))
  #})
  
  output$clusters<- renderPlotly({
    
    fviz_cluster(clusters2(), data = clusters1(), 
                 geom = c("point","text"),
                 ellipse.type = "convex",
                # labels=rownames(clusters1()),
                 ggtheme = theme_bw())
  })
  
  output$clustNet<- renderPlot({
      graph_ngram<-graph_from_data_frame(clustN1())
      ggraph(graph_ngram, layout = if (input$layout1== 1){
        "kk"
      }else if (input$layout1== 2){
        "fr"
      }else if (input$layout1== 3){
        "circle"
      }else if (input$layout1== 4){
        "grid"
      }) +
        geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                       arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                       end_cap = circle(1, "mm")) +
        geom_node_point(color = "pink", size = 3) +
        geom_node_text(aes(label = name), size = 4) +
        theme_void()
    })
  
  output$pca<- renderPlotly({
    ggbiplot(pca(),choices = c(1,2), labels = NULL)
  })
  
  output$tweetsTxt<- DT::renderDataTable({
    clustN()
    #dd()
  })
  #output$sentVals<- DT::renderDataTable({
  #  dd()
  #})
  
  output$clustEmo<- renderPlotly({
    print(clustEmotions())
    qplot(sentiment, data= clustEmotions(),weight= Freq, geom="bar",fill=sentiment, main = paste("Showing output for hashtag:", r2$my_hashtag_emotions))
  })
  
  output$centroids<- DT::renderDataTable({
    clusters2()$centers
  })
}

shinyApp(ui = ui, server = server)