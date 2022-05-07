## TwitterTextMiningApp
An in-development Shiny app for performing text mining of tweets collected trough Rtweet API. A product of a university project.

Application is an attempt to create a GUI for collecting and analyzing Twitter content focused on selected hashtag(s) (or any string of characters).
Obviously, it still has many improvements that can be implemented both regarding the app design as well as the quality of the analysis (especially text cleaning in some instances). Still, the app has demonstrated varying degrees of successfull analysis depending on the selected hashtag and particular analysis, and very often it is capable of producing useful plots and clusters that give useful insights into the content of tweets.

Among the files uploaded you will find the source code. If you want to use it, you will have to get your own Twitter developer account. You can apply for it on twitter developer portal. Also, there are a lot of drafts I keep hashtagged in the code- just ignore them for now, if you can, I will remove them soon.
Also, I have uploaded the 'afinn' and 'nrc' sentiment lexicons that the application uses for performing sentiment analysis. While there are specific libraries in R that allow for doing sentiment analysis directly, they are way too slow to be a viable option. Keep the 'afinn' and 'nrc' files in the same working directory as the app code for the code to work.
You will also find the file with a link to the working version of the app hosted on shinyapps.io. Keep in mind that the online version can be somewhat slow and unstable, depending on your computer and internet connection quality.

That's it, I think. Feel free to use the code and suggest improvements!
