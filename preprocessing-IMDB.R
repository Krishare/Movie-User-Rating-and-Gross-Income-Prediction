imdbData = read.csv("movie_metadata.csv")
pre_processed_data <-imdbData
pre_processed_data$color[is.na(pre_processed_data$color)] <- 'Color'
pre_processed_data$color[pre_processed_data$color == ""] <-"Color"
# pre_processed_data$color[pre_processed_data$color == " Black and White" ] <-"Black and White"


ppd <-pre_processed_data

director_duration = imdbData[,c(2,4)]

director_data = aggregate(director_duration[, 2], list(director_duration$director_name), mean)

ppd$duration[is.na(ppd$duration)] <- director_duration$duration[match(ppd$director_name[is.na(ppd$duration)],director_duration$director_name)]

director_budget = imdbData[,c(2,23)]
director_budget = na.omit(director_budget)
director_budget_data = aggregate(director_budget[, 2], list(director_budget$director_name), mean)
ppd$budget[is.na(ppd$budget)] <- director_budget_data$x[match(ppd$director_name[is.na(ppd$duration)],director_budget_data$Group.1)]

rplotwords = imdbData[,c(17,22)]
rplotwords = na.omit(rplotwords)
#rplotwords = agregrate(imdbData$plot_keywords,list(imdbData$content_rating == 'R'), ]

rplotwordsgroup = aggregate(rplotwords$plot_keywords, list(rplotwords$content_rating), FUN=paste, collapse='|')
missing_content_rating = ppd[,c(17,22)]


#for (i in 1:18){
 # if(grepl(missing_content_rating[,205],rplotwordsgroup[,i]))
#  {
#    print("found") 
#     }
# }
#for (i in 1:18)
#  {
#    for( j in 2:100)
#      {
#        if(grepl(rplotwordsgroup[i,j],toString(missing_content_rating[205,1])))
#         {
#          # missing_content_rating[205,2] = rplotwordsgroup[i,2]
#          print("a")
#         }
#       }
#  }





brokenplotwords<-data.frame(do.call('rbind', strsplit(as.character(rplotwordsgroup$x),'|',fixed=TRUE)))

cbind(rplotwordsgroup[,1],brokenplotwords[,1:100])
plotwordsbroken <- cbind(rplotwordsgroup[,1],brokenplotwords[,1:100])

