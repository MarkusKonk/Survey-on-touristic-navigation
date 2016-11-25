#Due to a running publication process, information on the are under 
#investigation were removed in order to ensure anonimity

surveyResults = read.csv("LimeSurvey/results.csv")

#Delete those who live in the city
surveyResults = surveyResults[-c(20,45,50,51),]

#translate from German to English
library(plyr)

for(i in 7:16){
  surveyResults[,i] = revalue(surveyResults[,i],c("nie" = "never", 
                                                  "selten" = "rarely",
                                                  "gelegentlich" = "sometimes",
                                                  "oft" = "often",
                                                  "immer" = "always"))
}

for(i in 37:46){
  surveyResults[,i] = revalue(surveyResults[,i],c("Ja" = "yes", 
                                                  "Nein" = "no")
                              )
}

for(i in 48:51){
  surveyResults[,i] = revalue(surveyResults[,i],c("trifft ziemlich zu" = "agree", 
                                                  "trifft voll zu" = "strongly agree",
                                                  "teils-teils" = "neither agree nor disagree",
                                                  "trifft wenig zu" = "disagree",
                                                  "trifft gar nicht zu" = "strongly disagree")
  )
}

surveyResults[,5] = revalue(surveyResults[,5],c("trifft ziemlich zu" = "agree", 
                                                "trifft voll zu" = "strongly agree",
                                                "teils-teils" = "neither agree nor disagree",
                                                "trifft wenig zu" = "disagree",
                                                "trifft gar nicht zu" = "strongly disagree")
)

surveyResults[,4] = revalue(surveyResults[,4],c("regelmaessig" = "regularly", 
                                                "zum ersten Mal hier" = "first visit",
                                                "schon mehrfach besucht" = "multiple visits" 
                                                )
)

#Demographic information
#Age
mean(surveyResults[,1])
sd(surveyResults[,1])
#Gender
summary(surveyResults[,2])
#visitor or living in the city
summary(surveyResults[,3])
#How often they visited the city
summary(surveyResults[,4])
#Degree of agreement for the statement: I am familiar with the city.
summary(surveyResults[,5])
#Degree of agreement for the statement: I find it easy to explore new cities.
summary(surveyResults[,6])



#https://cran.r-project.org/web/packages/likert/index.html 
require(likert)

likertPlot = function(dataSet, questions, x, y, scale){
  plotlevels <- scale
  subset = dataSet[,x:y]
  
  sapply(subset, class)
  sapply(subset, function(x) { length(levels(x)) } )
  
  for(i in 1:ncol(subset)){
    colnames(subset)[i] = questions[i]
  }
  
  for(i in seq_along(subset)) {
    subset[,i] <- factor(subset[,i], levels=plotlevels)
  }
  
  experimentdataset <- likert(subset)
  likert.bar.plot(experimentdataset, legend = "")
}

#Frequency of use for each device listed
freqScale = c("never", "rarely", "sometimes", "often", "always")
navigationTools = c("Apps on a mobile",
                 "Kiosk displays",
                 "Analogue maps",
                 "Maps behind billboards",
                 "Internet",
                 "Signage",
                 "Landmarks",
                 "Passerby",
                 "Tourist guides",
                 "Guidebooks"
)
likertPlot(surveyResults, navigationTools, 7, 16, freqScale)


#Disengagement caused by smartphones
#If I use a smartphone for orientation...
agreement = c("strongly disagree", "disagree", "neither agree nor disagree", 
              "agree", "strongly agree")
statements = c("...I perceive my environment less.",
                    "...I regularly miss the target.",
                    "...I am better able to concentrate on the environment.",
                    "...I directly find the target."
)
likertPlot(surveyResults, statements, 48, 51, agreement)


#First means when becoming lost
#Mobile apps
print(summary(surveyResults[,37]))
#Kiosk displays
print(summary(surveyResults[,38]))
#Analogue maps
print(summary(surveyResults[,39]))
#Maps behind billboards
print(summary(surveyResults[,40]))
#Internet
print(summary(surveyResults[,41]))
#Signage
print(summary(surveyResults[,42]))
#Landmarks
print(summary(surveyResults[,43]))
#Passersby
print(summary(surveyResults[,44]))
#Tourist guides
print(summary(surveyResults[,45]))
#Guidebooks
print(summary(surveyResults[,46]))

#Information needs before/while travelling
#Internet access
summary(surveyResults[,52])
summary(surveyResults[,75])
#Accommodation
summary(surveyResults[,53])
summary(surveyResults[,76])
#Public transport
summary(surveyResults[,54])
summary(surveyResults[,77])
#Events
summary(surveyResults[,55])
summary(surveyResults[,78])
#Restaurants
summary(surveyResults[,56])
summary(surveyResults[,79])
#Nightlife
summary(surveyResults[,57])
summary(surveyResults[,80])
#Sport
summary(surveyResults[,58])
summary(surveyResults[,81])
#Weather
summary(surveyResults[,59])
summary(surveyResults[,82])
#Shopping
summary(surveyResults[,60])
summary(surveyResults[,83])
#Sights
summary(surveyResults[,61])
summary(surveyResults[,84])
#Museum
summary(surveyResults[,62])
summary(surveyResults[,85])
#Library
summary(surveyResults[,63])
summary(surveyResults[,86])
#School/University
summary(surveyResults[,64])
summary(surveyResults[,87])

#Information sources before/while travelling
#Internet
summary(surveyResults[,66])
summary(surveyResults[,89])
#Moible apps
summary(surveyResults[,67])
summary(surveyResults[,90])
#Guidebooks
summary(surveyResults[,68])
summary(surveyResults[,91])
#Family/Friends
summary(surveyResults[,69])
summary(surveyResults[,93])
#Travel agency
summary(surveyResults[,70])
summary(surveyResults[,94])
#Tourist information
summary(surveyResults[,71])
summary(surveyResults[,95])
#Televsision
summary(surveyResults[,72])
summary(surveyResults[,96])
#Passersby
summary(surveyResults[,73])
summary(surveyResults[,97])

