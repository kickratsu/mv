
















































library("arules")

#data=read.transactions("datamba.csv", sep = ",",format = "single", cols=c(1,2),header = T)
#(bill no and item)

data=read.transactions("groceriesmba.csv", sep = ",")
summary(data)
inspect(data[1:5])#first 5 record

itemFrequencyPlot(data,support=0.10)
itemFrequencyPlot(data,topN=5)

apr<-apriori(data = data,parameter =list(support=0.01,confidence=0.50))
inspect(apr)

#library("arulesViz")
#plot(apr,method="graph")
