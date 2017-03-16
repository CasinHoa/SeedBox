library(readr)
library(lattice)
library(reshape2)

testSamples <- read_csv("C:/Users/Cas/Desktop/Seedbox/testSamples.csv")
transData <- read_csv("C:/Users/Cas/Desktop/Seedbox/transData.csv")
transData["Count"] <-1 #Adding a count
testSamples["Count"] <-1

############           1           ############

t.test(testSamples$test_group)  
#Statistics of the distribution of the control group



############           2           ############

RebillOnly <- subset(transData, transaction_type == "REBILL")
RebillOnly <- aggregate(list( number_of_rebill = transData$Count), list(sample_id=transData$sample_id), FUN = sum)
RebillOnly <- merge(RebillOnly, testSamples, by = "sample_id", all = TRUE)
RebillOnly <- RebillOnly[c("sample_id", "test_group", "number_of_rebill")]
RebillOnly[is.na(RebillOnly)] <- 0
TestRebillOnly <- subset(RebillOnly, test_group == 1)
ControlRebillOnly <- subset(RebillOnly, test_group == 0)
#RebillOnly will contain number of rebills separated in test / control groups. We will test statistic difference on it.

t.test(TestRebillOnly$number_of_rebill, ControlRebillOnly$number_of_rebill)
# Statistic test between rebill in control and rebill in test.


############           3           ############

Revenue <- aggregate(list(Revenue = transData$transaction_amount), list(sample_id=transData$sample_id), FUN = sum)
Revenue <- merge(Revenue, testSamples, by = "sample_id", all= TRUE)
Revenue[is.na(Revenue)] <- 0
TestRevenue <- subset(Revenue, test_group == 1)
ControlRevenue <- subset(Revenue, test_group == 0)
#Revenue separated in two groups.

t.test(TestRevenue$Revenue, ControlRevenue$Revenue)

# Statistic test between revenue in control and revenue in test. 



############           4           ############

Chargeback_rate <- merge(transData, testSamples, by= "sample_id")
Chargeback_rate <- aggregate(list(Count = Chargeback_rate$Count.x), list(group = Chargeback_rate$test_group, transaction_type=Chargeback_rate$transaction_type), FUN = sum)  
#The amount of chargeback and rebill in each group

Control_Chargeback_rate = Chargeback_rate[1,3] / Chargeback_rate[3,3]
Test_Chargeback_rate = Chargeback_rate[2,3] / Chargeback_rate[4,3]
print(Control_Chargeback_rate)
print(Test_Chargeback_rate)
#Chargeback rate of each group


############   Summarized graphic   ############

Summarized_Rebill <- aggregate(list(mean_of_rebill = RebillOnly$number_of_rebill), list(group = RebillOnly$test_group), FUN = mean)
Summarized_Revenue <- aggregate(list(Revenue = Revenue$Revenue), list(group = Revenue$test_group), FUN = mean)
Summarized_Chargeback_rate <- read.table(text= "  0  1
	Rebill  3756   3205
	Chargeback   106 57", header = TRUE
	)
Summarized_Chargeback_rate <- melt(Summarized_Chargeback_rate, id.var="Group" )
par(mfrow=c(1,3))
barplot (Summarized_Rebill$mean_of_rebill, names.arg = c("Control Group", "Test Group"), col = c("darkblue","red"), main = "Mean of Rebill by group by person" )
barplot (Summarized_Revenue$Revenue, names.arg = c("Control Group", "Test Group"), col = c("darkblue","red"), main = "Mean of Revenue by group by person" )
barplot (as.matrix(Summarized_Chargeback_rate), names.arg = c("Control Group", "Test Group"), col = c("green","yellow"), main = "Chargeback (Y) on Rebill (G)") 
