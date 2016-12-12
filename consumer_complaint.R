##### By Tianlin Duan 11/1/2016 #####

###Goal: manipulate and analyze consumer complaint data
library(dplyr)
library(tidyr)
library(readr)
library(xlsx)
library(stringr)
library(lubridate)
library(ggplot2)

# Read-in the data --------------------------------------------------------
cplt.all <- read_csv("Consumer_Complaints.csv") # much faster than read.csv, but strings not automatically processed as factors
glimpse(cplt.all)
# Restrict to data from 1/2013 to 12/2015
cplt.all$`Date received` <- as.Date(cplt.all$`Date received`,"%m/%d/%Y")
cplt <- cplt.all %>%
  filter(`Date received`>="2013-01-01" & `Date received`<"2016-01-01")
tail(cplt$`Date received`) #check
# Change class/type of some variables for case analysis (for further analysis, all should be the same type as in the dictionary given)
cplt <- cplt %>%
  mutate(`Consumer disputed?`=as.factor(`Consumer disputed?`)) %>%
  mutate(`Timely response?`=as.factor(`Timely response?`)) %>%
  mutate(`Date sent to company`=as.Date(`Date sent to company`,"%m%d%y"))

# [1] Data cleaning (oddities) --------------------------------------------

# (1) Structure of the data, relationships between variables
# For exmaple, in 'Issue', seems like some should be combined to the same category
# But in fact, 'Issue' is dependent on 'Product', so "Customer service/Customer relations" is for 'Pther financial service' in product
#"Customer service / Customer relations" is for 'Credit card' in Product
# So there's reason to believe that the different naming of the 'same' Issue was to intentionally separate the two
p_issue<-cplt %>%
  group_by(Product,Issue) %>%
  summarise(n())
p_issue %>%
  filter(str_detect(Issue,"Customer service"))
#However, do need to pay attention when analyzing these interdependent variables -- should include 'parent' variable as well

# (2) Negative or invalid Zip code
cplt %>%
  filter(`ZIP code`<"00000") %>%
  select(State,`ZIP code`)
sort(unique(cplt$State)) #also checked state abbreviations (common oddity)

# Not a huge issue since 'State' abbrev. are correct and can use that for analysis/mapping
# -and it's hard to find the correct ones for all, eg. '[26XX'
# -but if needed, can check correspondence between state and Zip code and change some of them, eg. CO zip codes start with '8'
cplt <- cplt %>%
  mutate(`ZIP code`=str_replace(cplt$`ZIP code`,"-2914",replacement="02914")) %>%
  mutate(`ZIP code`=str_replace(cplt$`ZIP code`,"-1631",replacement="01631")) %>%


# [2] Complaints per number of customers ----------------------------------
# First check total # of complaints
#Note: U.S. Bank is in the name of U.S. Bancorp, its affiliate. Better to check with non-exact 'str_detect'
total.comp <- cplt %>%
  filter(str_detect(Company,"Bank of America|Citibank|BB&T|Discover|U.S. Bancorp|SunTrust Bank")) %>%  group_by(Company) %>%
  summarise(complaint_count=n()) %>%
  arrange(desc(complaint_count)) %>%
  slice(-7) #Only keep the companies we want to inspect
# Add in the customer # data (unit: million)
customer<-read.xlsx("customer_estimate_source.xlsx",sheetIndex = 1)
customer<-customer[,1:2]
comp2<-inner_join(total.comp,customer,by=c("Company"="Company"))
comp2<- comp2 %>%
  mutate(Comp_per_million=comp2$complaint_count/comp2$million.US.customers) %>%
  mutate(Company=reorder(Company,Comp_per_million)) %>%
  arrange(desc(Comp_per_million))
p2<-comp2 %>%
  ggplot(aes(Company, Comp_per_million))+ geom_bar(stat = "identity")+ coord_flip()
p2 + labs(y = "complaints per million customers")
# Comment: Discover has the least (84.8 complaints per million of customers), Bank of America has the most (778.3 complaints per million of customers)


# [3] Comparison between BoA, Citi, U.S.Bank, BB&T (Wikipedia) ------------
# Metrics: 'Timely Response?', 'Consumer Disputed?', Issue
comp3<- cplt %>%
  filter(Company %in% c("Bank of America","Citibank","U.S. Bancorp", "BB&T Financial")) %>%
  group_by(Company) %>%
  summarise(total=n(),
            disputed=sum(`Consumer disputed?`=='Yes',na.rm=TRUE)/total,
            response=sum(`Timely response?`=='Yes',na.rm=TRUE)/total,
            well_perc=sum(`Timely response?`=='Yes' & `Consumer disputed?`=='No',na.rm=TRUE)/total,
            issue_most=names(which.max(table(Issue))),
            issue_perc=sum(Issue==issue_most,na.rm=TRUE)/total) %>%
  arrange(desc(well_perc))
comp3[,c(1,5,2:4)]
#Comment on overall performance: 
#Citi has the least % of disputed-complaints while BB&T has the most; 
#all four seem to have near-perfect timely responses to complaints;
#thus Citi has the best well-reponded percentage while BB&T has the worst among the four 
#but the total number of complaints differ largely (take this into account when designing #4 metric)
comp3[,c(1,6,7,2)]
#Comment on most-complained issue: 
#40% of BoA's complaints were on 'Loan modification, collection, foreclosure'
#which is also the most complained issue for 3 of the 4

#Perfomance by Issue also available
perfByIssue=cplt %>%
  filter(Company %in% c("Bank of America","Citibank","U.S. Bancorp", "BB&T Financial")) %>%
  group_by(Product,Issue,Company) %>%
  summarise(well_perc=sum(`Timely response?`=='Yes' & `Consumer disputed?`=='No',na.rm=TRUE)/n()) %>%
  spread(key=Company,value=well_perc)

# [4] Performance metric on dealing with complaints -----------------------

# Metrics to use: well-responded rate ([3]), size-normalized complaints
comp4<- cplt %>%
  group_by(Company) %>%
  summarise(total=n(),
            well_perc=sum(`Timely response?`=='Yes' & `Consumer disputed?`=='No',na.rm=TRUE)/total) %>%
  filter(total>1000)%>%
  arrange(desc(well_perc))

# combine with assets (proxy for size-based normalization) for 10 largest banking insititutions in 2015 (Statista)
asset<-read.xlsx("top10_asset.xlsx",sheetIndex = 1)
check4<-inner_join(comp4,asset,by=c("Company"="Company"))
score2<- check4 %>%
  mutate(norm_count=total/assest_15_billion) %>% #complaints per billion asset
  select(Company,well_perc,norm_count)
# plot the leaderboard
plot(x=-score2$norm_count,y=score2$well_perc, 
     main = "Comparing customer satisfaction for 10 largest banks in US",
     xlab = "minus Normalized count of complaints", ylab = "% of well-responded complaints",
     col="blue",pch = 19, cex = 1, lty = "solid", lwd = 2)
text(x=-score2$norm_count,y=score2$well_perc,label=score2$Company,cex= 0.7, pos=3)
abline(v=mean(-score2$norm_count),lty=3)
abline(h=mean(score2$well_perc),lty=3)

# trend (not incorporated into the metric)
trend<- cplt%>%
  mutate(yr=year(`Date received`)) %>%
  group_by(Company,yr) %>%
  summarise(total=n()) %>%
  spread(key=yr,value=total)

trend10<- inner_join(trend,asset,by='Company')
trend10<-trend10 %>%
  select(-assest_15_billion) %>%
  mutate(more=(`2015`-`2013`)/`2013`)
