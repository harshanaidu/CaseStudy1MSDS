  
  ##This project is to analyse the GDPs of all countries and look at all income groups and distribution of various countries compared to others


  ## Read csv files from https location

  X <- read.csv(url("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"))
  
  Y <- read.csv(url("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"),skip=4)
  
  colnames(Y)<- c('CountryCode','Ranking','Limiter','Economy','GDPMilofUSD')
  
  
  ##Merge by Country Code
  
  total <- merge(X,Y,by="CountryCode")

  ## convert x,00,000 USD to x00000 USD [Removes commas in numerics to add numeric sorting later]
  total$GDPMilofUSD<-as.numeric(gsub(",", "", as.character(total$GDPMilofUSD)))

  ## Replaces spaces and '..' with NAs
  total[total==""]<-NA
  total[total==".."]<-NA
  
  
  
  #order by GDP
  totalArrange <- total[ order(as.numeric(as.character(total$GDPMilofUSD))), ] 
  
  #Print 13th country in the data frame
  #We observe here that the 13th country in the data frame is St. Kitts and Nevis
  
  print("13th country in the data frame is ")
  print(totalArrange[13,]$Long.Name)
  
  #order by Income Group
  totalArrange <- total[ order(total$Income.Group), ] 
  
  
  
  
  ## Pattern to grep: High Income:OECD"
  ptn = 'High income: OECD' 
  ## Find all entries for High Income:OECD group
  HighIncomeOECD <- totalArrange[grep(ptn, totalArrange$Income.Group, perl=T),]
  print("average OECD mean is:")
  print(mean(as.numeric(as.character(HighIncomeOECD$Ranking))))
  
  ##Pattern to grep:High income: nonOECD
  ptn = 'High income: nonOECD' 
  ## Find all entries for High Income:nonOECD group
  HighIncomenonOECD <- totalArrange[grep(ptn, totalArrange$Income.Group, perl=T),]
 
  ##High income: nonOECD, ignore NA -> Non assigned rankings
   print("average nonOECD mean is:")
   print(mean(as.numeric(as.character(HighIncomenonOECD$Ranking)), na.rm = TRUE))
  
   ##Splitting total data set into smaller data sets for less crowded visualisations
 
  GDPlessthan20000 <- totalArrange[totalArrange$GDPMilofUSD < 25000,]
  GDPlessthan50000 <- totalArrange[totalArrange$GDPMilofUSD>=25000 & totalArrange$GDPMilofUSD<=250000, ]
  GDPlessthan500000 <- totalArrange[totalArrange$GDPMilofUSD>=250000 & totalArrange$GDPMilofUSD<=500000, ]
  GDPGreaterthan50000 <- totalArrange[totalArrange$GDPMilofUSD>=500000 & totalArrange$GDPMilofUSD<=1000000, ]
  GDPGreaterthan100000 <- totalArrange[totalArrange$GDPMilofUSD > 1000000,]
  
  ##plotting GDP vs Country code, highlighted by Income.group type, I am using 5 different plots here to cover all countries
  
  library(ggplot2)
  library(ggrepel)
  library(scales)
  
  ## plotting scatter plots for all countries using ggplot options, followed by single scatter plot for all countries in one plot
  
  ggplot(GDPlessthan20000, aes(CountryCode,GDPMilofUSD) )+ geom_point(data=GDPlessthan20000,aes(x=CountryCode, y=GDPMilofUSD,colour = Income.Group)) + scale_y_continuous(labels = comma)+ theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_text_repel(aes(label = CountryCode),box.padding = unit(0.45, "lines"))
  ggplot(GDPlessthan50000, aes(CountryCode,GDPMilofUSD) )+ geom_point(data=GDPlessthan50000,aes(x=CountryCode, y=GDPMilofUSD,colour = Income.Group)) + scale_y_continuous(labels = comma)+ theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_text_repel(aes(label = CountryCode),box.padding = unit(0.45, "lines"))
  ggplot(GDPlessthan500000, aes(CountryCode,GDPMilofUSD) )+ geom_point(data=GDPlessthan500000,aes(x=CountryCode, y=GDPMilofUSD,colour = Income.Group)) + scale_y_continuous(labels = comma)+ theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_text_repel(aes(label = CountryCode),box.padding = unit(0.45, "lines"))
  ggplot(GDPGreaterthan50000, aes(CountryCode,GDPMilofUSD) )+ geom_point(data=GDPGreaterthan50000,aes(x=CountryCode, y=GDPMilofUSD,colour = Income.Group)) + scale_y_continuous(labels = comma)+ theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_text_repel(aes(label = CountryCode),box.padding = unit(0.45, "lines"))
  ggplot(GDPGreaterthan100000, aes(CountryCode,GDPMilofUSD) )+ geom_point(data=GDPGreaterthan100000,aes(x=CountryCode, y=GDPMilofUSD,colour = Income.Group)) + scale_y_continuous(labels = comma)+ theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_text_repel(aes(label = CountryCode),box.padding = unit(0.45, "lines"))
  ggplot(total, aes(CountryCode,GDPMilofUSD) )+ geom_point(data=total,aes(x=CountryCode, y=GDPMilofUSD,colour = Income.Group)) + scale_y_continuous(labels = comma)+ theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
  
  #From the scatter plots we observe that most countries have GDP (in Mil of USD is less ) than 1,000,000, the scatter plots have also been marked in different colors for different income groups 
  
  #order by Ranking
  totalArrange <- total[ order(as.numeric(as.character(total$Ranking))), ] 
  
  
  #Extract only GDP and Income group
  subsetGDP<-subset(totalArrange, select=c("GDPMilofUSD", "Income.Group"))
  
  
 
  # Split into 5 quantiles and print
  nr <- nrow(subsetGDP)
  print(split(subsetGDP, rep(1:5, each=nr/5, length.out=nr)))
  
  Top38GDPCountries <- head(subsetGDP,38)
 
  
  
  ## Pattern to grep:  Lowermiddleincome
  ptn = 'Lower middle income' 
  
  ## Find all entries for  Lowermiddleincome
  Lowermiddleincome_InTop38GDP <- Top38GDPCountries[grep(ptn, Top38GDPCountries$Income.Group, perl=T),]
  
  print("Number of Countries which are Lower middle income but among the 38 nations with highest GDP")
  print(dim(Lowermiddleincome_InTop38GDP))
  
  ## We observe from above that 5 countries are of lower middle income among the 38 nations with the highest GDP
  ## Conclusion: GDP ranking does not necessarily reflect the Income group and living standards in countries as gross GDP could be high but the income group of the country and standard of living might still be in a lower bracket
  
  
  ##hmtl and .d file created using rmarkdown::render("Filename.R", "html_document",clean=FALSE)