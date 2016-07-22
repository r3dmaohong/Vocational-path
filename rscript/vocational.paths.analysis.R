##Vocational path
##switched over tree?
##how's the future of this job?
##p.s change encoding to ask(weird..?)

##Environment settings
#Sys.setlocale("LC_ALL", 'C')
#rm(list = ls()) #Remove all objects in this environment.
gc() #Frees up the memory
setwd(file.path('Vocational-path'))

dir.create('output', showWarnings = FALSE)
dir.create('rscript', showWarnings = FALSE)
dir.create('tmp', showWarnings = FALSE)

##Packages
source("..\\manual-library\\forloop_loading.R", print.eval  = TRUE)
library(data.table)
library(dplyr)
library(tidyr)
library(openxlsx)
library(arules)
library(stringr)

##Read resume data
list.files(pattern="*.csv")
raw.data <- fread("newdata.csv", verbose=TRUE)
names(raw.data)
raw.data$V1 <- NULL
raw.data$履歷修改日期 <- NULL
raw.data$履歷編號 <- NULL

raw.data <- unique(raw.data)

##Select only "Job titles" and "Number of management"
management.n <- select(raw.data,職務小類名稱,管理人數)
names(management.n) <- c('job_name','manage_num') ##something went wronge while using chinese col names...
(management.n.table <- management.n %>%  group_by(job_name,manage_num) %>% summarize(count=n())) 
(management.n.table <- management.n.table %>% filter(job_name!=''  & manage_num!='未定'))
management.n.table %>% xtabs(~manage_num,.)
management.n.table$count %>% sum
(management.n.table <- management.n.table %>% mutate(percentage=count/sum(count)))
management.n.table$count <- NULL

##Turning one col's(Number of management) content into mutiple cols.
management.n.table <- dcast(management.n.table,job_name~manage_num)
management.n.table[is.na(management.n.table)] <- 0
names(management.n.table)[2:length(names(management.n.table))] <- c('none','maxi','mini','small','mid','big')
##Caculate a new variable: Weighted numbers of management.
management.n.table <- management.n.table %>% mutate(weighted_n = 101*maxi+10*mini+15*small+35*mid+75*big)

##Data validation
setDF(management.n.table)
tmp <- management.n.table[,c(1,length(names(management.n.table)))]
(tmp <- tmp %>% arrange(weighted_n)) #seems to be ok
rm(tmp)
setDT(management.n.table) 
#filter(management.n.table,job_name=="工讀生")
#management.n.table[which(management.n.table$job_name=='工讀生'),]
##management.n percentage table complete

#list.files(pattern="*.xlsx")
#job.transfer_n <- openxlsx:::read.xlsx("職務對職務(20140610 最終檔).xlsx", sheet = 1, startRow = 1, colNames = TRUE)
job.transfer <- raw.data %>% select(職務小類名稱,第幾工作經歷)
names(job.transfer) <- c('name','order')
##Part-time worker should be removed. 
job.transfer <- job.transfer[which(job.transfer$name!='工讀生'),]

##Create a data frame for recording switched over status.
job.transfer_n <- data.frame('original'=character(),'after'=character(),stringsAsFactors=F)

##Recording job tranfering overview 
x = 1
for(i in 1:(nrow(job.transfer)-1)){
  if(job.transfer$order[i+1] > job.transfer$order[i]){
    job.transfer_n[x,1:2] = c(job.transfer$name[i],job.transfer$name[i+1])
    x = x + 1
  }
  ##cat("\r",(i/nrow(job.transfer)*100) %>% round(.,2) %>% format(.,nsmall=2), " %",rep(" ",15))
  forloop_loading(i,(nrow(job.transfer)-1),"Recording job tranfering overview ")
}
##job.transfer_n$percentage <- NULL

##Getting Freq of switched over status.
tmp <- job.transfer_n %>% group_by(original,after) %>% summarize(Freq=n())
tmp <- filter(tmp,original!="" & after!="")
tmp <- arrange(tmp,original,-Freq)

##If his previous job isn't a assistant, I think if his next job is a assistant, this data should be removed. 
none_assistant <- tmp %>% filter(!grepl("助理",after) & !grepl("助理",original))
assistant <- tmp %>% filter(grepl("助理",original))
total.transfer <- do.call(rbind,list(none_assistant,assistant)) ##just for practicing
total.transfer <- total.transfer %>% group_by(original)
total.transfer <-  total.transfer %>% mutate(percentage=Freq/sum(Freq))
rm(tmp)
write.csv(total.transfer,'tmp\\total.transfer.csv',row.names=F)

#setDT(job.transfer_n)
#str(job.transfer_n)
#names(job.transfer_n)[c(3,7,9)] <- c('original','after','percentage')
#job.transfer_n <- job.transfer_n %>% select(original,after,percentage)
#job.transfer_n <- job.transfer_n %>% mutate(prospect='')

##Importing data. Just using it for getting all job's titles.
job.list <- openxlsx:::read.xlsx("職務對職務(20140610 最終檔).xlsx", sheet = 1, startRow = 1, colNames = TRUE)
job.list <- job.list[,3]
job.list <- unique(job.list)

##Remove incorrect job's titles.
total.transfer <- total.transfer %>% filter(original %in% job.list)
total.transfer <- total.transfer %>% mutate(serial.num=0)

##Setting serial numbers for the data set. Each job will have it's numbers counting from 1 to last.
for(i in 1:nrow(total.transfer)){
  if(i == 1){
    total.transfer$serial.num[i] = 1
  }else if(total.transfer$original[i]==total.transfer$original[i-1]){
    total.transfer$serial.num[i] = total.transfer$serial.num[i-1] + 1
  }else{
    total.transfer$serial.num[i] = 1
  }
  forloop_loading(i,nrow(total.transfer),"Setting serial numbers for the data set")
}

##Filter out data set which serial numbers are less than 11
total.transfer <- total.transfer %>% filter(serial.num<11)
##Compute each group's sum?
#aggregate(total.transfer$percentage, by=list(Category=total.transfer$original), FUN=sum)

##I think 7 is a watershed.
setDF(management.n.table)
management.n.table[which(management.n.table[,8]<7),8] = 0


tmp <- rep(NA_integer_,nrow(total.transfer))
for(x in 1:nrow(total.transfer)){
  #Check if whether the after job has more number of management.
  tmp[x]<- ifelse(filter(management.n.table,job_name==total.transfer$after[x]) %>% select(weighted_n) %>% as.numeric > filter(management.n.table,job_name==total.transfer$original[x]) %>% select(weighted_n) %>% as.numeric,1,0)
  forloop_loading(x,nrow(total.transfer))
}
tmp[is.na(tmp)] <- 0
total.transfer$prospect <- tmp
rm(tmp)

##Filter out a new data frame which "prospect" equal to one.
promotion <- total.transfer %>% filter(prospect==1) %>% select(original,after)

##In job.transfer_n, prospect == 1 means that management.n of the after_job is bigger than after_job's management.n. 
n <- promotion$original %>% unique %>% length

##Create a promotion data frame.
promotion_df <- data.frame('original'=character(n),'after'=character(n),stringsAsFactors=F)
promotion_df$original <- promotion$original %>% unique

all <- sapply(promotion_df$original,function(x){
  tmp = promotion %>% filter(original==x) %>% select(after)
  tmp$after %>% paste0(collapse='、')
})

promotion_df$after <- all
##promotion circumstance complete

total.transfer$prospect.word <- total.transfer$after %>% sapply(function(x) ifelse(toString(promotion_df$after[which(promotion_df$original==x)])!='',promotion_df$after[which(promotion_df$original==x)],''))

##Export total transfer situation form.
write.csv(total.transfer,'output\\轉職光譜未來狀況.csv',row.names=F)
#save.image(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"),"job_transfer_complete"))

##Using apriori on jobs and one's background.
nrow(raw.data)
raw.data <- raw.data %>% filter(職務小類名稱 %in% job.list)
nrow(raw.data)
names(raw.data)

##Filter out unless cols.
data.for.trans <- raw.data[,c("職務小類名稱","最高學歷","科系類別","電腦專長","專業證照","語文能力")]
#save.image("ready_to_apri")

##Run each jobs apriori's output.
for(i in 1:length(unique(data.for.trans$職務小類名稱))){
  job <- unique(data.for.trans$職務小類名稱)[i]
  tmp <- data.for.trans %>% filter(職務小類名稱==job)
  
  ##Tranform data to a format that can be used in apriori...
  n <- max(sapply(tmp$電腦專長,function(x) unlist(gregexpr(",", x)) %>% length +1))
  computer.skills <- str_split_fixed(tmp$電腦專長, ", ", n) ##split the content by comma.
  computer.skills <- as.data.frame(computer.skills)
  names(computer.skills) <- c(rep('電腦專長',ncol(computer.skills)))
  n <- max(sapply(tmp$專業證照,function(x) unlist(gregexpr(",", x)) %>% length +1))
  certification <- str_split_fixed(tmp$專業證照, ", ", n)
  certification <- as.data.frame(certification)
  names(certification) <- c(rep('專業憑證',ncol(certification)))
  n <- max(sapply(tmp$語文能力,function(x) unlist(gregexpr(",", x)) %>% length +1))
  language <- str_split_fixed(tmp$語文能力, ", ", n)
  language <- as.data.frame(language)
  names(language) <- c(rep('語言要求',ncol(language)))
  
  tmp <- do.call(cbind,list(tmp[,c("職務小類名稱","最高學歷","科系類別")],computer.skills,certification,language))
  tmp =  data.frame(lapply(tmp, factor))
  
  ##Run Apriori algorithm.
  rules=apriori(as(tmp,"transactions"),parameter=list(supp=0.2,conf=0.8,maxlen=2),appearance=list(rhs=paste0("職務小類名稱=",job),default="lhs"))
  #inspect(sort(rules,by="support"))
  #inspect(rules)
  
  ##Some job title will occur error while exporting to file.
  job <- job %>% gsub("/","／",.)
  
  ##Export the rules
  sink(paste0("output\\apriori\\", job,"background.csv"))
  inspect(sort(rules,by="support"))
  sink()
  
  ##Import the rule with data frame format.
  tmp <- read.csv(paste0("output\\apriori\\", job,"background.csv"))
  tmp <- as.character(tmp[!grepl('=}',tmp[,1]) & !grepl('\\{\\}',tmp[,1]),1])
  
  ##Process the format
  temp <- lapply(tmp,function(x) substr(x,gregexpr('\\{',x)[[1]][1]+1,gregexpr('\\}',x)[[1]][1]-1)) %>% unlist() %>% data.frame
  names(temp) <- job
  temp[,1] <- gsub("[.][0-9]","",temp[,1])
  temp[,1] <- sort(temp[,1])
  
  ##Export
  write.csv(temp,paste0("output\\apriori\\", job,"background.csv"),row.names=F)
  forloop_loading(i,length(unique(data.for.trans$職務小類名稱)),paste0(job ,"\'s apriori"))
}

##I think I should combine this output with previous output (Folder:percentage).
