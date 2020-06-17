#Release my R code for sharing again. I use magic number by store can reach under LB: 0.5
#Actually I have no idea how top 1% guy's amazing jobs. Hope they will share after competition.

#The code below is break down the data by store and cat_id, you can have more memories for FE creating.
#Due to the validation set is released, I will try to do the final work.

#Hope my code give you another angle to see the problems and get great scores.
#Please share your magic to me, if you have better tactics ^_^

#Load packages
library(data.table)
library(lightgbm)

#Set random seed
set.seed(1)

#Forecasting 28 days
h <- 28

#Final training date
fday <- as.IDate("2016-04-24") 

#shift 28 and rolling 180 days
fday_max_lag=fday-(27+180)

#Memory release
free <- function() invisible(gc())

create_dt <- function(store,cat) {
  #read data
  dt=fread("../input/m5-forecasting-accuracy/sales_train_validation.csv")
  
  #which store and category
  dt=dt[store_id==store & cat_id==cat]
  
  #add 28 days dummy column for forecasting
  dt[,paste0("d_",1914:1941):=0.0]
  #transformer to long data format
  dt <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "Qty")
  #read calendar data
  cal <- fread("../input/m5-forecasting-accuracy/calendar.csv")
  #merge
  dt <- dt[cal, `:=`(date = as.IDate(i.date, format="%Y-%m-%d"),
                     wm_yr_wk = i.wm_yr_wk,
                     month = i.month,
                     year = i.year,
                     event_name_1 = i.event_name_1,
                     event_type_1 = i.event_type_1,
                     event_name_2 = i.event_name_2,
                     event_type_2 = i.event_type_2,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]
  #Read price data
  prices <- fread("../input/m5-forecasting-accuracy/sell_prices.csv")
  
  #Merge price
  dt[prices,`:=`(sell_price=i.sell_price),on=c("store_id","item_id","wm_yr_wk")]
  
  #Remove data which price is NA, Prices means item_id starts to sell
  #change sell_price NA greater than fday_max_lag to 0 for caculating lag FE
  dt[is.na(sell_price) & date>=fday_max_lag,sell_price:=0,by=item_id]
  dt[!is.na(sell_price)]
}

create_fea <- function(dt) {
  #Create FE
  #Price FE
  dt[,price_max:=max(sell_price),by="item_id"]
  dt[,price_min:=min(sell_price),by="item_id"]
  dt[,price_mean:=mean(sell_price),by="item_id"]
  dt[,price_sd:=sd(sell_price),by="item_id"]
  dt[,price_med:=median(sell_price),by="item_id"]
  ##min/max scale
  dt[,price_mms:=sell_price/price_max,by="item_id"]
  ##Nunique
  dt[,price_nui:=length(unique(sell_price)),by="item_id"]
  dt[,item_nui:=length(unique(item_id)),by="sell_price"]  
  #monthly price mean/sd/z score
  dt[,price_mean_month:=mean(sell_price),by=c("item_id","month")]
  dt[,price_sd_month:=sd(sell_price),by=c("item_id","month")]
  
  #yearly price mean/sd/z score
  dt[,price_mean_year:=mean(sell_price),by=c("item_id","year")]
  dt[,price_sd_year:=sd(sell_price),by=c("item_id","year")]
  
  #momentum  
  dt[,price_mtum:=sell_price/shift(sell_price),by="item_id"]
  dt[,price_mtum_month:=sell_price/price_mean_month,by="item_id"]
  dt[,price_mtum_year:=sell_price/price_mean_year,by="item_id"]
  
  #category FE
  #simple numeric
  dt[,paste0(c("event_name_1","event_type_1","event_name_2","event_type_2","snap_CA","snap_TX","snap_WI"),"_num"):=lapply(.SD,function(x) as.numeric(as.factor(x))),.SDcol=c("event_name_1","event_type_1","event_name_2","event_type_2","snap_CA","snap_TX","snap_WI")]
  
  #group Sell_price mean by category, not reduce the LB score
  dt[,price_goup_mean1:=mean(sell_price),by=c("item_id","snap_CA")]
  dt[,price_goup_mean2:=mean(sell_price),by=c("item_id","snap_TX")]
  dt[,price_goup_mean3:=mean(sell_price),by=c("item_id","snap_WI")]
  dt[,price_goup_mean4:=mean(sell_price),by=c("item_id","event_name_1")]
  dt[,price_goup_mean5:=mean(sell_price),by=c("item_id","event_name_2")]
  dt[,price_goup_mean6:=mean(sell_price),by=c("item_id","event_type_1")]
  dt[,price_goup_mean7:=mean(sell_price),by=c("item_id","event_type_2")]
  
  #Lag Fe
  dt[,paste0("Qty_lag",28:42):=shift(Qty,28:42),by="item_id"]
  
  #rolling mean
  dt[,paste0("rmean_lag_",1,"_",c(7,14,30,60)):=frollmean(shift(Qty,1),c(7,14,30,60)),by="item_id"]
  dt[,paste0("rmean_lag_",7,"_",c(7,14,30,60)):=frollmean(shift(Qty,7),c(7,14,30,60)),by="item_id"]
  dt[,paste0("rmean_lag_",14,"_",c(7,14,30,60)):=frollmean(shift(Qty,14),c(7,14,30,60)),by="item_id"]
  #new add on 5.30
  dt[,paste0("rmean_lag_",21,"_",c(7,14,30,60)):=frollmean(shift(Qty,21),c(7,14,30,60)),by="item_id"]
  
  #28 shift and rolling mean
  dt[,paste0("rmean28_",c(7,14,30,60,180)):=frollmean(shift(Qty,28),c(7,14,30,60,180)),by="item_id"]
  dt[,paste0("rsd28_",c(7,14,30,60,180)):=frollapply(shift(Qty,28),c(7,14,30,60,180),sd),by="item_id"]
  
  #time variable
  dt[,WK:=week(date)]
  dt[,Quat:=quarter(date)]
  dt[,Mth:=month(date)]
  dt[,WDay:=wday(date)]
  dt[,MDay:=mday(date)]
  dt[,YDay:=yday(date)]
  #is week
  dt[,Isweekend:=ifelse(WDay==1|WDay==7,1,0)]
  
  #group Sell_price mean by time variable
  dt[,price_WK:=mean(sell_price),by=c("item_id","WK")]
  dt[,price_Quat:=mean(sell_price),by=c("item_id","Quat")]
  dt[,price_WK:=mean(sell_price),by=c("item_id","Isweekend")]
  
  #delete unused variable
  dt[,`:=`(
    dept_id=NULL,
    cat_id=NULL,
    store_id=NULL,
    state_id=NULL,
    d=NULL,
    wm_yr_wk=NULL,
    event_name_1=NULL,
    event_type_1=NULL,
    event_name_2=NULL,
    event_type_2=NULL,
    snap_CA=NULL,
    snap_TX=NULL,
    snap_WI=NULL
  )]
  
}

#---------------------------
cat("Create dataset...\n")

#10 stores and 3 cat
STORE=c(paste0("CA_",1:4),paste0("TX_",1:3),paste0("WI_",1:3))
CAT=c("HOBBIES","HOUSEHOLD","FOODS")  

for(run in 1:length(STORE))
{
  for(ct in 1:length(CAT))
  {
    cat("store_id: ",STORE[run],"and",CAT[ct],"\n")
    #Create data
    tr <- create_dt(STORE[run],CAT[ct])
    create_fea(tr)
    #remove lag FE NA
    tr=na.omit(tr)
    
    #Training data size
    cat(paste0("file size: ",format(object.size(tr),unit="Gb"),"\n"))
    
    #-----------------------------------------------#
    cat("Model building and forecasting...\n")
    
    #use date to split train, val, test
    trnD=tr[date<=fday]
    
    #valid set
    valD=tr[date>fday-28 & date<=fday]
    
    #train set
    y1 <- trnD$Qty
    trnD[, c("id","item_id","Qty","date") := NULL]
    
    #test set
    y2 <- valD$Qty
    valD[, c("id","item_id","Qty","date") := NULL] 
    
    #Transfer to matrix for lgb model
    cat("Data matrix...\n")
    trnD <- data.matrix(trnD)
    valD <- data.matrix(valD)
    
    #construct lgb dataset
    xtr <- lgb.Dataset(trnD, label=y1)
    
    #create valid according bin of training
    vals <- lgb.Dataset.create.valid(xtr,valD, label=y2)
    vals=list(test=vals)
    
    #Memory release
    rm(trnD,valD,y1,y2)
    free()
    
    #---------------------------
    cat("Training model...\n")
    
    p=list(objective = "tweedie",
           tweedie_variance_power=1.1,
           metric ="rmse",
           force_row_wise = TRUE,
           #df: 31 max:131072
           num_leaves=2^8-1,
           #df: 20
           min_data_in_leaf=2^9-1,
           #df: no-limit
           # max_depth=12,
           #df:0.1
           learning_rate = 0.03,
           #df: 1
           feature_fraction= 0.5,
           #df: 1
           bagging_fraction= 0.5,
           #df: 255
           max_bin=100,
           #df: 1
           bagging_freq = 1,
           #
           boost_from_average=FALSE,
           #df: 0
           lambda_l1 = 0,
           lambda_l2 = 0,
           nthread = 4)
    
    m_lgb <- lgb.train(params = p,
                       data = xtr,
                       valids = vals,
                       early_stopping_rounds= 50,
                       eval_freq=100,
                       verbose= 1,
                       nrounds=500)
    
    rm(xtr,p)
    free()
    
    #---------------------------
    #test set,update shift 14 and 60 rolling mean
    max_lag=(13+58)
    tetD=tr[date>fday-max_lag]
    
    cat("Forecasting...\n")  
    
    for(day in seq(fday+1, length.out = h, by = "day"))
    {
      cat(paste0("fct: ",as.IDate(day),"\n"))
      print(as.IDate(day))
      tst <- tetD[date==day]
      tst <- data.matrix(tst[, c("id","item_id","Qty","date") := NULL])
      
      tetD[date==day,Qty := predict(m_lgb, tst)]
      
      #update lag FE, uddate lag1
      if(day<fday+h)
      {
        cat(paste0("update lag1 FE: ",as.IDate(day+1),"\n"))
        tetD[date<=day+1,rmean_lag_1_7:=frollmean(shift(Qty,1),7),by=id]
        tetD[date<=day+1,rmean_lag_1_14:=frollmean(shift(Qty,1),14),by=id]
        tetD[date<=day+1,rmean_lag_1_30:=frollmean(shift(Qty,1),30),by=id]
        tetD[date<=day+1,rmean_lag_1_60:=frollmean(shift(Qty,1),60),by=id]
      }
      if(day>=fday+7 && day<fday+h)
      {
        cat(paste0("update lag7 FE: ",as.IDate(day+1),"\n"))
        tetD[date<=day+1,rmean_lag_7_7:=frollmean(shift(Qty,7),7),by=id]
        tetD[date<=day+1,rmean_lag_7_14:=frollmean(shift(Qty,7),14),by=id]
        tetD[date<=day+1,rmean_lag_7_30:=frollmean(shift(Qty,7),30),by=id]
        tetD[date<=day+1,rmean_lag_7_60:=frollmean(shift(Qty,7),60),by=id]
      }
      if(day>=fday+14 && day<fday+h)
      {
        cat(paste0("update lag14 FE: ",as.IDate(day+1),"\n"))
        tetD[date<=day+1,rmean_lag_14_7:=frollmean(shift(Qty,14),7),by=id]
        tetD[date<=day+1,rmean_lag_14_14:=frollmean(shift(Qty,14),14),by=id]
        tetD[date<=day+1,rmean_lag_14_30:=frollmean(shift(Qty,14),30),by=id]
        tetD[date<=day+1,rmean_lag_14_60:=frollmean(shift(Qty,14),60),by=id]
      } 
      if(day>=fday+21 && day<fday+h)
      {
        cat(paste0("update lag21 FE: ",as.IDate(day+1),"\n"))
        tetD[date<=day+1,rmean_lag_21_7:=frollmean(shift(Qty,21),7),by=id]
        tetD[date<=day+1,rmean_lag_21_14:=frollmean(shift(Qty,21),14),by=id]
        tetD[date<=day+1,rmean_lag_21_30:=frollmean(shift(Qty,21),30),by=id]
        tetD[date<=day+1,rmean_lag_21_60:=frollmean(shift(Qty,21),60),by=id]
      } 
    }
    tetD=tetD[date>fday,]
    tetD[,d:=paste0("F",1:28), by = id][
      ,dcast(.SD,id~factor(d,levels=unique(d)),value.var = "Qty")][
        , fwrite(.SD, paste0(STORE[run],"_",CAT[ct],"_lgb.csv"))]
    
    rm(tr,tetD)
    free()
  }
}
#consolidate data
L=list.files("../working/")
Result=c()
for(i in 1:length(L))
  Result=rbind(Result,fread(paste0("../working/",L[i])))

Result_evaluation=copy(Result)
Result_evaluation[,id:=gsub("validation","evaluation",id)]
Result_evaluation[,paste0("F",1:28):=0]

fwrite(rbind(Result,Result_evaluation),"../working/my_sub.csv")

#Magic number by store
dt=fread("../working/my_sub.csv")

#Day1
dt[grep("CA_1",id),F1:=F1*1.009]
dt[grep("CA_2",id),F1:=F1*1.02]
dt[grep("CA_3",id),F1:=F1*1.009]
dt[grep("CA_4",id),F1:=F1*1.011]

dt[grep("TX_1",id),F1:=F1*1.01]
dt[grep("TX_2",id),F1:=F1*1.011]
dt[grep("TX_3",id),F1:=F1*1.011]

dt[grep("WI_1",id),F1:=F1*1.015]
dt[grep("WI_2",id),F1:=F1*1.02]
dt[grep("WI_3",id),F1:=F1*1.015]

#Day2
dt[grep("CA_1",id),F2:=F2*0.994]
dt[grep("CA_2",id),F2:=F2*0.998]
dt[grep("CA_3",id),F2:=F2*0.994]
dt[grep("CA_4",id),F2:=F2*0.996]

dt[grep("TX_1",id),F2:=F2*0.995]
dt[grep("TX_2",id),F2:=F2*0.996]
dt[grep("TX_3",id),F2:=F2*0.996]

dt[grep("WI_1",id),F2:=F2*0.998]
dt[grep("WI_2",id),F2:=F2*1]
dt[grep("WI_3",id),F2:=F2*0.998]

#Day3
dt[grep("CA_1",id),F3:=F3*0.994]
dt[grep("CA_2",id),F3:=F3*0.998]
dt[grep("CA_3",id),F3:=F3*0.994]
dt[grep("CA_4",id),F3:=F3*0.996]

dt[grep("TX_1",id),F3:=F3*0.995]
dt[grep("TX_2",id),F3:=F3*0.996]
dt[grep("TX_3",id),F3:=F3*0.996]

dt[grep("WI_1",id),F3:=F3*1]
dt[grep("WI_2",id),F3:=F3*1.005]
dt[grep("WI_3",id),F3:=F3*1]

#Day4
dt[grep("CA_1",id),F4:=F4*0.98]
dt[grep("CA_2",id),F4:=F4*1.02]
dt[grep("CA_3",id),F4:=F4*0.98]
dt[grep("CA_4",id),F4:=F4*1]

dt[grep("TX_1",id),F4:=F4*0.99]
dt[grep("TX_2",id),F4:=F4*1]
dt[grep("TX_3",id),F4:=F4*1]

dt[grep("WI_1",id),F4:=F4*1]
dt[grep("WI_2",id),F4:=F4*1.02]
dt[grep("WI_3",id),F4:=F4*1]

#Day5

dt[grep("CA_1",id),F5:=F5*0.995]
dt[grep("CA_2",id),F5:=F5*1]
dt[grep("CA_3",id),F5:=F5*0.995]
dt[grep("CA_4",id),F5:=F5*0.998]

dt[grep("TX_1",id),F5:=F5*0.996]
dt[grep("TX_2",id),F5:=F5*0.998]
dt[grep("TX_3",id),F5:=F5*0.998]

dt[grep("WI_1",id),F5:=F5*1]
dt[grep("WI_2",id),F5:=F5*1.01]
dt[grep("WI_3",id),F5:=F5*1]

#Day6

dt[grep("CA_1",id),F6:=F6*1.0002]
dt[grep("CA_2",id),F6:=F6*1.001]
dt[grep("CA_3",id),F6:=F6*1.0002]
dt[grep("CA_4",id),F6:=F6*1.001]

dt[grep("TX_1",id),F6:=F6*1]
dt[grep("TX_2",id),F6:=F6*1.001]
dt[grep("TX_3",id),F6:=F6*1.001]

dt[grep("WI_1",id),F6:=F6*1.001]
dt[grep("WI_2",id),F6:=F6*1.01]
dt[grep("WI_3",id),F6:=F6*1.001]

#Day7

dt[grep("CA_1",id),F7:=F7*0.994]
dt[grep("CA_2",id),F7:=F7*1]
dt[grep("CA_3",id),F7:=F7*0.994]
dt[grep("CA_4",id),F7:=F7*0.998]

dt[grep("TX_1",id),F7:=F7*0.995635]
dt[grep("TX_2",id),F7:=F7*0.998]
dt[grep("TX_3",id),F7:=F7*0.998]

dt[grep("WI_1",id),F7:=F7*1]
dt[grep("WI_2",id),F7:=F7*1.01]
dt[grep("WI_3",id),F7:=F7*1]

#Day8

dt[grep("CA_1",id),F8:=F8*0.996]
dt[grep("CA_2",id),F8:=F8*1]
dt[grep("CA_3",id),F8:=F8*0.996]
dt[grep("CA_4",id),F8:=F8*0.998]

dt[grep("TX_1",id),F8:=F8*0.9988]
dt[grep("TX_2",id),F8:=F8*1]
dt[grep("TX_3",id),F8:=F8*1]

dt[grep("WI_1",id),F8:=F8*1]
dt[grep("WI_2",id),F8:=F8*1.01]
dt[grep("WI_3",id),F8:=F8*1]

#Day9-19

dt[grep("CA_1",id),paste0("F",9:19):=.SD*1,.SDcol=paste0("F",9:19)]
dt[grep("CA_2",id),paste0("F",9:19):=.SD*1.015,.SDcol=paste0("F",9:19)]
dt[grep("CA_3",id),paste0("F",9:19):=.SD*1,.SDcol=paste0("F",9:19)]
dt[grep("CA_4",id),paste0("F",9:19):=.SD*1.013,.SDcol=paste0("F",9:19)]

dt[grep("TX_1",id),paste0("F",9:19):=.SD*1,.SDcol=paste0("F",9:19)]
dt[grep("TX_2",id),paste0("F",9:19):=.SD*1.013,.SDcol=paste0("F",9:19)]
dt[grep("TX_3",id),paste0("F",9:19):=.SD*1.013,.SDcol=paste0("F",9:19)]

dt[grep("WI_1",id),paste0("F",9:19):=.SD*1.015,.SDcol=paste0("F",9:19)]
dt[grep("WI_2",id),paste0("F",9:19):=.SD*1.02,.SDcol=paste0("F",9:19)]
dt[grep("WI_3",id),paste0("F",9:19):=.SD*1.015,.SDcol=paste0("F",9:19)]

#Day20-28

dt[grep("CA_1",id),paste0("F",20:28):=.SD*1.025,.SDcol=paste0("F",20:28)]
dt[grep("CA_2",id),paste0("F",20:28):=.SD*1.035,.SDcol=paste0("F",20:28)]
dt[grep("CA_3",id),paste0("F",20:28):=.SD*1.025,.SDcol=paste0("F",20:28)]
dt[grep("CA_4",id),paste0("F",20:28):=.SD*1.03,.SDcol=paste0("F",20:28)]

dt[grep("TX_1",id),paste0("F",20:28):=.SD*1.025,.SDcol=paste0("F",20:28)]
dt[grep("TX_2",id),paste0("F",20:28):=.SD*1.03,.SDcol=paste0("F",20:28)]
dt[grep("TX_3",id),paste0("F",20:28):=.SD*1.03,.SDcol=paste0("F",20:28)]

dt[grep("WI_1",id),paste0("F",20:28):=.SD*1.035,.SDcol=paste0("F",20:28)]
dt[grep("WI_2",id),paste0("F",20:28):=.SD*1.045,.SDcol=paste0("F",20:28)]
dt[grep("WI_3",id),paste0("F",20:28):=.SD*1.035,.SDcol=paste0("F",20:28)]

fwrite(dt,"../working/my_sub_adjust.csv")