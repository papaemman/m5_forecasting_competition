## 01. Drop the initial zero-demand values
stat_total$first_day <- 1914 - stat_total$lngth

sales_temp[stat_total %>% select(item_id, store_id, first_day), on = c("item_id", "store_id"), nomatch = 0]
  


## 02. Drop the zero-demand gaps
x <- c(1, 1, 0, 0, 1, 0, 0, 0,1) 
y <- c(1 ,0 , 0, 0, 0, 1, 1, 0,1)
myOriginalDf <- data.frame(value=c(x,y), id=rep(c('x','y'), c(length(x), length(y))))
DT <- data.table(myOriginalDf)
DT

# add the original order, so you can't lose it
DT[, orig := .I]

# rle by id, saving the length as a new variables
DT[, rleLength := {rr <- rle(value); rep(rr$length, rr$length)}, by = 'id']
DT

# key by value and length to subset 
setkey(DT, value, rleLength)

# which rows are value = 0 and length > 2
DT[list(0, unique(rleLength[rleLength>2])),nomatch=0]

# Keep only rows with less than 3 consequitevely 0
DT[order(orig)]
DT[!(value==0 & rleLength>2),][order(orig)]



