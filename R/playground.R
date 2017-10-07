library(tidyverse)
# 載入颱風停電戶資料
train <- read.csv("./data/train.csv")
submit <- read.csv("./data/submit.csv")

# 加入電桿資料
# 資料來源為政府資料開放平台(https://data.gov.tw/dataset/33305)
poleString <- c("北北區處pole.csv",    "嘉義區處pole.csv",     "澎湖區處pole.csv",
                "北南區處pole.csv",    "基隆區處pole.csv",     "花蓮區處pole.csv",
                "北市區處pole.csv",    "宜蘭區處pole.csv",     "苗栗區處pole.csv",
                "北西區處pole.csv",    "屏東區處pole.csv",     "金門區處pole.csv",
                "南投區處pole.csv",    "彰化區處pole.csv",     "雲林區處pole.csv",
                "台中區處pole.csv",    "新營區處pole.csv",     "馬祖區處pole.csv",
                "台南區處pole.csv",    "新竹區處pole.csv",     "高雄區處pole.csv",
                "台東區處pole.csv",    "桃園區處pole.csv",     "鳳山區處pole.csv")
pole_wd <- c()
for(i in 1:length(poleString)){
  pole_wd[i] <- paste0("./data/poledata/", poleString[i])
}
pole <- list()
for(i in 1:length(pole_wd)){
  pole[[i]] <- read.csv(pole_wd[i],
                        header = T, 
                        stringsAsFactors = F,
                        sep = "\t"
  )
  pole[[i]] <- pole[[i]][1:5]
}
pole <- Reduce(x = pole, f = rbind)
#清理電桿資料
pole$縣市 <- as.factor(pole$縣市)
pole$行政區 <- as.factor(pole$行政區)
levels(pole$縣市)[30] <- c("台南市")
levels(pole$縣市)[28] <- c("台中市")
levels(pole$縣市)[29] <- c("台北市")
levels(pole$行政區)[332] <- c("頭份市")
pole$縣市 <- as.character(pole$縣市)
pole$縣市行政區 <- paste0(pole$縣市, pole$行政區)
pole$縣市行政區 <- pole$縣市行政區 %>% as.factor()
pole$型式 <- pole$型式 %>% as.factor()
dropSet <- filter(pole, 縣市行政區 == "分#6"|縣市行政區 == "低2"|
                    縣市行政區 == "低1"|縣市行政區 == "9)"|
                    縣市行政區 == "5"|縣市行政區 == "2左低1 "|
                    縣市行政區 == "2左低1"|縣市行政區 == "1右低1"|
                    縣市行政區 == "26"|縣市行政區 == "2)"|
                    縣市行政區 == "2"|縣市行政區 == "1右低1 "|
                    縣市行政區 == "11右2"|縣市行政區 == "11"|
                    縣市行政區 == "1"|縣市行政區 == ")"|
                    縣市行政區 == " ")
pole <- setdiff(pole, dropSet)
# 計算每個縣市鄉鎮區各有哪些種類、各幾支的電桿
# 這邊我只有做到「鄉鎮區」，建議可以再進一步細做「村里」的部分
pole_type <- group_by(pole, 縣市行政區, 型式) %>% 
  summarise(n = n()) %>% 
  ungroup() 
pole_type <- pole_type[-1,]
pole_type <- spread(pole_type, key = 縣市行政區, value = n, fill = 0)
pole_type <- t(pole_type[,-1]) %>% as.data.frame()
names(pole_type) <- c("型式", "pole1", "pole2", "pole3", "pole4", "pole5",
                      "pole6", "pole7", "pole8", "pole9", "pole10")
# 後面讀取中文欄位有錯，先將電桿種類的欄位都改用英文
# 原資料為："3T桿", "H桿", "木併桿", "木桿", "水泥併桿",
# "水泥桿", "用戶自備桿", "鋼併桿", "鋼桿", "電塔"
pole_type$縣市行政區 <- rownames(pole_type)
pole_type <- pole_type[, -1]
train$縣市行政區 <- paste0(train$CityName, train$TownName) 
train <- left_join(train, pole_type, by = "縣市行政區")
for(i in 14:23){
  train[,i][is.na(train[,i]) == TRUE] <- 0
}

# 加入人口戶數資料
# 資料來源為政府資料開放平臺(https://data.gov.tw/dataset/32973#r0)
family <- read.csv("./data/opendata10603M030.csv")
family <- family[-1, c(2,4)]
family$site_id <- gsub(x = family$site_id, pattern = "臺", replacement = "台")
family$site_id <- gsub(x = family$site_id, pattern = "台東", replacement = "臺東")
family$site_id <- gsub(x = family$site_id, pattern = "　", replacement = "")
names(family)[1] <- "縣市行政區"
family$household_no <- as.character(family$household_no) %>% as.numeric()
family <- group_by(family, 縣市行政區) %>% 
  summarise(household = mean(household_no)) 
train <- left_join(train, family, by = "縣市行政區")
# 屏東縣霧臺鄉有1049戶
train$household[train$縣市行政區 == "屏東縣霧臺鄉"] <- rep(1049, 6)
# 雲林縣臺西鄉有8727戶
train$household[train$縣市行政區 == "雲林縣臺西鄉"] <- rep(8727, 15)
# 高雄市三民區有134958戶
train$household[train$縣市行政區 == "高雄市三民區"] <- rep(134958, 86)
# 高雄市鳳山區有134958戶
train$household[train$縣市行政區 == "高雄市鳳山區"] <- rep(138016, 76)

submit <- left_join(submit, train[, c(3, 14:24)], by = "VilCode")

# 將會用到的颱風資料先選出來
soudelor <- select(train, c(1:4, 13:24, 8))
meranti <- select(train, c(1:4, 13:24, 12))
megi <- select(submit, -c(5:6))
nesatAndHaitang <- select(submit, -c(5:6))

# 加入特各颱風風力資料
# 資料來源為颱風資料庫(http://rdc28.cwb.gov.tw/)
gust_csv <- read.csv("./data/gust.csv")
names(gust)[1] <- "CityName"
gust$CityName <- as.factor(gust$CityName)
soudelor <- left_join(soudelor, gust[,c(1:3)], by = "CityName")
megi <- left_join(megi, gust[,c(1, 6:7)], by = "CityName")
meranti <- left_join(meranti, gust[,c(1, 12:13)], by = "CityName")
nesatAndHaitang <- left_join(nesatAndHaitang, gust[,c(1, 14:15)], by = "CityName")

# 建立隨機森林模型
library(randomForest)
names(soudelor)[18:19] <- c("maxWind", "gust")
names(megi)[16:17] <- c("maxWind", "gust")
soudelor_rf <- randomForest(Soudelor~., data = soudelor[, -c(1:5)])
soudelor_pred <- predict(soudelor_rf, newdata = megi[5:17])
megi_pred <- 1.45*soudelor_pred

names(meranti)[18:19] <- c("maxWind", "gust")
names(nesatAndHaitang)[16:17] <- c("maxWind", "gust")
meranti_rf <- randomForest(MerantiAndMalakas~., data = meranti[, -c(1:5)])
meranti_pred <- predict(meranti_rf, newdata = nesatAndHaitang[5:17])
nesatAndHaitang_pred <- 1.53*meranti_pred

submit_dc <- cbind(submit[1:4], nesatAndHaitang_pred) %>% 
             cbind(megi_pred)
names(submit_dc)[5:6] <- c("NesatAndHaitang", "Megi")
write.csv(submit_dc, file = "submit_dc.csv", row.names = FALSE)

