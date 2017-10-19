library(tidyverse)
# 載入颱風停電戶資料
train <- read.csv("./data/train.csv")
submit <- read.csv("./data/submit.csv")

# 加入電桿資料
# 資料來源為政府資料開放平台(https://data.gov.tw/dataset/33305)
library(magrittr)
pole_raw <- dir("data/poledata", full.names = TRUE) %>%
  lapply(read.csv, header = TRUE, stringsAsFactors = FALSE, sep = "\t") 

for(i in 1:length(pole_raw)){
  pole_raw[[i]] <- pole_raw[[i]][1:5]
}

pole_raw <- Reduce(f = rbind, x = pole_raw)

#清理電桿資料
pole <- pole_raw
pole$縣市 <- as.factor(pole$縣市)
pole$行政區 <- as.factor(pole$行政區)
levels(pole$縣市)[which(levels(pole$縣市) == "臺南市")] <- c("台南市")
levels(pole$縣市)[which(levels(pole$縣市) == "臺中市")] <- c("台中市")
levels(pole$縣市)[which(levels(pole$縣市) == "臺北市")] <- c("台北市")
levels(pole$行政區)[which(levels(pole$行政區) == "頭份鎮")] <- c("頭份市")
pole$縣市行政區 <- paste0(pole$縣市, pole$行政區)

# 計算每個縣市鄉鎮區各有哪些種類、各幾支的電桿
# 這邊我只有做到「鄉鎮區」，建議可以再進一步細做「村里」的部分
pole_type <- group_by(pole, 縣市行政區, 型式) %>% 
             summarise(n = n()) %>% 
             ungroup() 
pole_type_sp <- spread(pole_type, key = 縣市行政區, value = n, fill = 0) %>% 
                select(-c(2:16))
pole_type_clean <- t(pole_type_sp) %>% 
                   as.data.frame(stringAsFactors = FALSE) %>% 
                   slice(-1) %>% 
                   select(-1)
names(pole_type_clean) <- c("pole1", "pole2", "pole3", "pole4", "pole5",
                            "pole6", "pole7", "pole8", "pole9", "pole10")
# 後面讀取中文欄位有錯，先將電桿種類的欄位都改用英文
# 原資料為："3T桿", "H桿", "木併桿", "木桿", "水泥併桿",
# "水泥桿", "用戶自備桿", "鋼併桿", "鋼桿", "電塔"
pole_type_clean$縣市行政區 <- names(pole_type_sp)[-1]

pole_type_clean$pole1 <- pole_type_clean$pole1 %>% 
                         as.character() %>% 
                         as.numeric()
pole_type_clean$pole2 <- pole_type_clean$pole2 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole3 <- pole_type_clean$pole3 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole4 <- pole_type_clean$pole4 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole5 <- pole_type_clean$pole5 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole6 <- pole_type_clean$pole6 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole7 <- pole_type_clean$pole7 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole8 <- pole_type_clean$pole8 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole9 <- pole_type_clean$pole9 %>% 
  as.character() %>% 
  as.numeric()
pole_type_clean$pole10 <- pole_type_clean$pole10 %>% 
  as.character() %>% 
  as.numeric()

train$縣市行政區 <- paste0(train$CityName, train$TownName) 
train_pole <- left_join(train, pole_type_clean, by = "縣市行政區")

for(i in 14:23){
  train_pole[,i][is.na(train_pole[,i]) == TRUE] <- rep(0, length(train_pole[,i][is.na(train_pole[,i]) == TRUE]))
}

# 加入人口戶數資料
# 資料來源為政府資料開放平臺(https://data.gov.tw/dataset/32973#r0)
family_raw <- read.csv("./data/opendata10603M030.csv")
family <- family_raw[-1, c(2,4)]
family$site_id <- gsub(x = family$site_id, pattern = "臺", replacement = "台")
family$site_id <- gsub(x = family$site_id, pattern = "台東", replacement = "臺東")
family$site_id <- gsub(x = family$site_id, pattern = "　", replacement = "")
names(family)[1] <- "縣市行政區"
family$household_no <- family$household_no %>% as.character() %>% as.numeric()
family_group <- group_by(family, 縣市行政區) %>% 
                summarise(household = mean(household_no)) 
train_pole_family <- left_join(train_pole, family_group, by = "縣市行政區")
# 屏東縣霧臺鄉有1049戶
train_pole_family$household[train_pole_family$縣市行政區 == "屏東縣霧臺鄉"] <- rep(1049, 6)
# 雲林縣臺西鄉有8727戶
train_pole_family$household[train_pole_family$縣市行政區 == "雲林縣臺西鄉"] <- rep(8727, 15)
# 高雄市三民區有134958戶
train_pole_family$household[train_pole_family$縣市行政區 == "高雄市三民區"] <- rep(134958, 86)
# 高雄市鳳山區有134958戶
train_pole_family$household[train_pole_family$縣市行政區 == "高雄市鳳山區"] <- rep(138016, 76)

submit_pole_family <- left_join(submit, train_pole_family[, c(3, 14:24)], by = "VilCode")

# 將會用到的颱風資料先選出來
soudelor <- select(train_pole_family, c(1:4, 13:24, 8))
meranti <- select(train_pole_family, c(1:4, 13:24, 12))
megi <- select(submit_pole_family, -c(5:6))
nesatAndHaitang <- select(submit_pole_family, -c(5:6))

# 加入特各颱風風力資料
# 資料來源為颱風資料庫(http://rdc28.cwb.gov.tw/)
gust <- read.csv("./data/gust.csv")
names(gust)[1] <- "CityName"
gust$CityName <- as.factor(gust$CityName)
soudelor_gust <- left_join(soudelor, gust[,c(1:3)], by = "CityName")
megi_gust <- left_join(megi, gust[,c(1, 6:7)], by = "CityName")
meranti_gust <- left_join(meranti, gust[,c(1, 12:13)], by = "CityName")
nesatAndHaitang_gust <- left_join(nesatAndHaitang, gust[,c(1, 14:15)], by = "CityName")

# 建立隨機森林模型
library(randomForest)
names(soudelor_gust)[18:19] <- c("maxWind", "gust")
names(megi_gust)[16:17] <- c("maxWind", "gust")
soudelor_rf <- randomForest(Soudelor~., data = soudelor_gust[, -c(1:5)])
soudelor_pred <- predict(soudelor_rf, newdata = megi_gust[5:17])
megi_pred <- 1.45*soudelor_pred

names(meranti_gust)[18:19] <- c("maxWind", "gust")
names(nesatAndHaitang_gust)[16:17] <- c("maxWind", "gust")
meranti_rf <- randomForest(MerantiAndMalakas~., data = meranti_gust[, -c(1:5)])
meranti_pred <- predict(meranti_rf, newdata = nesatAndHaitang_gust[5:17])
nesatAndHaitang_pred <- 1.53*meranti_pred

submit_dc <- cbind(submit_pole_family[1:4], nesatAndHaitang_pred) %>% 
             cbind(megi_pred)
names(submit_dc)[5:6] <- c("NesatAndHaitang", "Megi")
write.csv(submit_dc, file = "submit_dc.csv", row.names = FALSE)

