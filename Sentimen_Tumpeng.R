# Load required libraries
library(tm)
library(caret)
library(glmnet)

# 
# Baca dataset 
library(readxl)
data <- read_excel('Dataset.xlsx')
data$score <- data$review_rating
data$content <- data$review_text

# Create a corpus from the text reviews
corpus <- Corpus(VectorSource(data$content))

# Perform text preprocessing
preprocessData <- function(text) {
  # Spelling normalization
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, stripWhitespace)
  myStopwords = read.csv("Stopwords.csv")
  myStopwords <- na.omit(myStopwords)
  # remove stopwords from corpus
  text<- tm_map(text, removeWords, myStopwords$Stopwords)
  # Remove your own stop word
  # specify your stopwords as a character vector
  text <- tm_map(text,removeWords,c("jam","minggu","full","jaya","ambil","nambah","tokopedia","shopee","dateng","gabisa","kota","dikasih","kemana","yah","yaa","jga","kena","back","lazada","negatif","emang","bagusup","indo","hrs","urus","rumah","seminggu","batas","tulis","nulis","dapatkan","gede","dipakai","setia","semenjak","namanya","rating","lakukan","job","smpai","pdahal","merah","kta","bintangnya","tulisan","tanda","edit","butuh","mah"))
  # Eliminate extra white spaces
  text<- tm_map(text, stripWhitespace)
  # Remove URL
  removeURL<- function(x) gsub("http[[:alnum:]]*", " ", x)
  text<- tm_map(text, removeURL)
  #Replace words
  text<- tm_map(text, gsub, pattern="ribet", replacement="rumit")
  text<- tm_map(text, gsub, pattern="ngawur", replacement="asalasalan")
  text<- tm_map(text, gsub, pattern="ngaco", replacement="asalasalan")
  text<- tm_map(text, gsub, pattern="top", replacement="bagus")
  text<- tm_map(text, gsub, pattern="keren", replacement="bagus")
  text<- tm_map(text, gsub, pattern="belom", replacement="belum")
  text<- tm_map(text, gsub, pattern="system", replacement="sistem")
  text<- tm_map(text, gsub, pattern="histori",replacement="history")
  text<- tm_map(text, gsub, pattern="sgen", replacement="agen")
  text<- tm_map(text, gsub, pattern="handphone", replacement="hp")
  text<- tm_map(text, gsub, pattern="kadaluarsa", replacement="kadaluwarsa")
  text<- tm_map(text, gsub, pattern="tlp", replacement="telepon")
  text<- tm_map(text, gsub, pattern="telpon", replacement="telepon")
  text<- tm_map(text, gsub, pattern="lelet", replacement="lambat")
  text<- tm_map(text, gsub, pattern="lemot", replacement="lambat")
  text<- tm_map(text, gsub, pattern="tf", replacement="transfer")
  text<- tm_map(text, gsub, pattern="cepet", replacement="cepat")
  text<- tm_map(text, gsub, pattern="duit", replacement="uang")
  text<- tm_map(text, gsub, pattern="nunggu", replacement="menunggu")
  text<- tm_map(text, gsub, pattern="ilang", replacement="hilang")
  text<- tm_map(text, gsub, pattern="males", replacement="malas")
  text<- tm_map(text, gsub, pattern="notif",replacement="notifikasi")
  text<- tm_map(text, gsub, pattern="ngasih", replacement="memberi")
  text<- tm_map(text, gsub, pattern="brg", replacement="barang")
  text<- tm_map(text, gsub, pattern="brang", replacement="barang")
  text<- tm_map(text, gsub, pattern="hhilang", replacement="hilang")
  text<- tm_map(text, gsub, pattern="nyesel", replacement="menyesal")
  text<- tm_map(text, gsub, pattern="komplen", replacement="komplain")
  text<- tm_map(text, gsub, pattern="nomer", replacement="nomor")
  text<- tm_map(text, gsub, pattern="voucer", replacement="voucher")
  text<- tm_map(text, gsub, pattern="apps", replacement="aplikasi")
  text<- tm_map(text, gsub, pattern="credits", replacement="kredit")
  text<- tm_map(text, gsub, pattern="blum", replacement="belum")
  text<- tm_map(text, gsub, pattern="pocher", replacement="voucher")
  text<- tm_map(text, gsub, pattern="vouchernya", replacement="voucher")
  text<- tm_map(text, gsub, pattern="ongkirnya", replacement="ongkir")
  text<- tm_map(text, gsub, pattern="negonya", replacement="nego")
  text<- tm_map(text, gsub, pattern="cpt", replacement="cepat")
  text<- tm_map(text, gsub, pattern="resinya", replacement="resi")
  text<- tm_map(text, gsub, pattern="pesen", replacement="pesan")
  text<- tm_map(text, gsub, pattern="cairin", replacement="cair")
  text<- tm_map(text, gsub, pattern="kesel", replacement="kesal")
  text<- tm_map(text, gsub, pattern="nyari", replacement="mencari")
  text<- tm_map(text, gsub, pattern="sempet", replacement="sempat")
  text<- tm_map(text, gsub, pattern="seneng", replacement="senang")
  text<- tm_map(text, gsub, pattern="tqut", replacement="takut")
  text<- tm_map(text, gsub, pattern="seneng", replacement="ketipu")
  text<- tm_map(text, gsub, pattern="menungguin", replacement="menunggu")
  text<- tm_map(text, gsub, pattern="ktipu", replacement="ketipu")
  text<- tm_map(text, gsub, pattern="bgus", replacement="bagus")
  text<- tm_map(text, gsub, pattern="baguss", replacement="bagus")
  text<- tm_map(text, gsub, pattern="mantap", replacement="bagus")
  text<- tm_map(text, gsub, pattern="lemooottt", replacement="lama")
  text<- tm_map(text, gsub, pattern="menuaskan",replacement="memuaskan")
  text<- tm_map(text, gsub, pattern="top",replacement="bagus")
  text<- tm_map(text, gsub, pattern="cape",replacement="lelah")
  text<- tm_map(text, gsub, pattern="nipu",replacement="tipu")
  text<- tm_map(text, gsub, pattern="tunggu",replacement="menunggu")
  text<- tm_map(text, gsub, pattern="tranfer",replacement="transfer")
  text<- tm_map(text, gsub, pattern="simpel",replacement="mudah")
  text<- tm_map(text, gsub, pattern="photo",replacement="foto")
  text<- tm_map(text, gsub, pattern="gambar",replacement="foto")
  text<- tm_map(text, gsub, pattern="blanja",replacement="belanja")
  text<- tm_map(text, gsub, pattern="belanjanya",replacement="belanja")
  text<- tm_map(text, gsub, pattern="sdah",replacement="sudah")
  text<- tm_map(text, gsub, pattern="repot",replacement="rumit")
  text <- tm_map(text, stemDocument)
  return(text)
}
corpus <- preprocessData(corpus)
# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Convert the matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm))
dtm_df$sentiment <- data$score



# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(dtm_df$sentiment, p = 0.7, list = FALSE)
train <- dtm_df[trainIndex, ]
test <- dtm_df[-trainIndex, ]

# Train a maximum entropy model using glmnet
x_train <- as.matrix(train[, -ncol(train)])
y_train <- as.factor(train$sentiment)

model <- glmnet(x_train, y_train, family = "multinomial")

# Prepare the test data for prediction
x_test <- as.matrix(test[, -ncol(test)])

# Predict on the test set
predictions <- predict(model, newx = x_test, type = "class")
predictions <- as.factor(predictions)
test$sentiment <- as.factor(test$sentiment)
# Evaluate the model
data_length <- length(predictions)-length(test$sentiment)+1
pred_length <- length(predictions)
confusionMatrix(predictions[data_length:pred_length], test$sentiment) #ambil kelas terakhir dari prediksi sebagai baris pembanding

# Build the wordcloud
library(wordcloud)
# Split data based on a specific attribute using subset()
positive_review <- subset(data, score == c(4,5))
corpus_positive <- Corpus(VectorSource(positive_review$content))
corpus_positive <- preprocessData(corpus_positive)
wordcloud(corpus_positive,min.freq=4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))
title("Positive Review")
# Split data based on a specific attribute using subset()
negative_review <- subset(data, score == c(1,2,3))
corpus_negative <- Corpus(VectorSource(negative_review$content))
corpus_negative <- preprocessData(corpus_negative)
wordcloud(corpus_negative,min.freq=4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))
title("Negative Review")

# Create pie chart
# Sentiment class labelling
raw_data <- read_excel('Scraping Review Tumpeng Menoreh New.xlsx')
raw_data$review_rating <- ifelse(raw_data$review_rating == c(1,2,3), 2,1)
number_of_positive_review <- sum(raw_data$review_rating == 1)
number_of_positive_review
number_of_negative_review <- sum(raw_data$review_rating == 2)
number_of_negative_review
number_of_review <- c(number_of_positive_review,number_of_negative_review)
# Calculate percentages
total <- sum(number_of_review)
percentages <- round(number_of_review / total * 100, 1)
pie(number_of_review,labels = sprintf("%s (%s%%)", c("Positive","Negative"), percentages), main = "Tumpeng Menoreh Reviews Pie Chart")
