<<<<<<< HEAD
# link to Medium Post:
# https://medium.com/@kotayama/predicting-must-play-games-from-user-reviews-ff30904b5d95?source=friends_link&sk=5f7cd885840a4c1d4f61d812b7d174c3

Sys.setenv(lang="en")

# install all packages below before proceeding
library(readr)
library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)

# read dataset
mc.info = read_csv("dataset/metacritic_game_info.csv")
mc.comments = read_csv("dataset/metacritic_game_user_comments.csv")

# remove duplicated titles
mc.info = mc.info[!duplicated(mc.info$Title), ]

# prepare mc.info for regression
mc.info$Metascore = as.integer(mc.info$Metascore)
mc.info$Year = as.integer(mc.info$Year)
mc.info$Avg_Userscore = as.numeric(mc.info$Avg_Userscore)
mc.info$MustPlay = as.numeric(mc.info$Metascore >= 90)
mc.info$Online = as.numeric(grepl("  Online", mc.info$No_Players))
mc.info = na.omit(mc.info)
head(mc.info, 20)

# check the game's generation
check_gen = function(x) {
  if (x >= 2012) {
    return(8)
  } else if (x >= 2005) {
    return(7)
  } else if (x >= 1998) {
    return(6)
  } else if (x >= 1993) {
    return(5)
  } else {
    return(4)
  }
}

mc.info$Generation = lapply(mc.info$Year, check_gen)
mc.info$Generation = as.numeric(mc.info$Generation)

head(mc.info, 20)
nrow(mc.info)

# Summary Statistics
count = count(mc.info, Metascore)
ggplot(count, aes(x=Metascore, y=n)) + 
  geom_bar(stat = "identity") +
  labs(x="Metascore", y="Number of Games", title="Distribution of Metascore")

ggplot(data=mc.info, aes(x=Avg_Userscore, y=Metascore)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, col="red") +
  labs(x="Avg. Userscore", y="Metascore", title="Relationship between Metascore and Avg. Userscore")

# conduct linear regression to see which variables are related with Meta score
lm = lm(Metascore ~ Avg_Userscore 
        + Online 
        + as.factor(Generation)
        + as.factor(Platform), 
        data=mc.info)
summary(lm)

# conduct logistic regression using variables from dataset
glm1 = glm(MustPlay ~ Avg_Userscore 
        + Online 
        + as.factor(Generation) 
        + as.factor(Platform), 
        data=mc.info,
        family="binomial")
summary(glm1)

# pick random sample of 500 games from the user reviews
mc.game = inner_join(mc.info, mc.comments, by="Title")
set.seed(0)
sample = sample(1:nrow(mc.game), size=20000, replace=FALSE)
head(sample, 10)

mc.rgame = mc.game[sample, ]
head(mc.rgame$Title, 10)
nrow(mc.rgame)

# create corpus
corp.original = VCorpus(VectorSource(mc.rgame$Comment))

custom_stopwords = c("game", "games", "zelda", "battlefield", "gta", "skyrim",
                     "battlefield", "bioshock", "cod", "diablo", "fallout", 
                     "mario", "bioware", "fifa", "football", "witcher", "mass",
                     'oblivion', "portal", "ops", "black", "duty", "warfare", 
                     "call", "galaxy", "zombies", "elder", "modern", "scrolls",
                     "uncharted", "dog", "metal", "legend", "wild", "link", 
                     "infinite", "deus", "effect")

corp = tm_map(corp.original, content_transformer(iconv), to="ASCII", sub="")
corp = tm_map(corp, removePunctuation)
corp = tm_map(corp, removeNumbers)
corp = tm_map(corp, content_transformer(tolower))
corp = tm_map(corp, removeWords, stopwords("english"))
corp = tm_map(corp, removeWords, custom_stopwords)
corp = tm_map(corp, stripWhitespace)

corp[[2]][1]

# create dtm
dtm = DocumentTermMatrix(corp)
dim(dtm)
dtms = removeSparseTerms(dtm, 0.995)
dim(dtms)
dtms.matrix = as.matrix(dtms) 
dtms.matrix[141:150, 402:411]

# compute correlation between the words and MustPlay
corr = cor(mc.rgame$MustPlay, dtms.matrix)

# keep only top 200
top200 = order(abs(corr), decreasing=T)[1:200]
top200

top200words = colnames(corr)[top200]
top200words

# create wordcloud
worddf = data.frame(word=top200words, corr=corr[top200])
worddf$color = ifelse(worddf$corr < 0, "red", "green")
head(worddf, 10)

wordcloud(worddf$word, abs(worddf$corr), max.words=200, colors=worddf$color, 
          random.order=F, ordered.colors=T, rot.per = 0.30, scale=c(3.5, 0.25))

# create new dtm with only top 200
top.dtm = data.frame(dtms.matrix)
top.dtm
top.dtm = top.dtm %>% select(any_of(top200words))
nrow(top.dtm)

# create dataframe for model
foo = as.data.frame(cbind(MustPlay = mc.rgame$MustPlay, top.dtm))
nrow(foo)

# divide into 70% training and 30% testing data
div = round(nrow(foo)*0.7)
div

train.data = foo[1:div, ]
test.data = foo[(div+1):nrow(foo), ]
nrow(train.data)
nrow(test.data)

# create model with train data
model = glm(MustPlay~., data=train.data, family="binomial")
summary(model)

# get coefficients
coef = coef(model)[-1]
coef

# Positive words
pos = coef[coef > 0]
top.pos = sort(pos, decreasing=T)[1:30]
top.pos

# Negative words
neg = coef[coef <= 0]
top.neg = sort(abs(neg), decreasing=T)[1:30]
top.neg

S# predict on training data
train.pred = predict(model, data=train.data, type="response")
train.data$pred = train.pred
head(train.data$pred, 10)

# compute accuracy on train data
train.data$MPPred = as.numeric(train.data$pred > 0.49)
mean(train.data$MustPlay == train.data$MPPred)

# compute accuracy if all values are 0 or 1
mean(0 == train.data$MustPlay)
mean(1 == train.data$MustPlay)

# predict on test data
test.pred = predict(model, newdata=test.data, type="response")
test.data$pred = test.pred

# compute accuracy on test data
test.data$MPPred = as.numeric(test.data$pred > 0.49)
mean(test.data$MustPlay == test.data$MPPred)

# compute accuracy if all values are 0 or 1
mean(0 == test.data$MustPlay)
mean(1 == test.data$MustPlay)


# test on different kinds of reviews/comments. Uncomment to use.

# Must Play Games:
# The Legend of Zelda: Ocarina of Time (Score: 99)
data = read_csv("web_scraped_data/zelda_ocarina.csv")

# Animal Crossing: New Horizons (Score: 90)
# data = read_csv("web_scraped_data/animal_crossing.csv")

# Dragon Quest 11 (Score: 92)
# data = read_csv("web_scraped_data/dragon_quest11.csv")

# Metroid Prime Remastered (Score: 94)
# data = read_csv("web_scraped_data/metroid_prime.csv")

# Tony Hawk's Pro Skater 2 (Score: 98)
# data = read_csv("web_scraped_data/tony_hawk.csv")

# Not Must Play Games:
# Hogwarts Legacy (Score: 87)
# data = read_csv("web_scraped_data/hogwarts_legacy.csv")

# Mario Kart 8 (Score: 88)
# data = read_csv("web_scraped_data/mario_kart8.csv")

# Wild Hearts (Score: 78)
# data = read_csv("web_scraped_data/wild_hearts.csv")

# Dead Space 3: Awakened (Score: 50)
# data = read_csv("web_scraped_data/dead_space3.csv")

# Fast & Furious Crossroads (Score: 35)
# data = read_csv("web_scraped_data/fast_furious.csv")

# create corpus from query
query = paste(data$Comments, collapse = " ")
qcorp.original = VCorpus(VectorSource(query))

qcorp = tm_map(qcorp.original, content_transformer(iconv), to="ASCII", sub="")
qcorp = tm_map(qcorp, removePunctuation)
qcorp = tm_map(qcorp, removeNumbers)
qcorp = tm_map(qcorp, content_transformer(tolower))
qcorp = tm_map(qcorp, removeWords, stopwords("english"))
qcorp = tm_map(qcorp, stripWhitespace)

qcorp[[1]][1]

# create dtm
qdtm = DocumentTermMatrix(qcorp)
dim(qdtm)
qdtms = removeSparseTerms(qdtm, 0.995)
dim(qdtms)

qdtms.matrix = as.matrix(qdtms)
qdtms.matrix[1:10]
qdf = data.frame(qdtms.matrix)

# words that are included in both the dtm and top200words.
inc = intersect(top200words, colnames(qdf))
inc
# words that are included in top200words but not in dtm.
exc = setdiff(top200words, colnames(qdf))
exc

# create df from the included and excluded term. 
df = data.frame(matrix(nrow=1, ncol=length(top200words)))
colnames(df) = top200words
df[inc] = qdf[inc]
df[exc] = 0

# use trained model and df to predict
result = predict(model, newdata=df, type="response")
result

if (result > 0.49) {
  print("this game is a Must Play game.")
} else {
  print("this game is not a Must Play game.")
}

=======
version https://git-lfs.github.com/spec/v1
oid sha256:490896253079b6cf1daeb03777a067375dc113a394eba4853c0e9bb0517e5066
size 8126
>>>>>>> 75e4252 (code updated)
