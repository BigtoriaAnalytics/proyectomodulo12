# The first step is to read data:
library(twitteR)
library(qdapRegex)
library(tm)
library(RTextTools)
library(e1071)
library(wordcloud)
library(rpart)

# trace("create_matrix", edit=T): linea 42 en "Acronym" pasar a "acronym"

### CONECTAR A TWITTER:
consumer_key <- "BrVhe4k8pENjacAcJXzAg"
consumer_secret <- "beqBNfvdZ6iuPIsztGIlM5MRHkDr6qXxLK5Sq9UAXEk"
access_token <- "81855295-JdLxK2F2S9Uveho5FkAj3eW3TsbYzMZTfTJ01Vh5m"
access_secret <- "oYpyQeehIwnlzOmLfDHWyFsKmkBs1lpNb0k4VNOtDzA"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


### IMPORTACION DE DATOS:
# Datos de entreno:
tweetsML <- as.matrix(read.csv("tweetsML.csv", header = T, sep = ";"))

mat.demanda <- tweetsML[,c(1,2)] # datos para clasificar demanda
mat.tema <- tweetsML[, c(1,3)]  # datos para clasificar tema

# Importar tweets a clasificar:
new_tweets <- searchTwitter( "basura",  geocode='42.846718,-2.671635,10km',n=50)

new_tweets <- strip_retweets(new_tweets) # elimina retweets
t.df <- twListToDF(new_tweets) # pasarlo a dataframe

# Limpiar tweets a clasificar:
txt <- t.df$text

clean_text = function(x) {
  x = iconv(x, to = "ASCII", sub = " ")  #elimina letras raras
  x = gsub("@\\w+", "", x) # elimina tags
  x = gsub("[[:punct:]]", "", x) # elimina signos de puntuacion
  x = gsub("[[:digit:]]", "", x) # relimina numeros
  x = gsub("http\\w+", "", x)  # elimina links
  x = gsub("[ |\t]{2,}", "", x) # elimina tabs
  x = gsub("^ ", "", x)  # elimina espacios al principio
  x = gsub(" $", "", x)  # elimina espacios al final
  x = gsub(" +", " ", x)  # elimina espacios en blanco
  x = tolower(x)  # pasarlo a minusculas
  x = gsub("^\\s*<U\\+\\w+>\\s*", "", x)  # elimina emoticonos
  #x = gsub(stopwords("es"), "", x)  # elimina palabras comunes
  x = unique(x)  # elimina duplicados
}

tweets_limpios <- clean_text(txt)
dt.new <- data.frame("tweet"=tweets_limpios, "demanda"=NA)


#### DEMANDA:
# Matrix demanda:
matrixDem= create_matrix(mat.demanda[,1], language="es", 
                      removeStopwords=F, removeNumbers=TRUE, 
                      stemWords=F) 

# Entrenar modelo:
matDem = as.matrix(matrixDem)
classifierDem = naiveBayes(matDem, as.factor(mat.demanda[,2]))

# Matrix tweets nuevos:
matrixNew= create_matrix(dt.new[,1], language="es", 
                         removeStopwords=F, removeNumbers=TRUE, 
                         stemWords=F)

matNew = as.matrix(matrixNew)


# Prediccion:
predictedDem = predict(classifierDem, matNew)
predictedDem

result <- dt.new
result$demanda <- predictedDem

### Localizacion:

geo <- c("zabalgana", "lakua", "ariznavarra", "judizmendi",
         "centro", "salburua")

random.geo <- sample(geo, nrow(result), replace = T)
result$Barrio <- random.geo

### Guardar resultados en csv:
write.csv(result, "New.tweets.csv", quote = F, row.names = F)





#### TEMA:
dtMatrix= create_matrix(mat.tema[,1], language="es", 
                       removeStopwords=F, removeNumbers=TRUE, 
                       stemWords=F) 

# Configure the training data
mat.tema.df <- data.frame(mat.tema)

container <- create_container(dtMatrix, mat.tema.df$tema, trainSize=1:3145, virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)



# escoger de los tweets nuevos los que son demanda:
tweets.demanda <- result[result$demanda=="1", ]
dt.demandas <- as.data.frame(tweets.demanda[, 1])
colnames(dt.demandas) <- "tweet"
dt.demandas$tema <-NA 

predMatrix= create_matrix(dt.demandas[,1],originalMatrix=dtMatrix) 

# create the corresponding container
predSize = nrow(dt.demandas)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

res.tema <- classify_model(predictionContainer, model)
ptema <- res.tema[,1]


result.tema <- dt.demandas
result.tema$tema <- ptema


### Localizacion:

geo <- c("zabalgana", "lakua", "ariznavarra", "judizmendi",
         "centro", "salburua")

rand.geo <- sample(geo, nrow(result.tema), replace = T)
result.tema$Barrio <- rand.geo

write.csv(result.tema, "temas_demanda.csv", quote = F, row.names = F)

#######     FIN!!!!     #######