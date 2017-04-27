# proyectomodulo12
Proyecto formativo (no profesional)
# Libreria necesaria:
library(twitteR)
library(qdapRegex)
library(tm)
library(RTextTools)
library(e1071)
library(wordcloud)
library(rpart)

# trace("create_matrix", edit=T): cambiar en linea 42 "Acronym" a "acronym"

### CONECTAR A TWITTER:
consumer_key <- "YOUR CONSUMER KEY"
consumer_secret <- "YOUR COMSUMER SECRET"
access_token <- "YOUR ACCES TOKEN"
access_secret <- "YOUR ACCESS SECRET"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


### IMPORTACION DE DATOS:
# Datos de entreno:
tweetsML <- as.matrix(read.csv("tweetsML.csv", header = T, sep = ";"))

mat.demanda <- tweetsML[,c(1,2)] # datos para clasificar demanda
mat.tema <- tweetsML[, c(1,3)]  # datos para clasificar tema

# Importar tweets a clasificar:
new_tweets <- searchTwitter( "basura",  geocode='42.846718,-2.671635,10km',n=50)
    # buscamos "basura" por ejemplo

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

# Entrenar modelo Naive Bayes:
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
result

### Guardar resultados en csv:
write.csv(result, "New.tweets.csv", quote = F, row.names = F)



#### TEMA:
dtMatrix= create_matrix(mat.tema[,1], language="es", 
                       removeStopwords=F, removeNumbers=TRUE, 
                       stemWords=F) 

# Configurar data
mat.tema.df <- data.frame(mat.tema)

container <- create_container(dtMatrix, mat.tema.df$tema, trainSize=1:3145, virgin=FALSE)

# entrenar modelo SVM
model <- train_model(container, "SVM", kernel="linear", cost=1)



# escoger de los tweets nuevos los que son demanda:
tweets.demanda <- result[result$demanda=="1", ]
dt.demandas <- as.data.frame(tweets.demanda[, 1])
colnames(dt.demandas) <- "tweet"
dt.demandas$tema <-NA 

predMatrix= create_matrix(dt.demandas[,1],originalMatrix=dtMatrix) 

# crear el contenedor
predSize = nrow(dt.demandas)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)


# Prediccion tema:
res.tema <- classify_model(predictionContainer, model)
ptema <- res.tema[,1]

result.tema <- dt.demandas
result.tema$tema <- ptema

write.csv(result.tema, "temas_demanda.csv", quote = F, row.names = F)

#######     FIN!!!!     #######
