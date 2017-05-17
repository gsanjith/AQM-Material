library(MCMCpack)
library(gtools)
library(pander) 
library(dplyr)

#TOPIC DISTRIBUTIONS 

set.seed(2015) # for reproducibility

alpha = .1
N = 1
K = 10
x = rdirichlet(N, rep(alpha, K)) %>% as.numeric()
i = 1:10
plot(i,x[1,i])

#We can see that the higher the αα parameter, the more even the standard distribution. 
#A value of 1000 would make very little sense for LDA - each topic is represented equally, 
#resulting in an incoherent document. Rather, the value of α is often fixed at either 
#0.1 or 1/K to create distributions were some topics are highly probable. 
#Some implementations also allow you to vary the parameter individually for each topic, 
#although this is not as common.

#We know now how can can generate a topic distribution for a document. 
#We will follow a similar procedure for the word distributions. 
#Remember, each of the ten topics has a term distribution associated with it.
#As this distribution is also sampled from a Dirichlet, the procedure will be very similar.

#TERM DISTRIBUTIONS

install.packages("rcorpora") 
library(rcorpora)
vocab = corpora("words/literature/shakespeare_words")$words
head(vocab) 
length(vocab)

sizeVocab = length(vocab)
beta = .1
phi = rdirichlet(K, rep(beta, sizeVocab))
# Plotting distribution for the first topic
plot(phi[1,] %>% as.numeric())

dim(phi)

# DOCUMENT GENERATION 

N = 5
NumWords = 20

K = 10
alpha = 1/K
beta = .1

sizeVocab = length(vocab)

theta = rdirichlet(N, rep(alpha, K))
dim(theta)

phi = rdirichlet(K, rep(beta, sizeVocab))
dim(phi)

#Now remember, to generate a document, we first have to allocate topics to each word in each 
#document. Then we have to sample a terms from that document, and concatenate those words 
#to create the document. Let's do this step by step.

i = 1
topics = sample(1:K,
                size = NumWords, 
                replace = TRUE, 
                prob = theta[i,]) 
topics

theta[1,]

#Now, let us obtain words based on the topics chosen. 

j = topics[1]  # first topic
term = sample(vocab, 
              size = 1, 
              prob = phi[j,])
term

# takes a document i and generates a topic distribution
generateTopics = function(i){
  topics = sample(1:K,
                  size = NumWords, 
                  replace = TRUE, 
                  prob = theta[i,]) 
  topics
}

# takes a topic j and samples a term from its term distribution
generateWord = function(j){
  term = sample(vocab, 
                size = 1, 
                prob = phi[j,])
  term
}

generateTopics(i)
generateWord(j)

generateDocument = function(d){
  topics = generateTopics(d) # d is the current document
  terms = lapply(topics, generateWord) %>% unlist() # for each topic, generate a word
  document = terms %>% paste(., collapse = " ") # string those together
  document
}
documents = lapply(1:N, generateDocument)
documents  %>% pander()

#The documents were successfully generated based on the term and topic distribution. 
#Of course, they're all nonsense. Neither our vocabulary or the bag-of-words assumption, 
#where order doesn't matter, reflect reality. Nevertheless, this procedure is the 
#underlying idea behind the success of LDA.
