```{r}
library(tm)
txt <- system.file("texts", "txt", package = "tm")
(ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
                 readerControl = list(language = "lat")))

docs <- c("This is a text.", "This another one.")
VCorpus(VectorSource(docs))

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))

inspect(ovid[1:2])

meta(ovid[[2]], "id")
#[1] "ovid_2.txt"
identical(ovid[[2]], ovid[["ovid_2.txt"]])
#[1] TRUE

inspect(ovid[[2]])
#results in an output of the text and metadata

#lappy with as.character will output the text without the metadata
lapply(ovid[1:2], as.character)

data(crude)
data("acq")


acq[[10]][1]

acqStem <- tm_map(acq, stemDocument)
acqStem[[10]][1]

install.packages("wordnet")
library(wordnet)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
getDict()
synonyms("company", "NOUN")
library(SnowballC)
acqRepl <- tm_map(acqStem, removeWords, synonyms(dict, "company"), by = "company")
?tm_map
```