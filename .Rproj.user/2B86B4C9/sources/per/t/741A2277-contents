#Install Package Not in Noteable
install.packages(c("tmap","ggspatial","quanteda.textplots", "quanteda.textmodels", "wordcloud"))

# Load required libraries
library(tm)
library(wordcloud)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda)
library(sf)
library(tidyverse)
library(ggspatial)
library(tmap)

# Step 1: Prepare the Dataset ===========
# Set the path to the directory containing our text files
text_files_dir <- "Accounts"

# List the files in the directory
text_files <- list.files(text_files_dir, pattern = ".txt", full.names = TRUE)

# Create a data frame to store title and text
data <- data.frame(title = character(0), text = character(0), stringsAsFactors = FALSE)

# Import and process text files
for (file in text_files) {
  text <- tolower(readLines(file))
  title <- gsub(".txt$", "", basename(file))
  data <- rbind(data, data.frame(title = title, text = paste(text, collapse = " ")))
}

# Save the data frame as a CSV file
write.csv(data, "text_data.csv", row.names = FALSE)

# 2. Clean the Dataset ===========
# Create a Quanteda corpus of the 'article text' column from our data set:
stat_text<-corpus(data, text_field='text')

# 2.1. Extract Information about the Corpus -------------
# Some methods for extracting information about the corpus:
# Print doc in position 5 of the corpus
summary(stat_text, 5)
# Check how many docs are in the corpus
ndoc(stat_text) 
# Check number of characters in the first 10 documents of the corpus
nchar(stat_text[1:10]) 
# Check number of tokens in the first 10 documents
ntoken(stat_text[1:10]) 

#2.2. Extract More info from the dataset
#Extract area and parish from the title
data$Area<- sub(".*(P|C)\\.(.*?)\\..*", "\\2", data$title)
#Extract the Parish
data$Parish<- sub(".*\\.", "", data$title)
# Create a new vector with tokens for all articles and store the vector as a new data frame with three columns (Ntoken, Dataset, title)
NtokenStats<-as.vector(ntoken(stat_text))
TokenScotland <-data.frame(Tokens=NtokenStats, Dataset="Scotland", title=data$title, Area=data$Area, Parish=data$Parish)

# Now we want to see how much material we have for each area
BreakoutScotland<- TokenScotland %>%
  group_by(Area,Dataset)%>%
  summarize(NReports=n(), MeanTokens=round(mean(Tokens)))

# Now we can plot the trends. 
ggplot(BreakoutScotland, aes(x=Area, y=NReports))+ # Select data set and coordinates we are going to plot
  geom_point(aes(size=MeanTokens, fill=MeanTokens),shape=21, stroke=1.5, alpha=0.9, colour="black")+ # Which graph I want 
  labs(x = "Areas", y = "Number of Reports", fill = "Mean of Tokens", size="Mean of Tokens", title="Number of Reports and Tokens in the Scotland Archive")+ # Rename labs and title
  scale_size_continuous(range = c(5, 15))+ # Resize the dots to be bigger
  geom_text(aes(label=MeanTokens))+ # Add the mean of tokens in the dots
  scale_fill_viridis_c(option = "plasma")+ # Change the colour coding
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none") # Remove the Size from the Legend 

# 2.3. Tokenise the Corpus =================
# Now, we can tokenise the corpus, which will break the textual data into separate words grouped by document. We are also removing symbols, URLs, and punctuation.
Report_tokens <- quanteda::tokens(data$text, 
                                   remove_symbols=TRUE, 
                                   remove_url=TRUE, 
                                   remove_punct=TRUE,
                                  remove_numbers = FALSE,
                                  split_hyphens = TRUE)

# Take a look at our tokens list by printing the second document:
Report_tokens[2]

# Remove tokens under 3 characters. (Shorter words won't tell us much about our data, and because we removed punctuation, we want to get rid of the 
#truncated contractions--e.g. I'm -->'I', 'm')
Report_tokens <- tokens_select(Report_tokens, min_nchar = 3)

#Remove stop words
Report_tokens <-tokens_remove(Report_tokens, c(stopwords("english"), "statistical", "account", "parish"))

# 3. Visualise the Results ==============
# Convert to document-feature matrix (aka "dfm")
dfm_Report <- dfm(Report_tokens)

## 3.1. Wordcolud ----------------------
# Plot a wordcloud
textplot_wordcloud(dfm_Report,
                   max_words=100,
                   color='black')
# What observations do we have about the wordcloud? what should our next steps be?

## 3.2. Improving the WordCloud ----------------------

#Let's try the word cloud again:
textplot_wordcloud(dfm_Report, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Spectral")))



# 4. Keywords in Context =================
# keyword search examples (using kwic aka "keyword in context")
tree<-kwic(Report_tokens , #on what
           c("(tree|trees)\\b", "grass", "pasture"), #regex pattern
           valuetype = "regex",#use regex to do so
           window = 10)

#add them back to our main dataset 
TokenScotland<-rownames_to_column(TokenScotland, var = "ID") #Saving Row names as variable
TokenScotland$ID<-paste0("text",TokenScotland$ID)#add text to the ID
#Merge the two dataset
Merged <-merge(tree,TokenScotland, by.x="docname", by.y="ID")
#merge the before and after the keyword
Merged$NewText<-paste0(Merged$pre, Merged$post)

# Now we tokenise again
Merged_tokens <- quanteda::tokens(Merged$NewText, 
                                  remove_symbols=TRUE, 
                                  remove_url=TRUE, 
                                  remove_punct=TRUE,
                                  remove_numbers = TRUE,
                                  split_hyphens = TRUE)
# Remove tokens under 3 characters. (Shorter words won't tell us much about our data, and because we removed punctuation, we want to get rid of the 
#truncated contractions--e.g. I'm -->'I', 'm')
Merged_tokens <- tokens_select(Merged_tokens, min_nchar = 3)

#Remove stop words
Merged_tokens <-tokens_remove(Merged_tokens, c(stopwords("english"), "statistical", "account", "parish","tree", "trees", "grass", "pasture" ))

# Convert to document-feature matrix (aka "dfm")
dfm_Merged <- dfm(Merged_tokens)

#Plot it
textplot_wordcloud(dfm_Merged, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Spectral")))


# 5. Working with Geographical Data

# Where can we find more mentions of weather related events
#Check for keywords and add them to the data dataset
data$meteo<- ifelse(grepl("weather|rain|snow|wind|thunder|meteo", data$text, ignore.case = T), "yes","no")

#group by meteo and area
WeatherGroup<-data %>%
  group_by(Parish,meteo)%>%
  summarize(NReportsM=n())

#Subset only the one containing mentions
MeteoMentions<-subset(WeatherGroup, meteo=="yes")

#Export the file
write.csv(MeteoMentions, "meteo.csv", row.names = FALSE)

# Add the exported data into a geopackage with GIS
# Read Data
Parishes <- st_read(
  dsn = here("Spatial/Parishes_Scotland.gpkg"))

# Edit the Count field
Parishes <- Parishes %>%
  mutate(meteo_NReportsM = ifelse(is.na(meteo_NReportsM), 0, meteo_NReportsM))%>% #transform NA into 0
  mutate(meteo_NReportsM=as.numeric(meteo_NReportsM))# read column as continuous

# Plot 
Parishes %>% # pipe data to
  ggplot() +# a ggplot function
  geom_sf(# precise that it will be a spatial geometry
    aes(# provide some aesthetics
      geometry = geom,# the geometry column (usually auto detected)
      fill = meteo_NReportsM)# we want the polygon color to change following the count
  )  +
  scale_fill_gradientn(colours=c("white", "red"),guide = guide_legend(title = "Meteo Report"))

# THE END ####
    