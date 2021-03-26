# BCB546-R-assignment
Contains R markdown file and README file with files generated using code

## Work Flow steps:
  1. Download the files directly from git hib repository and read as tsv files
  2. Data Insepections tools - str(), head(), dim()
  3. Data processing:
        # Part I 
          1. transpose and edit Fang et al file as data frame, remove sample names. Also just select SNP_ID, Chromosome, Position from SNP file and finally merge both file-merge_fangSNP by SNP_ID
          2. Create files based on groups for Maize (ZMMIL, ZMMLR, ZMMMR) and Teosinate (ZMPBA, ZMPIL, ZMPJA) from the merged file
          3. transpose and process the new data set for Maize and Teosinate to have them ready for further analysis
          4. create 3 separate directories to store all the files and graphs
          5.  The files are already selected for the required group so we can use replace the missing values ? with - and name it Maize_replaced. 
          This file can be used to sort in the descending order.
          7. Sort the files in asceding order using arrange function() and specifying as.numeric for position
          8. Sort the files in descensing order using arrange funtion(arrange(desc))using __replaced files
          9.  Thus now the files are ready to be sorted according to chromosome number 1-10
          10. Use for loop to filter the merged files based on chromosome number and them write_tsv function to store them

     # Part II
          1. Data Visualization processing: use filter function to remove missing ,unknown values from SNP file
          2. use ggplot to generate different graphs as required
---
R code

title: "BCB 546 R assignment"
author: "Prita"
date: "3/18/2021"
output:
  word_document: default
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# clear global environment
rm(list = ls())

# Load require packages
library(tidyverse)
library(dplyr)


```

```{r}
# download data files
fang <- read_tsv("https://raw.githubusercontent.com/EEOB-BioData/BCB546-Spring2021/main/assignments/UNIX_Assignment/fang_et_al_genotypes.txt")
head(fang)

SNP <- read_tsv("https://raw.githubusercontent.com/EEOB-BioData/BCB546-Spring2021/main/assignments/UNIX_Assignment/snp_position.txt")
head(SNP)

```

```{r}
# data inspection


str(fang)
str(SNP)

dim(fang)
dim(SNP)

class(fang)
class(SNP)

fang[1:10]
SNP[1:10]

# Summary table for getting counts by chromosome
as.data.frame(table(SNP$Chromosome, deparse.level = 2), responseName = 'Counts')

```

```{r}
# data processing I


# transpose the fang et al file as data frame 
tfang <- as.data.frame(t(fang))

# Make sample names to column names
names(tfang) <- lapply(tfang[1, ], as.character)

# Remove sample names in rows
tfang <- tfang[-1,]

# To name the column/variable of the row names
tfang <- rownames_to_column(tfang, var="SNP_ID")

# create SNP file with SNP_ID, Chromosome and position
SNP_edit <- SNP %>% select(SNP_ID, Chromosome, Position)

# table for getting counts by chromosome
#as.data.frame(table(SNP$Chromosome, deparse.level = 2), responseName = 'Counts')

# Merge both files by SNP_ID
merged_fangSNP <- merge(SNP_edit,tfang, by.x="SNP_ID", by.y="SNP_ID", all = TRUE)

# Use as_tibble or head()[range of columns] to visualize data

as_tibble(merged_fangSNP)        


# select groups

## Maize - ZMMIL, ZMMLR, ZMMMR

Maize <- filter(fang, Group %in% c("ZMMLR", "ZMMMR", "ZMMIL"))
Maize_name <- Maize[,c(1:3)]
Maize_fullname <- apply(Maize_name, 1, paste, collapse = ", ") 

## Teosinate - ZMPBA, ZMPIL, ZMPJA

Teosinate <- filter(fang, Group %in% c("ZMPBA", "ZMPIL","ZMPJA"))
Teosinate_name <- Teosinate[,c(1:3)]
Teosinate_fullname <- apply(Teosinate_name, 1, paste, collapse =",")

# edit maize files - transpose and merge

tMaize<- as.data.frame(t(Maize[,c(-1:-3)]))
colnames(tMaize) <- Maize_fullname
rnames <- as.data.frame(rownames(tMaize))
rownames(tMaize) <- NULL
tMaize <- cbind(rnames,tMaize)
colnames(tMaize)[1] <- "SNP_ID"

# check the edited maize file 
as_tibble(tMaize)

merge_Maize <- merge(SNP_edit,tMaize, by = "SNP_ID")

as_tibble(merge_Maize)


# edit Teosinate files - transpose and merge

tTeosinate<- as.data.frame(t(Teosinate[,c(-1:-3)]))
colnames(tTeosinate) <- Teosinate_fullname
rnames <- as.data.frame(rownames(tTeosinate))
rownames(tTeosinate) <- NULL
tTeosinate <- cbind(rnames,tTeosinate)
colnames(tTeosinate)[1] <- "SNP_ID"

# check edited teosinate file

as_tibble(tTeosinate)

merge_Teosinate <- merge(SNP_edit, tTeosinate, by = "SNP_ID")

as_tibble(merge_Teosinate)


```

```{r}

## Data processing I
# Editing files 

library(tidyverse)
library(dplyr)

# create directories
dir.create("./Maize_files")
dir.create("./Teosinate_files")
dir.create("./Graphs_files")

## replace the missing values by ? and - for decreasing order files

Maize_replaced <- merge_Maize %>% mutate_all(function(x) gsub("\\?", "\\-",x))
Teosinate_replaced <- merge_Teosinate %>% mutate_all(function(x) gsub("\\?", "\\-",x))

## sorting in ascending 

merge_Maize <- merge_Maize %>% arrange(.,as.numeric(Position))
merge_Teosinate <- merge_Teosinate %>% arrange(.,as.numeric(Position))

#merge_Maize <- merge_Maize[order(merge_Maize$Position)]
#merge_Teosinate <- merge_Teosinate[order(merge_Teosinate$Position)]


# sorting in descending (use _replaced files to sort in descending order)
Maize_replaced <- Maize_replaced %>% arrange(.,desc(as.numeric(Position)))
Teosinate_replaced <- Teosinate_replaced %>% arrange(.,desc(as.numeric(Position)))

# Maize_replaced <- Maize_replaced[order(-merge$Position)]
# Teosinate_replaced <- Teosinate_replaced[order(-merge$Position)]


```


```{r}

# Using for loop to create each - 10 chromosome files, write as tsv and save into the respective directory
for (i in 1:10) {
  Maize_Asc <- filter(merge_Maize, Chromosome == i)
  Maize_Desc <- filter(Maize_replaced, Chromosome == i)
  
  write_tsv(Maize_Asc, file.path("./Maize_files", paste("Maize_Chr",i,"increasing.txt", sep = "_")))
  write_tsv(Maize_Desc, file.path("./Maize_files", paste("Maize_Chr",i, "decreasing.txt", sep = "_")))
  
  Teosinate_Asc <- filter(merge_Teosinate, Chromosome == i)
  Teosinate_Desc <- filter(Teosinate_replaced, Chromosome == i)
  
  write_tsv(Teosinate_Asc, file.path("./Teosinate_files", paste("Teosinate_Chr",i,"increasing.txt" , sep = "_")))
  write_tsv(Teosinate_Desc, file.path("./Teosinate_files" , paste("Teosinate_Chr",i, "decreasing.txt", sep = "_")))
  
}

```

```{r}
## Data Visulization

# Total SNPs per chromosome 
# added as per reviewer suggestion -as.double sorts chromosome from 1:10; as.factor transform each chromosome into a factor 

filtered_fangSNP <- filter(merged_fangSNP , Position != "multiple",  Position != "unknown")

ggplot(data = filtered_fangSNP) +
  geom_bar(mapping = aes(x = as.factor(as.double(Chromosome)))) + 
  ggtitle(label = "SNPs per chromosome") +
  xlab(label = "Chromosome") +
  ylab(label = "Number of SNPs") +
  theme_bw()

ggsave(filename = "./Graphs_files/SNPs per chromosome.png", device = "png")

## Generate density plot to show distribution

#ggplot(filtered_fangSNP, aes(x= as.numeric(Position))) + geom_density(aes(fill = Chromosome)) + facet_wrap(~ Chromosome) + theme_linedraw() + ggtitle(label = " Distribution of SNPs on chromosome") + labs(x = "Position", y = "Density")


# Generate density plot to show distribution
ggplot(filtered_fangSNP, aes(x= as.numeric(Position)/1000000)) + 
  # Position in megabases instead of bp
  
  geom_density(aes(fill = as.factor(as.double(Chromosome)))) + # sort Labels for Chr on the right side panel from 1:10
  
  facet_wrap(~ as.double(Chromosome), scales = "free_x") + 
  # sort 10 plots from 1 to 10, scales = "free_x" allows plotting each chromosome from their own begining to their own end (instead of from 0 to max(filtered_fangSNP$Position))
  
  theme_linedraw() +
  labs(x = "Position (Mb)", y = "Density", fill = "Chromosome") 
# change label for the legend on the right side

ggsave(filename = "./Graphs_files/SNP Distribution.png" , device = "png")


```

```{r}

## Data processing II for further grouping
tidy_fang <- fang %>% select(-JG_OTU) %>% 
  pivot_longer( -Sample_ID:-Group, names_to = "SNP_ID", values_to = "Sequence")

tidy_fang <- tidy_fang %>% 
  mutate(new_sequence = ifelse(Sequence %in% c("A/A","T/T","C/C","G/G"), "Homozygous", 
  ifelse(Sequence == "?/?", "Missing","Heterozygous")))

## SNPs/ Zygosity in sample

ggplot(data = tidy_fang, mapping = aes(x= Sample_ID, fill = new_sequence)) + 
  geom_bar(position = "stack") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ggtitle(label = "Zygosity per sample")+
  labs(X = "Sample_ID" , y = "Counts")

ggsave(filename = "./Graphs_files/Zygozity per sample.png" , device = "png")


## SNPs / Zygosity in different groups

ggplot(tidy_fang, aes(x = Group , fill = new_sequence)) + geom_bar(position = "fill") + 
  theme_bw() + theme(axis.text.x = element_text(size = 5, angle = 90)) + ggtitle(label = "Zygosity in different groups") + labs(y = "Proportion")

ggsave(filename = "./Graphs_files/Zygosity-SNP proportion in different groups.png" , device = "png" )


```

```{r}
#  To check proportion of all genotypes in sample - my interest 

ggplot(filter(tidy_fang, Sequence != "?/?") , aes(x = Sample_ID, fill = Sequence)) + 
  geom_bar(position = "fill") + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + ggtitle(label = "Genotypes in each sample") +labs(x = "Sample ID", y = "Proportion")

ggsave(filename = "./Graphs_files/Genotypes in each sample.png" , device = "png")


```



```

