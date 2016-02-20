ibrary(dplyr)
library(ggplot2)


df <- tbl_df(read.csv('../data/text_answers_in_word2vec_space.csv', header = TRUE,
               stringsAsFactors = FALSE))
df <- na.omit(df)
write.csv(df, file = '../data/text_answers_in_w2v_no_nans.csv', row.names = FALSE)
load('../data/capr.RData')
meta <- tbl_df(capr)


# Read in t-SNE 
tsn <- tbl_df(read.csv('../data/final_coords_pca45_perp15.csv', header = FALSE,
                       stringsAsFactors = FALSE))

text_stem <- function(x) {
    y <- x[1:5]
    y <- y[!is.na(y)]
    if(length(y) > 1) return(paste(y, collapse = ' '))
    else return(y)
    }


text_size = 1

# Ballot I
colnames(tsn) <- c("question_id", "caseid", "x", "y")
tsn_b1 <- filter(tsn, question_id == "I1")

out <-  mutate(meta, 
               text_lab = sapply(strsplit(I1, ' '), text_stem)) %>%
    right_join(tsn_b1, by='caseid')

ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=ThermAngHap.int), size=text_size) +
    theme_bw()
ggsave('I1_tsne_angry_happy.pdf', device="pdf")
ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=ThermWorHop.int), size=text_size) +
    theme_bw()
ggsave('I1_tsne_worry_hopeful.pdf', device="pdf")
ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=ThermAshPrd.int), size=text_size) +
    theme_bw()
ggsave('I1_tsne_ashamed_proud.pdf', device="pdf")


# Ballot II
tsn_II1 <- filter(tsn, question_id == "II1")
out <-  mutate(meta, 
               text_lab = sapply(strsplit(II1, ' '), text_stem)) %>%
    right_join(tsn_II1, by='caseid')
ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=II1A), size=text_size) +
    theme_bw()
ggsave('II1_proud.pdf', device="pdf")


tsn_II2 <- filter(tsn, question_id == "II2")
out <-  mutate(meta, 
               text_lab = sapply(strsplit(II2, ' '), text_stem)) %>%
    right_join(tsn_II2, by='caseid')
ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=II2A), size=text_size) +
    theme_bw()
ggsave('II2_angry.pdf', device="pdf")

tsn_II3 <- filter(tsn, question_id == "II3")
out <-  mutate(meta, 
               text_lab = sapply(strsplit(II3, ' '), text_stem)) %>%
    right_join(tsn_II3, by='caseid')
ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=II3A), size=text_size) +
    theme_bw()
ggsave('II3_worried.pdf', device="pdf")

tsn_II4 <- filter(tsn, question_id == "II4")
out <-  mutate(meta, 
               text_lab = sapply(strsplit(II4, ' '), text_stem)) %>%
    right_join(tsn_II4, by='caseid')
ggplot(out) + 
    geom_text(aes(x=x, y=y, label=text_lab, color=II4A), size=text_size) +
    theme_bw()
ggsave('II4_hopeful.pdf', device="pdf")





# Make df for classification task
w2v <- tbl_df(read.csv('../data/text_answers_in_w2v_no_nans.csv',
                       header = TRUE, stringsAsFactors = FALSE))

write.table(meta, file = '../data/capr_data.tsv', sep='\t', row.names = FALSE)

b1_w2v <- filter(w2v, question_id == "I1")
colnames(b1_w2v)[2] <- 'caseid'

out_2 <- left_join(b1_w2v, meta, by='caseid')
write.csv(out_2, file = '../data/w2v_sentiment_ballot_1.csv', row.names = FALSE)


