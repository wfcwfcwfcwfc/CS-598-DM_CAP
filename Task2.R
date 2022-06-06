library(readr)
library(dplyr)
library(readtext)
library(stringr)
library(text2vec)
library(superheat)
library(readr)
setwd("C:/Users/wangf/iCloudDrive/OneDrive/UIUC/CS-598-DM/categories")
docs = readtext(paste0("C:/Users/wangf/iCloudDrive/OneDrive/UIUC/CS-598-DM/categories/", "*"))

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
}
docs$review_clean = prep_fun(docs$text)

it = itoken(docs$review_clean, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)
dim(dtm)
jac_sim = sim2(dtm, dtm, method = "jaccard", norm = "none")

jac_sim_mtx = as.matrix(jac_sim)
diag(jac_sim_mtx) <- NA

dimnames(jac_sim_mtx) = list(str_replace(docs$doc_id, ".txt", ""), str_replace(docs$doc_id, ".txt", ""))
#cos <- as.data.frame(summary(jac_sim))
jac_sim_df = data.frame(jac_sim_mtx)

plot = function(mat) {
superheat(mat[1:100, 1:100], 
          
       row.dendrogram = T, 
        col.dendrogram = T,
          # make gridlines white for enhanced prettiness
          grid.hline.col = "white",
          grid.vline.col = "white",
          
          # rotate bottom label text
          #bottom.label.text.angle = 90,
          legend = TRUE,
#          legend.breaks = c(-0.1, 0.1, 0.3, 0.5))

bottom.label.text.angle = 90,
bottom.label.text.alignment = "right",
bottom.label.text.size = 2,
bottom.label.size = 0.1,

# left labels
left.label.text.alignment = "right",
left.label.text.size = 2,
left.label.size = 0.1,
#left.label.col = str_replace(docs$doc_id, ".txt", "")
)
}


cuisine_sim_matrix <- read_csv("C:/Users/wangf/iCloudDrive/OneDrive/UIUC/CS-598-DM/cuisine_sim_matrix.csv", col_names=FALSE)
cuisine_sim_matrix = as.matrix(cuisine_sim_matrix)
diag(cuisine_sim_matrix) <- NA
dimnames(cuisine_sim_matrix) = list(str_replace(docs$doc_id[1:100], ".txt", ""), str_replace(docs$doc_id[1:100], ".txt", ""))
cuisine_sim_matrix = data.frame(cuisine_sim_matrix)
plot(cuisine_sim_matrix)
