#### ---- Project: APHRC Wash Data ----
#### ---- Task: Clustering ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 13 (Wed) ----

library(tidyr)
library(dplyr)
library(ggplot2)
library(cluster)
library(Rtsne)

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7902)

#Since the clustering data contains both categorical and numeric variables, we can not therefore use the k-modes clustering. K-medoids clustering allows both numeric and categorical data.

#The goal is to find k representative objects which minimize the sum of the dissimilarities of the observations to their closest representative object.

#### ---- Determine k ----
factors <- function(x){
	factor(x, levels = c(0, 1), labels = c("Unimproved", "Improved"))
}

cluster_vars <- colnames(working_df)[!colnames(working_df) %in% c("total_wash_indicators", "wash_access_rate")]

cluster_df <- (working_df
	%>% select(cluster_vars)
	%>% mutate_at(wash_vars, funs(factors))
	%>% as.data.frame()
)
cluster_df <- cluster_df[sample(1:nrow(working_df), 8000),]

var_levels <- as.data.frame(sapply(cluster_df
		, function(x) length(levels(x))
	)
)
colnames(var_levels) <- "levels"
bin_vars <- row.names(var_levels)[var_levels[["levels"]]==2]
bin_vars_pos <- match(bin_vars, colnames(cluster_df))
#bin_vars_pos <- sapply(bin_vars, function(x) grep(x, colnames(cluster_df)))

#### ---- Clustering ----

gower_distance <- daisy(cluster_df
	, metric = "gower"
#	, type = list(symm = c(bin_vars_pos))
)

# Selecting the number of clusters: use silhoutte width
# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_distance,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# cluster number: pick clusters with high silhoutte width
max_sil_width <- max(sil_width[!is.na(sil_width)==T])
k <- match(max_sil_width, sil_width)

# plot of k vs silhoutte width
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# perform clustering
#k <- which.max(sil_width)
k <- 4
pam_fit <- pam(gower_distance, diss = TRUE, k = k)

# add cluster variable to the datast
cluster_df$cluster <- pam_fit$clustering

cluster_df$cluster2 <- factor(pam_fit$clustering, levels = 1:k,
                                    labels = paste("Cluster",1:k, sep=""))


# create t-SNE object - 2 dimensions
tsne_obj <- Rtsne(gower_distance, is_distance = TRUE)

# extract the 2 dimenstions
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

# viasulaize the clusters ona graph
print(ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) + theme_minimal()
)
