#LET THE TREE DECIDE: CLASSIFYING FORESTS WITH A FOREST

#packages
install.packages(c("janitor", "ggplot2")) 
install.packages("scales")
install.packages("reshape2")
install.packages("randomForest")       
library(randomForest)
library(scales)     
library(reshape2)
library(janitor)
library(ggplot2)
library(dplyr)
library(scales)
library(rpart)
library(rpart.plot)

### rename 
forest = read.csv("/Users/danigomez/Downloads/Forest_Area.csv",
                  stringsAsFactors = FALSE)


forest = clean_names(forest)     

names(forest) = c(
  "country_id", "country_and_area",
  "forest_area_1990", "forest_area_2000", "forest_area_2010",
  "forest_area_2015", "forest_area_2020",
  "total_land_area_2020", "forest_ratio_2020",
  "deforestation_2015_2020", "fire_damage_2015"
)

#dropping world 
forest = forest[forest$country_and_area != "WORLD", ]

# numeric
num_cols = c(
  "forest_area_1990","forest_area_2000","forest_area_2010",
  "forest_area_2015","forest_area_2020",
  "total_land_area_2020","forest_ratio_2020",
  "deforestation_2015_2020","fire_damage_2015"
)

for (col in num_cols) {
  forest[[col]] = suppressWarnings(as.numeric(gsub(",", "", forest[[col]])))
}

summary(forest[, num_cols])
colSums(is.na(forest[, num_cols]))

forest_model = forest

#missing values

forest_model$deforestation_2015_2020[is.na(forest_model$deforestation_2015_2020)] =
  median(forest_model$deforestation_2015_2020, na.rm = TRUE)
forest_model$fire_damage_2015[is.na(forest_model$fire_damage_2015)] = 0

#visualizations 
install.packages("gridExtra")   
library(gridExtra)

##Histogram for the variables 
hist_list = list()                 

for (v in num_cols) {
  p = ggplot(forest_model, aes_string(x = v)) +
    geom_histogram(bins = 40, fill = "forestgreen") +
    theme_minimal() +
    labs(title = v, x = "", y = "Count")
  hist_list[[v]] = p
}

grid.arrange(grobs = hist_list, ncol = 3,
             top = "Distributions of key forest indicators")                

##box plots
box_list = list()

for (v in num_cols) {
  p = ggplot(forest_model, aes_string(x = "\"\"", y = v)) s
    geom_boxplot(outlier.colour = "red") +
    theme_minimal() +
    labs(title = v, x = "", y = "")
  box_list[[v]] = p
}

grid.arrange(grobs = box_list, ncol = 3,
             top = "Box‑plots (red dots mark potential outliers)")

#log tranformaation due to the large gap
size_vars = c("total_land_area_2020",
              "forest_area_1990","forest_area_2000","forest_area_2010",
              "forest_area_2015","forest_area_2020",
              "fire_damage_2015","deforestation_2015_2020")

for (col in size_vars) {
  forest_model[[col]] = log1p(forest_model[[col]])
}

##box‑plots after log transformation
box_log_list = list()

for (v in num_cols) {
  p = ggplot(forest_model, aes_string(x = "\"\"", y = v)) +
    geom_boxplot(outlier.colour = "red") +
    theme_minimal() +
    labs(title = paste(v), x = "", y = "")
  box_log_list[[v]] = p
}

grid.arrange(grobs = box_log_list, ncol = 3,
             top = "Box plots after log transformation")


#largest loss in forest 
forest_sorted = forest_model[order(forest_model$deforestation_2015_2020,
                                   decreasing = TRUE), ]
forest_sorted$label = ""
forest_sorted$label[1:10] = forest_sorted$country_and_area[1:10]

ggplot(forest_sorted,
       aes(x = forest_ratio_2020,
           y = deforestation_2015_2020,
           label = label)) +
  geom_point(colour = "forestgreen") +
  geom_text(hjust = -0.1, size = 3) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Forest Cover vs. Deforestation Rate (Top 10 Labelled)",
    x = "Forest Area as % of Land (2020)",
    y = "Deforestation Rate (log scale)"
  )

#top 15 absolute deforestation- top rates
forest_model$def_rate_pct = expm1(forest_model$deforestation_2015_2020)

top15 = forest_model[order(forest_model$def_rate_pct,
                           decreasing = TRUE), ][1:15, ]

ggplot(top15,
       aes(x = reorder(country_and_area, def_rate_pct),
           y = def_rate_pct)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 15 Deforestation Rates (2015–2020)",
       x = "", y = "Deforestation Rate")


##correlation 
corr_matrix = cor(forest_model[, num_cols],
                  use = "pairwise.complete.obs")
corr_df     = melt(corr_matrix)

ggplot(corr_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "white", high = "forestgreen", limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank()) +
  labs(title = "Correlation heat map",
       x = "", y = "", fill = "r")

###PCA 
X_scaled = scale(forest_model[, num_cols])
pca      = prcomp(X_scaled, center = FALSE, scale. = FALSE)
print(summary(pca))

pc_scores = as.data.frame(pca$x[, 1:3])


#elbow plot for k means 

wss = sapply(1:10, function(k) {
  kmeans(pc_scores[, c("PC1","PC2","PC3")], centers = k, nstart = 25)$tot.withinss
})
elbow_df = data.frame(k = 1:10, WSS = wss)

ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method",
       x = "Number of Clusters",
       y = "Total WSS") +
  theme_minimal()


#kmeans clustering = k= 3 
set.seed(1)
kmeans_result_3 = kmeans(pc_scores[, c("PC1","PC2","PC3")],
                         centers = 3, nstart = 25)


clustered_data_3 = data.frame(pc_scores)           
clustered_data_3$cluster = factor(kmeans_result_3$cluster)
clustered_data_3$country = forest_model$country_and_area

ggplot(clustered_data_3, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 2) +
  geom_text(aes(label = country), check_overlap = TRUE, size = 2) +
  labs(title = "KMeans Clustering (k = 3)") +
  theme_minimal()

#how many countries in the clusters 
table(clustered_data$cluster)


#clustering - k=4 
set.seed(1)
kmeans_result_4 = kmeans(pc_scores[, c("PC1", "PC2", "PC3")],
                         centers = 4, nstart = 25)

clustered_data_4 = as.data.frame(pc_scores)
clustered_data_4$cluster = as.factor(kmeans_result_4$cluster)
clustered_data_4$country = forest_model$country_and_area

plot_4 = ggplot(clustered_data_4, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  geom_text(aes(label = country), size = 2, hjust = -0.1, vjust = 0) +
  labs(title = "K-means Clustering (k = 4)",
       x = "PC1", y = "PC2", color = "Cluster") +
  theme_minimal()

print(plot_4)

#what is in each cluster 
forest_model$cluster = clustered_data_3$cluster

cluster_labels = c(
  "1" = "High Risk",
  "2" = "Medium Risk",
  "3" = "Low Risk"
)

#visualization of the cluster variables 
forest_model$cluster_named = cluster_labels[as.character(forest_model$cluster)]

cluster_summary = aggregate(
  forest_model[, num_cols],
  by = list(Cluster = forest_model$cluster_named),
  FUN = mean
)

cluster_long = melt(cluster_summary, id.vars = "Cluster")

cluster_long$Cluster = as.character(cluster_long$Cluster)

#colours
color_map = c(
  "High Risk"    = "darkgreen",
  "Medium Risk"  = "mediumseagreen",
  "Low Risk"     = "lightgreen"
)

#plot
ggplot(cluster_long, aes(x = Cluster, y = value, fill = Cluster)) +
  geom_col() + 
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = color_map) + 
  labs(
    title = "Average Variable Values by Cluster",
    x = "Cluster",
    y = "Mean Value",
    fill = "Cluster"
  ) +
  theme_minimal()


#subcluster= fire vs deforestation 

high_risk_df = forest_model[forest_model$cluster_named == "High Risk", ]

#kmeans
set.seed(1)
kmeans_result_rf = kmeans(high_risk_df[, c("deforestation_2015_2020", "fire_damage_2015")], centers = 2)

high_risk_df$subcluster_rf = ifelse(kmeans_result_rf$cluster == 1,
                                    "Deforestation-driven",
                                    "Fire-driven")

forest_model$subcluster_rf = NA
forest_model$subcluster_rf[match(high_risk_df$country_and_area, forest_model$country_and_area)] = high_risk_df$subcluster_rf

table(forest_model$subcluster_rf)

ggplot(high_risk_df,
       aes(x = deforestation_2015_2020,
           y = fire_damage_2015,
           color = subcluster_rf)) +
  geom_point(size = 2) +
  geom_text(aes(label = country_and_area),
            check_overlap = TRUE,
            size = 2, vjust = -0.5) +
  scale_color_manual(values = c(
    "Deforestation-driven" = "forestgreen",
    "Fire-driven"          = "red"
  )) +
  labs(
    title = "High Risk Cluster: Deforestation vs Fire" ,
    x     = "Deforestation 2015–2020",
    y     = "Fire Damage 2015",
    color = "Primary Driver"
  ) +
  theme_minimal()

##classification - part 2 of the model 

#classification tree
forest_model$cluster = as.factor(forest_model$cluster)
table(forest_model$cluster)


tree_model = rpart(cluster_named ~ ., 
                   data = forest_model[, c(num_cols, "cluster_named")], 
                   method = "class", 
                   cp = 0.01)

rpart.plot(tree_model)

summary(tree_model)

#how accurate?
predicted = predict(tree_model, type = "class")

#confusion matrix
table(Predicted = predicted, Actual = forest_model$cluster_named)

#accuracy
mean(predicted == forest_model$cluster_named)

#random forest 

set.seed(1)
myforest = randomForest(
  cluster ~ forest_area_1990 + forest_area_2000 + forest_area_2010 +
    forest_area_2015 + forest_area_2020 +
    total_land_area_2020 + forest_ratio_2020 +
    deforestation_2015_2020 + fire_damage_2015,
  ntree      = 500,        
  data       = forest_model, 
  importance = TRUE,         
  na.action  = na.omit       
)

myforest 
varImpPlot(myforest)    

#how accurate?

#WHICH COUNTRIES ARE AT RISK?

#undo log 
forest_model$def_rate_pct  = expm1(forest_model$deforestation_2015_2020)
forest_model$fire_km2      = expm1(forest_model$fire_damage_2015)

#top 5 overall deforestation rate 
idx_def = forest_model$subcluster_rf == "Deforestation-driven"
watch_def = forest_model[idx_def, c("country_and_area","def_rate_pct")]
watch_def = watch_def[order(watch_def$def_rate_pct, decreasing=TRUE), ]
watch_def = head(watch_def, 5)

#top countries for fire driven deforestiaion 
idx_fire = forest_model$subcluster_rf == "Fire-driven"
watch_fire  = forest_model[idx_fire, c("country_and_area","fire_km2")]
watch_fire  = watch_fire[order(watch_fire$fire_km2, decreasing=TRUE), ]
watch_fire  = head(watch_fire, 5)

#plot deforestation rate top countries 
ggplot(watch_def,
       aes(x = reorder(country_and_area, def_rate_pct),
           y = def_rate_pct)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  labs(
    title    = "Top 5 Deforestation Driven Countries",
    subtitle = "Ranked by Forest Loss Rate (2015–2020)",
    x        = NULL,
    y        = "Deforestation Rate (%)"
  ) +
  theme_minimal(base_size = 13)

# plot top fire driven 
ggplot(watch_fire,
       aes(x = reorder(country_and_area, fire_km2),
           y = fire_km2)) +
  geom_col(fill = "red") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Top 5 Fire Driven Countries",
    subtitle = "Ranked by Fire Damage (2015)",
    x        = NULL,
    y        = "Fire Damage (km²)"
  ) +
  theme_minimal(base_size = 13)


