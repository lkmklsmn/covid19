library(RCurl)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dtw)
theme_set(theme_bw())

URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
data <- read.csv(text = URL, check.names = F)

# Plot world map ####
world <- ne_countries(scale = "medium", returnclass = "sf")
num_confirmed <- data.matrix(data[,-c(1:4)])
tmp <- data.frame(num_confirmed, data$Long, data$Lat)

plots <- lapply(1:ncol(num_confirmed), function(x){
  counts <- log10(num_confirmed[,x] + 1)
  
  day <- colnames(num_confirmed)[x]
  
  ggplot(data = world) + geom_sf() + xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = day) +
    geom_point(data= data,aes(x=Long, y=Lat), color = "darkblue", size = counts)
  
  ggsave(paste0('/Users/lukas.simon/Downloads/covid_world_', x, '.png'))
})

tmp <- data[, c('Long', 'Lat')]
clustering <- kmeans(tmp[,1], centers = 3)
location <- as.character(clustering$cluster)
location <- gsub('1', 'Asia', location)
location <- gsub('2', 'USA', location)
location <- gsub('3', 'Europe', location)

tmp <- data.frame(tmp, location)
ggplot(data = world) + geom_sf() + xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map") +
  geom_point(data = tmp,aes(x=Long, y=Lat, color = location)) + scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
ggsave('/Users/lukas.simon/Downloads/Worldmap.pdf')

ok <- which(apply(num_confirmed, 1, var) > 0)
tmp <- t(num_confirmed[ok,])
colnames(tmp) <- data$`Province/State`[ok]

correl <- cor(tmp)
rownames(correl) <- colnames(correl) <- paste0('x', 1:nrow(correl))
p <- pheatmap::pheatmap(correl)
cluster <- cutree(p$tree_col, k = 3)

anno <- data.frame(cluster = as.character(cluster))
anno <- data.frame(cluster = as.character(clustering$cluster)[ok])
rownames(anno) <- colnames(correl)
pheatmap::pheatmap(correl, annotation_row = anno)

asplit <- split(1:nrow(num_confirmed), location)
cluster_counts <- do.call(cbind, lapply(asplit, function(x) colSums(num_confirmed[x,])))

pdf('/Users/lukas.simon/Downloads/Spread_real_time.pdf')
plot(cluster_counts[,1], col = 0, ylim = c(0, max(cluster_counts)), xlab = 'Real time', ylab = '# confirmed infections')
lines(cluster_counts[,1], col = "#00AFBB", lwd = 2)
lines(cluster_counts[,2], col = "#E7B800", lwd = 2)
lines(cluster_counts[,3], col = "#FC4E07", lwd = 2)
legend('topleft', c('Asia', 'Europe', 'USA'), pch = 16, col = c("#00AFBB", "#E7B800", "#FC4E07"), bty = 'n')
dev.off()

alignment_europe <- dtw(cluster_counts[,2], cluster_counts[,1],
                 open.begin = T, keep = T, step = asymmetric, open.end = T)
alignment_usa <- dtw(cluster_counts[,3], cluster_counts[,1],
                        open.begin = T, keep = T, step = asymmetric, open.end = T)

pdf('/Users/lukas.simon/Downloads/Spread_aligned_time.pdf')
plot(alignment_europe$index1, t(cluster_counts[,1]), col = 0, xlab = 'Relative time', ylab = '# confirmed infections')
lines(alignment_europe$index1, t(cluster_counts[,1]), col = "#00AFBB", lwd = 2)
lines(alignment_europe$index2, t(cluster_counts[,2]), col = "#E7B800", lwd = 2)
lines(alignment_usa$index2, t(cluster_counts[,3]), col = "#FC4E07", lwd = 2)
legend('bottomright', c('Asia', 'Europe', 'USA'), col = c("#00AFBB", "#E7B800", "#FC4E07"), bty = 'n', pch = 19)
dev.off()

