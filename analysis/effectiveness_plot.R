##-----------------------------
## Visualising the effectiveness of closures
## in changing F based on various sources and
## resolutions of data
## For discussion with Paul
##-----------------------------

load("Combined_Results.RData")


## Calculate the percentage difference 
###################
library(dplyr)

avg2_4  <- combined %>% filter(year %in% 29:29) %>% 
group_by(scenario, pop, metric, timescale, basis, data_type, resolution) %>% summarise(value = mean(data, na.rm = T))

avg8_10 <- combined  %>% filter(year %in% 50:50) %>% 
group_by(scenario, pop, metric, timescale, basis, data_type, resolution) %>% summarise(value = mean(data, na.rm = T))

combined <- merge(avg2_4, avg8_10, by = c("scenario","metric","pop", "timescale", "basis", "data_type", "resolution"))

colnames(combined)[8:9] <- c("before", "after")

combined$diff <- ((combined$after - combined$before) / combined$before)  * 100

##################
library(ggplot2); theme_set(theme_bw())
library(viridis)

f_diff <- subset(combined, metric == "F" & basis == "high_pop")

## similar to current plot
ggplot(f_diff, aes(x = diff, y = data_type))+
    geom_point(aes(pch = timescale, col = resolution)) +
    facet_wrap(~ pop, ncol = 1)


## a quick regression tree to see which variables are most important
library(REEMtree)
library(rpart.plot)

f_diff$ftimescale <- factor(f_diff$timescale)
f_diff$fresolution <- factor(f_diff$resolution)
f_diff$fpop <- factor(f_diff$pop)

## unpruned tree with a maximum depth set for visualisation
rtree <- rpart(diff ~ fpop + ftimescale + data_type + fresolution,
               data = f_diff,
               control = list(minbucket = 2, minsplit = 2, cp = -1, maxdepth = 4))

## variable importance
barplot(rtree$variable.importance)
## so it goes fpop > fresolution > data_type > ftimescale 

## visualise the regression tree
blue2red <- colorRampPalette(c("blue","white","red"))
cols <- blue2red(100)
breaks <- seq(min(f_diff$diff) - 1, max(f_diff$diff) + 1, length=101)

col_index <- cut(rtree$frame$yval, breaks)

pdf("f_diff_tree.pdf")
prp(rtree,
    box.col = cols[col_index],
    faclen = 0,
    varlen = 0,
    do.par = FALSE,
    left = T,
    type = 4,
    clip.right.labs = FALSE)
dev.off()

## now order visualisation according to the main axes of change

## re-order so focus species is first
f_diff$fpop <- factor(f_diff$pop, levels = c("spp_3", "spp_1", "spp_2", "spp_4"))

pdf("f_diff_effectiveness.pdf", height = 6, width = 8)
ggplot(f_diff, aes(x = resolution, y = diff))+
    geom_line(aes(colour = data_type)) +
    facet_grid(timescale ~ fpop) +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_colour_manual("Data source", values = c("#d95f02", "#7570b3", "#1b9e77")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab("Spatial resolution (high to low)") +
    ylab("Percentage difference in F before and after closure")
dev.off()
