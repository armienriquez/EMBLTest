library(tidyverse)

raw_cts <- read_csv("data_rnaseq/counts_raw.csv")
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")
test_results <- read_csv("data_rnaseq/test_result.csv")

# Convert from wide to long format which ggplot prefers
trans_cts_long <- trans_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1: mut_180_r3) 

# Join information from sample_info and trans_cts_long and overwrite the trans_cts_long object
trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")

# Create a plot that shows a histogram of cts which is not very useful
trans_cts_long %>% 
  ggplot(aes(x = cts)) + 
  geom_freqpoly()

# Color by replicate
trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) + 
  geom_freqpoly(binwidth = 1) # Bins are 0 to 1, 1 to 2 and so on

# Separate by strain and time
trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) + 
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

# convert the raw_cts created above to a long format using pivot_longer() 
# Produce a similar plot for the raw count data (hint: log transform the data)
# Try out other ways to visualize these data for example as a boxplot

raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1: mut_180_r3) 

raw_cts_long <- full_join(raw_cts_long, sample_info, by = "sample")

raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) + 
  geom_freqpoly() +
  scale_x_log10() +
  facet_grid(rows = vars(strain), cols = vars(minute))

raw_cts_long %>% 
  ggplot(aes(x = log10(cts), colour = replicate)) + 
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

# R removed infinite values which cannot be plotted
# If you add 1 to all of the counts, these data would have been included 
# The result would not be largely different, this is just for plotting
raw_cts_long %>% 
  ggplot(aes(x = log10(cts+1), colour = replicate)) + 
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

# boxplot instead of frequency polygon, needs both x and y to be defined
raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y=log10(cts + 1), fill = strain)) + 
  geom_boxplot() +
  facet_grid(cols = vars(replicate))

# Correlation between wt sample at T0 and T30
# It's more useful to use the wide table because the information
# is already in separate columns
trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point() +
  geom_abline(color = "brown") #This places a diagonal line

# The dots are more compact near the diagonal because
# these are just different different replicates at the same time
trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point() +
  geom_abline(color = "brown")

# To look at the correlation of count data across all samples in our experiment
trans_cts_corr <- trans_cts %>% 
  select(-gene) %>%             # This removes the gene column
  cor(method = "spearman")       # This calculates correlations, product is a matrix

library(corrr)
# In case you need to install, use install.packages("corrr")

# Make a heatmap
rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = -90, hjust = 1))                         
#This changes the angle of x label and right-aligns it

# Compare trans_cts and raw_cts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

# The raw and transformed data have very different scales
# There must have been some log transformations done

raw_cts %>% 
  ggplot(aes(x= wt_0_r1, y=wt_0_r2)) +
  geom_point()

# Make a plot that contains log transformations
raw_cts %>% 
  ggplot(aes(x = wt_0_r1 + 1, y = wt_0_r2 + 1)) +
           geom_point() + 
           scale_x_continuous(trans = "log2") +
           scale_y_continuous(trans = "log2")
         
# Make a plot with means on the x and variance on the y
# We will use the long format
raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(colour = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")
# Log2 transformed data will highlight smaller changes 
# with respect to log10-transformed

# The transformed data shows a more normalized distribution vs. the raw
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() 

# Colors the points with high variance above 4
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  mutate(above_four = var_cts > 4) %>% 
  ggplot(aes(x = mean_cts, y = var_cts, colour = !above_four)) +
  geom_point()

#######
# Transpose to matrix, it's a 2-dimensional object
pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>%      
  as.matrix() %>%            # converts our data frame to a matrix
  t()                        # Transpose function is just named t

# PCA function is part of R and doesn't need to be loaded
sample_pca <- prcomp(pca_matrix)

class(sample_pca)       #Result is that it's a PCA computation result "prcomp"
str(sample_pca)         #Result is the structure which includes 
                        #sdev, rotation, center, center, scale, x and their types & attributes
summary(sample_pca)     #Shows the values of the PCA components starting from the 
                        #biggest contribution to the variance

pca_matrix[1:10, 1:5]

as_tibble(pca_matrix)   #We lost the sample names
as_tibble(pca_matrix, rownames="sample")

pc_eigenvalues <- sample_pca$sdev^2    #We square the standard deviations to get the eigenvalues

# Convert into a tibble because we are going to use ggplot 
# which works with tibbles or data frames
pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))       #cumsum is cumulative sum

# Make a Pareto chart
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y=pct)) +
  geom_line(aes(y=pct_cum, group=1)) +
  geom_point(aes(y=pct_cum)) +
  geom_hline(aes(yintercept=80))+
  labs(x  = "Principal component", y = "Fraction of variance explained")

# justcurious <- summary(sample_pca)
# The PCA result is a complex object, which is why we have to extract 
# information in turn and convert into tibble

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

#Plot the first two dimensions on the xy scale
pc_scores %>% 
  ggplot(aes(x=PC1, y=PC2)) +
  geom_point()

pca_plot <- pc_scores %>% 
  full_join(sample_info, by="sample") %>% 
  ggplot(aes(x=PC1, y=PC2,
         color = factor(minute),
         shape = strain)) +
  geom_point()
# The plot tells us that time seems to be the biggest factor
# especially in the initial 15 and 30 minutes
# But at some point the time doesn't matter
# There is no difference between 120 and 180 minutes

# Get the top genes with the biggest contributions to PC1 and PC2
pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames="gene")

top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2), #This part draws the segments or lines
               arrow = arrow(length = unit(0.1, "in")), 
               color = "mediumorchid4") +
  geom_text(aes(x = PC1, y=PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))
  
# The longer the segment, the more that the gene contributes
# Learn more about PCA 
# https://www.youtube.com/watch?v=PFDu9oVAE-g&pp=ygURM2JsdWUxYnJvd24gZWlnZW4%3D

# Join plots together
library(patchwork)

(pca_plot | loadings_plot) #side by side

(pca_plot / loadings_plot) #stacked vertically

# three plots on top and one at the bottom with labels for each
(pca_plot | pca_plot |pca_plot) / loadings_plot + 
  plot_annotation(tag_levels = "A")
# of course you would show other PCs on the other two plots
# and you can do other formats that you like

library(ggfortify)
autoplot(sample_pca)

# This is a shortcut to make the PCA plot
autoplot(sample_pca, data=sample_info, 
         colour ="minute", shape="strain")

# Broom tidies up whatever but we didn't elaborate today
library(broom)

# this is a shortcut alternative to lines 164 to 167
tidy(sample_pca, matrix ="eigenvalues")

# this is a shortcut alternative to lines 202 to 203 
tidy(sample_pca, matrix = "loadings")


autoplot(sample_pca, 
         data=sample_info %>% 
           mutate(minute = as.factor(minute)), 
         colour ="minute", shape="strain")
# The plot looks circular, clockwise according to time

# Differential expression results
test_results 

# gene column = gene name
# baseMean column = normalized expression level of a gene
# log2FoldChange column = amount of change between 2 time points
# lfcSE column = standard error associated with log2FoldChange
# stat column = statistics value computed as log2FoldChange / lfcSE 
#               compared to standard normal distribution
# pvalue column = p-value associated with the change
# padj column = p-value corrected for multiple hypothesis testing
# comparison column = comparison group

# Generate MA plot: base mean vs. log2foldchange
# Organize panels by comparison (time point) i.e. one scatterplot per time point
# Hint: consider log transform baseMean

test_results %>% 
  ggplot(aes(x = log10(baseMean), y=log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))

# Color the points that are more significant (padj < 0.01)
# Up-regulated genes are above the line and down-regulated genes are below
ma_plot <- test_results %>% 
  mutate(sig = ifelse( padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x = log10(baseMean), y=log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color="dodgerblue") +
  geom_point(aes(y = sig), color="tomato", size = 1) +
  facet_wrap(facets = vars(comparison))

ma_plot|pca_plot
# So from these plots the biggest changes would be seen at time=30

# Visualizing expression trends
# 1. Compile a list of candidate genes (padj < 0.01) 
candidate_gene <- test_results %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>%   #Extracts one column and turns it into a vector, same as test_results[,"gene"] or test_results$gene
  unique()

str(candidate_gene)

# 2. Filter trans_cts_long for candidate genes and compute mean expression value
#    for each gene in each time period and each genotype

trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene,strain, minute) %>% 
  summarize(mean_cts = mean(cts), nrep = n()) %>%    #n function works only in specific functions
  ungroup()

# 3. Plot trends
trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

# Scaling data to improve visualization
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = (cts - mean(cts))/sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled = mean(cts_scaled), nrep = n()) %>%
  ungroup()

trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "brown", linetype= "dashed") +
  facet_grid(rows = vars(strain)) + 
  scale_x_continuous(breaks = unique(trans_cts_mean$minute))
# This plot hints that there are groups of genes that are expressed together
# Suggests that we want to do a clustering analysis
# The suggestion is to do different methods but for this example, we will just do one

# Clustering
# 1. Create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

# assign row names as gene names
rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_gene,]  #keeping only the candidate genes

hclust_matrix <- hclust_matrix %>% 
  t() %>%            # matrix transpose, columns to rows and vice versa
                     # so genes are columns instead of sample   
  scale() %>%        # genes are all expressed differently e.g. housekeeping genes are expressed more
                     # so we need to scale the gene expressions
                     # scale function only works for matrices
                     # calculate the z score for each gene           
  t()                # then transpose the matrix again to bring back the original rows and columns

# Calculate the pair-wise distances
gene_dist <-dist(hclust_matrix)

# Hierarchical clustering
gene_hclust <- hclust(gene_dist, method = "complete")

plot(gene_hclust, labels = F) #remove the labels
# The plot looks like a tree. How to remove the clusters?
abline(h=10, col = "brown", lwd = 2) # The line makes 5 clusters

# Make clusters based on the number that I want
cutree(gene_hclust, k = 5)
# Outputs a vector with the cluster number / membership of a gene in a cluster

gene_cluster <- cutree(gene_hclust, k=5) %>% 
  enframe()   %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene)) + 
  facet_grid(cols = vars(cluster), row = vars(strain))
# What would happen if the number of clusters change?

# To install 
# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)


