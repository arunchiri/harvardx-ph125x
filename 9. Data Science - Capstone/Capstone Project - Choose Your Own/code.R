##########################################
############## DOWNLOAD DATA #############

if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")

# scrape dataset from the wall street journal article
url <- "http://online.wsj.com/public/resources/documents/info-Degrees_that_Pay_you_Back-sort.html?mod=article_inline#top"
h <- read_html(url)
nodes <- h %>% html_nodes('table')

# locate table of interest from nodes "xml_nodeset"
tab <- nodes[[7]]
raw_data <- html_table(tab)
head(raw_data, 3)

rm(h, nodes, tab, url)

##########################################
############### WRANGLE DATA #############

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# transform the data
colnames(raw_data) <- c("College.Major", "Starting.Median.Salary", "Mid.Career.Median.Salary", "Career.Percent.Growth", "Percentile.10", "Percentile.25", "Percentile.75", "Percentile.90" )
raw_data <- raw_data[-1,]
rownames(raw_data) <- 1:nrow(raw_data)

# clean up the data 
degrees <- raw_data %>% 
  mutate_at(vars(Starting.Median.Salary: Percentile.90), function(x) as.numeric(gsub('[\\$,]',"",x))) %>%
  mutate(Career.Percent.Growth = Career.Percent.Growth / 100)

rm(raw_data)

##########################################
############### DATA ANALYSIS ############

# Load relevant packages
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

# Elbow method
# select and scale the relevant features and store as k_means_data
k_means_data <- degrees %>%
  select(Starting.Median.Salary, Mid.Career.Median.Salary, Percentile.10, Percentile.90) %>% 
  scale()
# run the fviz_nbclust function with selected data and method "wss"
elbow_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "wss")
elbow_method + theme_fivethirtyeight() + 
               theme(axis.title = element_text()) + 
               xlab('\nNumber of clusters k') + 
               ylab('Total Within Sum of Square\n')

# Silhouette method
# run the fviz_nbclust function with the method "silhouette" 
silhouette_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "silhouette")
silhouette_method + theme_fivethirtyeight() + 
                    theme(axis.title = element_text()) + 
                    xlab('\nNumber of clusters k') + 
                    ylab('Average silhouette width\n')

# Gap Statistic method
# use the clusGap function to apply the Gap Statistic Method
gap_stat <- clusGap(k_means_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
# use the fviz_gap_stat function to vizualize the results
gap_stat_method <- fviz_gap_stat(gap_stat)
gap_stat_method + theme_fivethirtyeight() + 
                  theme(axis.title = element_text()) + 
                  xlab('\nNumber of clusters k') + 
                  ylab('Gap Statistic (k)\n')

# K-means algorithm
# set a random seed
suppressWarnings(set.seed(111, sample.kind = 'Rounding'))
# set k equal to the optimal number of clusters
num_clusters <- 3
# run the k-means algorithm 
k_means <- kmeans(k_means_data, centers = num_clusters, iter.max = 15, nstart = 25)
# label the clusters of degrees
degrees_labeled <- degrees %>%
  mutate(clusters = k_means$cluster)

# Visualizing the clusters
# graph the clusters by Starting and Mid Career Median Salaries
career_growth <- ggplot(degrees_labeled, aes(x = Starting.Median.Salary, y = Mid.Career.Median.Salary, color=factor(clusters))) + 
  geom_point(alpha = 4/5, size = 7) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_color_manual(name = "Clusters", values = c("#EC2C73", "#29AEC7", "#FFDD30"))
career_growth + theme_fivethirtyeight() + 
                theme(axis.title = element_text()) + 
                xlab('\nStarting Median Salary') + 
                ylab('Mid Career Median Salary\n')

# A deeper dive into the clusters
# use gather function to reshape degrees and use mutate() to reorder the new percentile column
degrees_perc <- degrees_labeled %>%
  select(College.Major, Percentile.10, Percentile.25, Mid.Career.Median.Salary, Percentile.75, Percentile.90, clusters) %>%
  gather(key=percentile, value=salary, -c(College.Major, clusters)) %>%
  mutate(percentile = factor(percentile, levels = c("Percentile.10", "Percentile.25", "Mid.Career.Median.Salary", "Percentile.75", "Percentile.90")))

# The liberal arts cluster
# graph the majors of Cluster 1 by percentile
cluster_1 <-  ggplot(degrees_perc %>% filter(clusters == 1), aes(x=percentile, y=salary, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)
cluster_1 + theme_fivethirtyeight() + labs(color = "College Major") + 
            theme(axis.title = element_text()) + 
            xlab('\nPercentile') + 
            ylab('Salary\n')

# The goldilocks cluster
# graph the majors of Cluster 2 by percentile
cluster_2 <-  ggplot(degrees_perc %>% filter(clusters == 2), aes(x=percentile, y=salary, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)
cluster_2 + theme_fivethirtyeight() + labs(color = "College Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salary\n')

# The over achiever cluster
# graph the majors of Cluster 3 by percentile
cluster_3 <-  ggplot(degrees_perc %>% filter(clusters == 3), aes(x=percentile, y=salary, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7)) + 
  scale_y_continuous(labels = scales::dollar)
cluster_3 + theme_fivethirtyeight() + labs(color = "College Major") + 
  theme(axis.title = element_text()) + 
  xlab('\nPercentile') + 
  ylab('Salary\n')

# sort degrees by Career.Percent.Growth
degrees_sorted <- degrees_labeled %>% arrange(desc(Career.Percent.Growth))
degrees_sorted %>% as_tibble()

