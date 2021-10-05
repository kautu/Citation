
## 
component.ipc <- energy.triadic %>% 
  group_by(FC, IPC) %>% 
  summarise(n = n())

#
edges.sparse <- subset(node.centrality[,c(4,1,3)], membership %in% unique(c(community.triadic$membership_source, 
                                                                            community.triadic$membership_target))) %>% 
  mutate(category = substring(node, nchar(node))) %>% 
  mutate(membership = as.character(membership)) %>% 
  left_join(component.ipc, by = c('node' = 'FC')) %>%  ##
  left_join(category.cluster, by = c('membership' = 'node')) #%>% 
  #group_by(cluster, category, IPC) %>% 
  #summarise(n = n(), 
  #          eigen_centrality = mean(centr_eigen)) %>% 
  #arrange(cluster, -n)
  
energy.sparse <- subset(energy.triadic, FC %in% unique(c(edges.sparse$node))) %>% 
  group_by(FC, IPC) %>% 
  summarise(count = n()) %>% 
  left_join(node.centrality, by = c('FC' = 'node')) %>% 
  group_by(membership, IPC) %>% 
  summarise(n_c = n(),
            count = sum(count)) %>% 
  #arrange(membership, -n) %>% 
  mutate(membership = as.character(membership)) %>% 
  left_join(category.cluster, by = c('membership' = 'node')) %>% 
  group_by(Cluster, IPC) %>% 
  summarise(n = n(),
            sum = sum(n_c),
            ssum = sum(count)) %>% 
  arrange(Cluster, -ssum)

energy.sparse.summary <- energy.sparse %>% 
  group_by(Cluster) %>% 
  summarise(Class = sum(n),
            Community = sum(sum),
            Family = sum(ssum))

energy.sparse.top <- energy.sparse %>% 
  group_by(Cluster) %>% 
  top_n(25) %>% 
  left_join(energy.sparse.summary, by = 'Cluster') %>% 
  mutate(Percent = round(sum/Community, 4),
         Percentage = round(ssum/Family, 4),
         Cumulative = cumsum(Percentage)) 


write.csv(energy.sparse.top, 'cluster_character.csv', row.names = FALSE)

