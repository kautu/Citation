
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
  left_join(category.cluster, by = c('membership' = 'node')) %>% #
  group_by(cluster, category, IPC) %>% 
  summarise(n = n(), 
            eigen_centrality = mean(centr_eigen)) %>% 
  arrange(cluster, -n)
  
