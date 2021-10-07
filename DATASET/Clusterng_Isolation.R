
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




##
energy.isolate <- subset(energy.triadic, !FC %in% unique(c(edges.sparse$node))) %>% 
  group_by(FC, IPC) %>% 
  summarise(count = n()) %>% 
  left_join(node.centrality, by = c('FC' = 'node')) %>% 
  group_by(membership, IPC) %>% 
  summarise(num = n(),
            count = sum(count)) %>% 
  filter(is.na(membership) == FALSE ) %>% 
  mutate(membership = as.factor(membership)) %>% 
  arrange(membership, -count) %>% 
  slice_max(count, n = 3) %>% 
  #left_join(energy.sparse, by = 'IPC')
  filter(!IPC %in% unique(energy.sparse$IPC))

isolate.community <- data.frame(energy.isolate[,1]) %>% 
  distinct(membership)
colnames(isolate.community) <- 'name'
isolate.community$Class <- 'Community'

isolate.ipc <- data.frame(energy.isolate[,2]) %>% 
  distinct(IPC) 
colnames(isolate.ipc) <- 'name'
isolate.ipc$Class <- 'IPC'

isolate.vertices <- rbind(isolate.community, isolate.ipc) %>% 
  mutate(Class = as.factor(Class))

isolate.graph <- graph_from_data_frame(energy.isolate, vertices = isolate.vertices, directed = FALSE)


CairoPDF('isolation.pdf', 23.96, 10.888)

ggraph(isolate.graph, layout = 'tree'  # 'sugiyama'  
       ) +
  geom_edge_link(color = "lightgrey")+ 
  geom_node_point(aes(colour = Class)) +
  geom_node_text(aes(label = name, colour = Class), size = 2, repel = TRUE)+
  #scale_color_discrete() +
  theme_graph()

dev.off()
