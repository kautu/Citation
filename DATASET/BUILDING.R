library('data.table') # data manipulation
library('tibble') # data wrangling
library(dplyr)
library(tidyr)

library(igraph)
library(ggraph)
library(tidygraph)

## ## Data Preparation with the manual compiling lists
tpf.ipc <- as.tibble(fread('201902_TPF_IPC.txt', stringsAsFactors = TRUE))


improved <- subset(tpf.ipc, substring(IPC, 1, 7) == 'F02C007')

##
efficiency.improving <- subset(tpf.ipc, substring(IPC, 1, 7) %in% c('C10J003',
                                                                    
                                                                    'F23C001') | 
                               IPC == 'F23C005/24' | 
                               substring(IPC, 1, 7) %in% c('F23C006', 'F23B010', 'F23B030', 'F23B070', 'F23B080', 'F23D001', 'F23D007', 'F23D017') | 
                                        
                               IPC %in% c('B01J008/20', 'B01J008/21', 'B01J008/22', 
                                          'B01J008/24', 'B01J008/26', 'B01J008/28', 'B01J008/30') |
                               substring(IPC, 1, 7) == 'F27B015'|
                               substring(IPC, 1, 7) == 'F23C010'|
                                 
                               substring(IPC, 1, 7) == 'F22B031'|
                               IPC %in% c('F22B033/14', 'F22B033/15', 'F22B033/16') |
                                 
                               substring(IPC, 1, 7) %in% c('F01K003',
                                                           'F01K005',
                                                           'F01K023') |
                               
                               substring(IPC,1,4) == 'F22G'|
                               
                               IPC %in% sort(unique(improved$IPC))[c(11:13, 14:17, 38)] | 
                                #"F02C007/08"  "F02C007/10"  "F02C007/105" #F02C7/12-143 #F02C007/30 
                              
                               #F01K23/02-10  
                               IPC %in% c("F02C003/20", "F02C003/22", "F02C003/24", "F02C003/26", "F02C003/28", "F02C003/30", "F02C003/32", "F02C003/34", "F02C003/36", #F02C3/20-36                             
                                          "F02C006/10", "F02C006/12") | #F02C6/10-12 
                               
                               IPC %in% c("F02B001/12", "F02B001/14", #F02B1/12-14
                                          "F02B003/06", "F02B003/08", "F02B003/10") | #F02B3/06-10 
                               substring(IPC, 1, 7) %in% c('F02B007', 
                                                           'F02B011') | 
                               IPC %in% c("F02B013/02", "F02B013/04") | #F02B13/02-04 
                               substring(IPC, 1, 7) == 'F02B049' |
                                 
                               IPC == 'F01K017/06' |
                               substring(IPC, 1, 7) == 'F01K027' |
                               IPC == 'F02C006/18' |
                               substring(IPC, 1, 7) == 'F02G005' |
                               IPC == 'F25B027/02'
                               )
                               

##
fossil.fuel <- rbind(subset(tpf.ipc, substring(IPC, 1, 4) %in% c('C10J', 'F01K', 'F02C', 'F02G')),
                     subset(tpf.ipc, substring(IPC, 1, 3) %in% c('F22', 'F23', 'F27')))

##
renewable.electricity <- subset(tpf.ipc, (substring(IPC, 1, 7) == 'H01M004' & as.numeric(substring(IPC, 9, 10) %in% c(86:98))) |
                                (substring(IPC, 1, 7) == 'H01M002' & substring(IPC, 9, 10) %in% c('00', '01', '02', '03', '04')) |
                                substring(IPC, 1, 7) == 'H01M008' | ##  8/00-8/24  
                                substring(IPC, 1, 7) == 'H01M012' |
                                substring(IPC, 1, 10) == 'C10B053/00' | IPC == 'C10J' |
                               
                                substring(IPC, 1, 10) %in% c('C10L005/00', 'C10L005/42', 'C10L005/44') |
                                substring(IPC, 1, 10) %in% c('F23G007/00', 'F23G007/10') | #
                                IPC %in% c('C10J003/02', 'C10J003/46', 'F23B090/00', 'F23G005/027') | ##
                                IPC %in% c('B09B003/00', #
                                                          'F23G007/00') | 
                                IPC %in% c('C10L005/48', 'F23G005/00') |   #        #
                                IPC == 'C21B005/06' |
                                IPC == 'D21C011/00' |
                                IPC %in% c('A62D003/02', 'C02F011/04', 'C02F011/14') |
                                IPC == 'B09B' |
                                IPC %in% c('B01D053/02', 'B01D053/04', 'B01D053/047', 'B01D053/14', 'B01D053/22', 'B01D053/24', 'C10L005/46') |
                               
                                substring(IPC, 1, 7) == 'E02B009' |
                                IPC %in% c('F03B', 'F03C') |
                                (substring(IPC, 1, 7) =='F03B013' & as.numeric(substring(IPC, 9, 10)) %in% c(12:26)) |
                                substring(IPC, 1, 7) =='F03B015' | #
                                IPC %in% c('B63H019/02', 'B63H019/04') |
                               
                                IPC == 'F03G007/05' |
                               
                                IPC == 'F03D' | 
                                IPC =='H02K007/18' |
                                IPC %in% c('B63B035/00', 'E04H012/00') |
                                IPC == 'F03D011/04' |
                                IPC == 'B60K016/00' |
                                IPC == 'B60L008/00' |
                                IPC == 'B63H013/00' |
                               
                                IPC %in% c('H01L027/142', 'H01L031/00', 'H01L031/078', 'H01G009/20', 'H02N006/00') |
                                IPC == 'H01L027/30' | (substring(IPC, 1, 7) =='H01L051' & as.numeric(substring(IPC, 9, 10) %in% c(42:48))) |
                                (substring(IPC, 1, 7) =='H01L025' & substring(IPC, 9, 10) %in% c('00','03', '16', '18')) | IPC == 'H01L031/042' |
                                IPC %in% c('C01B033/02', 'C23C014/14', 'C23C016/24', 'C30B029/06') |
                                IPC == 'G05F001/67' |
                                IPC %in% c('F21L004/00', 'F21S009/03') |
                                IPC == 'H02J007/35' |
                                IPC %in% c('H01G009/20', 'H01M014/00') |
                                (substring(IPC, 1, 7) =='F24J002' & as.numeric(substring(IPC, 9, 10)) %in% c(0:54)) |
                                IPC == 'F24D017/00' |
                                IPC %in% c('F24D003/00', 'F24D005/00', 'F24D011/00', 'F24D019/00') |
                                IPC == 'F24J002/42' |
                                IPC %in% c('F03D001/04', 'F03D009/00', 'F03D011/04', 'F03G006/00') |
                                IPC == 'C02F001/14' |
                                IPC == 'F02C001/05' |
                                IPC == 'H01L031/058' |
                                IPC == 'B60K016/00' |
                                IPC == 'B60L008/00' |
                                substring(IPC, 1, 7) =='F03G006' | #F03G 6/00-6/06
                                IPC %in% c('E04D013/00', 'E04D013/18') |
                                IPC %in% c('F22B001/00', 'F24J001/00') |
                                IPC == 'F25B027/00' |
                                IPC %in% c('F26B003/00', 'F26B003/28') |
                                IPC %in% c('F24J002/06', 'G02B007/183') |
                                IPC == 'F24J002/04' |
                               
                                IPC %in% c('F01K', 'F24F005/00', 'F24J003/08', 'H02N010/00', 'F25B030/06') |
                                substring(IPC, 1, 7) =='F03G004' | IPC == 'F03G007/04' | #F03G 4/00-4/06, 7/04 
                                IPC %in% c('F24J001/00', 'F24J003/00', 'F24J003/06') |
                                IPC == 'F24D011/02' |
                                IPC == 'F24D015/04' |
                                IPC == 'F24D017/02' |
                                IPC == 'F24H004/00' |
                                IPC == 'F25B030/00' |
                               
                                IPC == 'F01K027/00' | # F01K 17/00;23/04 
                                IPC %in% c('F01N005/00', 'F02G005/00', 'F02G005/02', 'F02G005/04', 
                                                          'F25B027/02') | #F01K 23/06-23/10, F01N 5/00, F02G 5/00-5/04, F25B 27/02
                                IPC == 'F02C006/18' |
                                IPC == 'F25B027/02' |
                                IPC == 'C02F001/16' |
                                IPC == 'D21F005/20' |
                                IPC == 'F22B001/02' |
                                IPC == 'F23G005/46' |
                                IPC == 'F24F012/00' |
                                IPC == 'F27D017/00' |
                                (substring(IPC, 1, 7) %in% c('F28D017', 'F28D018', 'F28D019', 'F28D020') & IPC != 'F28D020/02') | #F28D 17/00-20/00
                                IPC == 'C10J003/86' |
                                substring(IPC, 1, 7) == 'F03G005') # F03G 5/00-5/08 
                               
##                               
electricity.storage <-   rbind(subset(tpf.ipc, IPC == 'B60K006/28'),
                               subset(tpf.ipc, IPC == 'B60W010/26'),
                               subset(tpf.ipc, IPC == 'H01M010/44'),
                               subset(tpf.ipc, IPC == 'H01M010/46'),
                               subset(tpf.ipc, IPC == 'H01G009/155'),
                               subset(tpf.ipc, IPC == 'H02J003/28'),
                               subset(tpf.ipc, IPC == 'H02J007/00'),
                               subset(tpf.ipc, IPC == 'H02J015/00')
                               )
  


## Adding the identified symbols for Family Component 
fossil.fuel$component <- paste(fossil.fuel$Family_id, 'F', sep = '')
renewable.electricity$component <- paste(renewable.electricity$Family_id, 'R', sep = '')
efficiency.improving$component <- paste(efficiency.improving$Family_id, 'I', sep = '')
electricity.storage$component <- paste(electricity.storage$Family_id, 'S', sep = '')

energy.triadic <- rbind(efficiency.improving, fossil.fuel, renewable.electricity, electricity.storage)


## Simplified the compounding Family Component

# avoiding the ambiguity for the overlapping component 
appln.unique <- energy.triadic %>% 
  group_by(Appln_id) %>% 
  summarise(n = n_distinct(FC)) %>% 
  filter(n > 1)
# the compounding Family component with the multiple category belongings
energy.triadic[which(energy.triadic$Appln_id %in% appln.unique$Appln_id),]$FC <- paste(energy.triadic$Family_id[which(energy.triadic$Appln_id %in% appln.unique$Appln_id)], 
                                                                                       'c', sep = '')

## connections between the patent application and patent family
liasion.triadic <- energy.triadic %>% 
  distinct(Appln_id, FC)

# Original Citation
epo.citations <- na.omit(subset(as.tibble(fread('201902_EP_Citations.txt', 
                                                select = c('Citing_pub_date', 'Citing_app_nbr', 'Citing_appln_id', 'Cited_Appln_id', 'Citn_Category'))), 
                                Citing_appln_id %in% energy.triadic$Appln_id | Cited_Appln_id %in% energy.triadic$Appln_id) ) %>% 
  distinct(Citing_appln_id, Cited_Appln_id, .keep_all = TRUE)

# Family Component Citation Network
epo.triadic <- epo.citations %>% 
  left_join(liasion.triadic, by = c('Citing_appln_id' = 'Appln_id')) %>% 
  left_join(liasion.triadic, by = c('Cited_Appln_id' = 'Appln_id'), 
            suffix = c('_Citing', '_Cited')) %>% 
  filter(is.na(FC_Citing) == FALSE & is.na(FC_Cited) == FALSE) %>% 
  #distinct(FC_Citing, FC_Cited, .keep_all = TRUE)
  group_by(FC_Citing, FC_Cited) %>% 
  summarise(weight = log(1+n()))

epo.triadic$id <- row.names(epo.triadic)
colnames(epo.triadic)[1:2] <- c('source', 'target')


epo.graph <- simplify(graph_from_data_frame(subset(epo.triadic, source != target), directed = FALSE))
epo.community <- cluster_fast_greedy(epo.graph, 
                                     weights = E(epo.graph)$weight) 

node.centrality <- as.data.frame(matrix(0, length(V(epo.graph)), 3))
colnames(node.centrality) <- c('node', 'centr_degree', 'centr_eigen')
node.centrality[,1] <- V(epo.graph)$name
node.centrality[,2] <- centr_degree(epo.graph)$res
node.centrality[,3] <- centr_eigen(epo.graph, directed = FALSE)$vector
node.centrality$membership <- membership(epo.community)

# Identified Communities with the inter-connections
community.triadic <- epo.triadic %>% 
  left_join(node.centrality[,-2], by = c('source' = 'node')) %>% 
  left_join(node.centrality[,-2], by = c('target' = 'node'),  suffix = c('_source', '_target')) %>% 
  filter(membership_source != membership_target) %>% 
  group_by(membership_source, membership_target) %>% 
  summarise(n = n())

community.graph <- simplify(graph_from_data_frame(community.triadic, directed = FALSE))

#
length(unique(c(community.triadic$membership_source, 
                community.triadic$membership_target))) / length(epo.community)

## Spectral Clustering
set.seed(817)

laplacian <- laplacian_matrix(community.graph)
eigenvalues <- eigen(laplacian)$value
plot(eigenvalues[1:50])
#
category.grouping <- as.data.frame(matrix(0, 72, 7))
category.grouping[,1] <- V(community.graph)$name
category.grouping[,2:10] <- eigen(laplacian)$vectors[,1:9]

clustering <- kmeans(category.grouping[,2:4], 6)

category.cluster <- as.data.frame(matrix(0,72, 2))
category.cluster[,1] <- V(community.graph)$name
category.cluster[,2] <- clustering$cluster
colnames(category.cluster) <- c('node', 'cluster')

clustering$size

community.cluster <- graph_from_data_frame(d = as_data_frame(community.graph, what = "edges"),
                                           vertices = category.cluster, directed = FALSE)
#
ggraph(community.cluster, #layout = 'sugiyama'
       layout = 'centrality', cent = centrality_betweenness() ) +
  geom_edge_link(color = "lightgrey")+ 
  geom_node_point(aes(size = centrality_eigen(), color = as.factor(cluster)))+
  geom_node_text(aes(label = name), size = 5, repel = TRUE)+
  scale_color_discrete() +
  theme_graph()




##                               
## Subset Selecting 
electricity.sector <- subset(tpf.ipc, Family_id %in% c(efficiency.improving$Family_id,
                                                       fossil.fuel$Family_id,
                                                       renewable.electricity$Family_id,
                                                       electricity.storage$Family_id))
##  
electricity.sector$storage <- electricity.sector$renewable <- electricity.sector$fossil <- electricity.sector$efficiency <- 0
#colnames(electricity.sector)
electricity.sector[electricity.sector$IPC %in% efficiency.improving$IPC,6] <- 1
electricity.sector[electricity.sector$IPC %in% fossil.fuel$IPC,7] <- 1
electricity.sector[electricity.sector$IPC %in% renewable.electricity$IPC,8] <- 1
electricity.sector[electricity.sector$IPC %in% electricity.storage$IPC,9] <- 1


##
electricity.alternative <- subset(tpf.ipc, !Family_id %in% unique(electricity.sector$Family_id) &
                                  IPC %in% unique(subset(electricity.sector, 
                                                         fossil+renewable+efficiency+storage==0)$IPC))

##
electricity.isolation <- subset(tpf.ipc, !Family_id %in% c(electricity.sector$Family_id,
                                                           electricity.alternative$Family_id))

## #
write.csv(electricity.sector, 'electricity_sector.csv', row.names = FALSE)
write.csv(electricity.alternative, 'electricity_alternative.csv', row.names = FALSE)

write.csv(electricity.isolation, 'electricity_isolation.csv', row.names = FALSE)

