# load packages
library(igraph)
library(dplyr)
library(RColorBrewer)

# load data
within <- read.csv("~/Downloads/scc2034_kilifi_all_contacts_within_households.csv")
across <- read.csv("~/Downloads/scc2034_kilifi_all_contacts_across_households.csv")
within$type <- "W"
within$node1 <- paste(within$h1, within$m1, sep = "")
within$node2 <- paste(within$h2, within$m2, sep = "")
across$type <- "A"
across$node1 <- paste(across$h1, across$m1, sep = "")
across$node2 <- paste(across$h2, across$m2, sep = "")
within_origin <- within
across_origin <- across

within$order <- (within$node1>within$node2)
within_replace <- within[within$order==TRUE, ]
colnames(within_replace) <- c("h2", "m2", "h1", "m1", "age2", "age1", "sex2", "sex1", "duration", "day", "hour", "type",
                              "node2", "node1", "order")
within_replace <- within_replace[, c(3,4,1,2,6,5,8,7,9,10,11,12,14,13,15)]
head(within_replace)
within_nonreplace <- within[within$order==FALSE, ]
within <- rbind(within_replace, within_nonreplace)

avg_contacts <- within %>% group_by(age1, age2) %>% summarize(num_contacts = n(), contacts_dur = sum(duration))
within %>% group_by(age1, age2) %>% summarize(contacts_dur = sum(duration))

within_deg <- as.data.frame(deg)
within_deg <- tibble::rownames_to_column(within_deg, "m")

within_deg <- within_deg %>% left_join(nodes_within, by=c("m"="label"))
within_deg 

within_deg %>% group_by(age) %>% summarise(mean(deg))

############################ within household frequency ###############################################################
# nodes list
node1 <- within %>% distinct(node1) %>% rename(label=node1) 
node2 <- within %>% distinct(node2) %>% rename(label=node2) 

nodes_within <- full_join(node1, node2, by="label")
nodes_within <- nodes_within %>% left_join(within_origin, by=c("label"="node1")) %>% select(label, h1, age1, sex1)

#nodes_within <- nodes_within %>% left_join(within, by=c("label"="node1")) %>% select(label, h1, age1, sex1)
#nodes_within2 <- nodes_within[c(32426:32432), ]
#nodes_within2 <- nodes_within2 %>% left_join(within, by=c("label"="node2")) %>% select(label, h1, age1, sex1)
colnames(nodes_within) <- c("label", "h", "age", "sex")
#colnames(nodes_within2) <- c("label", "h", "age", "sex")
#nodes_within <- rbind(nodes_within[c(1:32425), ], nodes_within2)
nodes_within <- distinct(nodes_within)
head(nodes_within)

# edges list
per_route <- within %>% group_by(node1, node2) %>% summarise(frequency=n()/3) %>% ungroup()
head(per_route)

edges <- per_route %>% left_join(nodes_within, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_within, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, frequency)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_within, directed=F)

# vertex size
deg <- degree(net, mode="all")
sort(deg, decreasing = TRUE)
V(net)$size <- deg/2

# Calculate the Eigenvector centrality
eigen <- evcent(net, directed = FALSE)
V(net)$eigen <- eigen$vector
sort(round(eigen$vector,4), decreasing = TRUE)
eigenvector <- as.data.frame(round(eigen$vector,4))
eigenvector <- tibble::rownames_to_column(eigenvector, "m")

# edge width = duration
E(net)$width <- E(net)$frequency/30

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(5, "Pastel2")
V(net)[V(net)$h == "B"]$color <- v_colrs[1]
V(net)[V(net)$h == "E"]$color <- v_colrs[2]
V(net)[V(net)$h == "F"]$color <- v_colrs[3]
V(net)[V(net)$h == "H"]$color <- v_colrs[4]
V(net)[V(net)$h == "L"]$color <- v_colrs[5]

#identify high eigen centrality
V(net)$label <- ifelse(V(net)$eigen > 0.9, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.3, y=1.3, c("B", "E", "F", "H", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on sex:
v_colors2 <- c("lightpink", "lightgrey")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
V(net)$label <- NA
plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.3, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Pastel1")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.3, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)

############################ within betweenness frequency ##########################
# nodes list
# nodes list
#node1 <- within %>% distinct(node1) %>% rename(label=node1) 
#node2 <- within %>% distinct(node2) %>% rename(label=node2) 

#nodes_within <- full_join(node1, node2, by="label")
#nodes_within <- nodes_within %>% left_join(within, by=c("label"="node1")) %>% select(label, h1, age1, sex1)
#nodes_within2 <- nodes_within[c(32426:32431), ]
#nodes_within2 <- nodes_within2 %>% left_join(within, by=c("label"="node2")) %>% select(label, h2, age2, sex2)
#colnames(nodes_within) <- c("label", "h", "age", "sex")
#colnames(nodes_within2) <- c("label", "h", "age", "sex")
#nodes_within <- rbind(nodes_within[c(1:32425), ], nodes_within2)
#nodes_within <- distinct(nodes_within)

# edges list
per_route <- within %>% group_by(node1, node2) %>% summarise(frequency=n()/3) %>% ungroup()
head(per_route)

edges <- per_route %>% left_join(nodes_within, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_within, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, frequency)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_within, directed=F)

# vertex size
between <- betweenness(net)
sort(round(between,2), decreasing = TRUE)
V(net)$size <- between

# Calculate the Eigenvector centrality
# eigen <- evcent(net, directed = FALSE)
# V(net)$eigen <- eigen$vector
# sort(round(eigen$vector,4), decreasing = TRUE)

# edge width = duration
E(net)$width <- E(net)$frequency/30

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(5, "Pastel2")
V(net)[V(net)$h == "B"]$color <- v_colrs[1]
V(net)[V(net)$h == "E"]$color <- v_colrs[2]
V(net)[V(net)$h == "F"]$color <- v_colrs[3]
V(net)[V(net)$h == "H"]$color <- v_colrs[4]
V(net)[V(net)$h == "L"]$color <- v_colrs[5]

#identify high centrality
V(net)$label <- ifelse(V(net)$size > 16, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.3, y=1.3, c("B", "E", "F", "H", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

############################ within household duration #####################
# edges list
per_route <- within %>% group_by(node1, node2) %>% summarise(weight=mean(duration)) %>% ungroup()
head(per_route)

edges <- per_route %>% left_join(nodes_within, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_within, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, weight)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_within, directed=F)

# vertex size
deg <- degree(net, mode="all")
sort(deg, decreasing = TRUE)
V(net)$size <- deg/1.3

eigen <- evcent(net, directed = FALSE)
V(net)$eigen <- eigen$vector
sort(round(eigen$vector,2), decreasing = TRUE)

# edge width = duration
E(net)$width <- E(net)$weight/60

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(5, "Pastel2")
V(net)[V(net)$h == "B"]$color <- v_colrs[1]
V(net)[V(net)$h == "E"]$color <- v_colrs[2]
V(net)[V(net)$h == "F"]$color <- v_colrs[3]
V(net)[V(net)$h == "H"]$color <- v_colrs[4]
V(net)[V(net)$h == "L"]$color <- v_colrs[5]

#identify high eigen centrality
V(net)$label <- ifelse(V(net)$eigen > 0.9, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net,edge.curved=.1, layout = layout_on_sphere)
legend(x=1.3, y=1.3, c("B", "E", "F", "H", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on sex:
v_colors2 <- c("lightpink", "grey")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
plot(net,edge.curved=.1, layout = layout_on_grid)
legend(x=1.3, y=1.3, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Pastel1")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net,edge.curved=.1, layout = layout_on_grid)
legend(x=1.3, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)

############################ across household ##############################
across$order <- (across$node2>across$node1)
across_replace <- across[across$order==TRUE, ]
colnames(across_replace) <- c("h2", "m2", "h1", "m1", "age2", "age1", "sex2", "sex1", "duration", "day", "hour", "type",
                              "node2", "node1", "order")
across_replace <- across_replace[, c(3,4,1,2,6,5,8,7,9,10,11,12,14,13,15)]
head(across_replace)
across_nonreplace <- across[across$order==FALSE, ]
across <- rbind(across_replace, across_nonreplace)

across %>% group_by(age1, age2) %>%summarise(n())
############################ across degree frequency  ##############################
# nodes list
node1 <- across %>% distinct(node1) %>% rename(label=node1) 
node2 <- across %>% distinct(node2) %>% rename(label=node2) 

nodes_across <- full_join(node1, node2, by="label")
nodes_across <- nodes_across %>% left_join(across, by=c("label"="node1")) %>% select(label, h1, age1, sex1)
nodes_across2 <- nodes_across[c(219:235), ]
nodes_across2 <- nodes_across2 %>% left_join(across, by=c("label"="node2")) %>% select(label, h2, age2, sex2)
colnames(nodes_across) <- c("label", "h", "age", "sex")
colnames(nodes_across2) <- c("label", "h", "age", "sex")
nodes_across <- rbind(nodes_across[c(1:218), ], nodes_across2)
nodes_across <- distinct(nodes_across)
table(is.na(nodes_across$h))

# edges list
per_route <- across %>% group_by(node1, node2) %>% summarise(weight=n()/2) %>% ungroup()
head(per_route)

edges <- per_route %>% left_join(nodes_across, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_across, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, weight)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_across, directed=F)
# vertex size
deg <- degree(net, mode="all")
deg
sort(deg, decreasing = TRUE)
V(net)$size <- deg*2

across_deg <- as.data.frame(deg)
across_deg <- tibble::rownames_to_column(across_deg, "m")

eigen <- evcent(net, directed = FALSE)
V(net)$eigen <- eigen$vector
sort(eigen$vector, decreasing = TRUE)

across_eigen <- as.data.frame(round(eigen$vector,4))
across_eigen <- tibble::rownames_to_column(across_eigen, "m")

# edge width
E(net)$width <- E(net)$weight/2

# Generate vertex colors based on HH:
v_colrs <-  brewer.pal(3, "Blues")
V(net)[V(net)$h == "E"]$color <- v_colrs[1]
V(net)[V(net)$h == "F"]$color <- v_colrs[2]
V(net)[V(net)$h == "L"]$color <- v_colrs[3]

#identify high eigen centrality
V(net)$label <- ifelse(V(net)$eigen > 0.5, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.1, y=1.3, c("E", "F", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on sex:
v_colors2 <- c("lightpink", "grey")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.1, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Pastel1")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.1, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)

nodes_across %>% group_by(sex) %>% summarise(n())
nodes_across %>% group_by(age) %>% summarise(n())

across %>% group_by(age1, age2) %>% summarise(n())
across %>% group_by(sex1, sex2) %>% summarise(n())

##################### across betweenness frequency ################################
# vertex size
betweenness <- betweenness(net)
betweenness
V(net)$size <- deg*2
sort(betweenness, decreasing = T)
betweenness.across <- as.data.frame(betweenness)
betweenness.across <- tibble::rownames_to_column(betweenness.across, "m")

eigen <- evcent(net, directed = FALSE)
V(net)$eigen <- eigen$vector
sort(eigen$vector, decreasing = TRUE)

# edge width
E(net)$width <- E(net)$weight/2

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(3, "Pastel2")
V(net)[V(net)$h == "E"]$color <- v_colrs[1]
V(net)[V(net)$h == "F"]$color <- v_colrs[2]
V(net)[V(net)$h == "L"]$color <- v_colrs[3]

#identify high eigen centrality
V(net)$label <- V(net)$name
V(net)$label <- ifelse(V(net)$eigen > 0.8, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.0, y=1.3, c("E", "F", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on sex:
v_colors2 <- c("lightpink", "grey")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.1, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Pastel1")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.0, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)

##################### across degree duration ##############################
# nodes list
node1 <- across %>% distinct(node1) %>% rename(label=node1) 
node2 <- across %>% distinct(node2) %>% rename(label=node2) 

nodes_across <- full_join(node1, node2, by="label")
nodes_across <- nodes_across %>% left_join(across, by=c("label"="node1")) %>% select(label, h1, age1, sex1)
nodes_across2 <- nodes_across[c(219:235), ]
nodes_across2 <- nodes_across2 %>% left_join(across, by=c("label"="node2")) %>% select(label, h2, age2, sex2)
colnames(nodes_across) <- c("label", "h", "age", "sex")
colnames(nodes_across2) <- c("label", "h", "age", "sex")
nodes_across <- rbind(nodes_across[c(1:218), ], nodes_across2)
nodes_across <- distinct(nodes_across)
table(is.na(nodes_across$h))

# edges list
per_route <- across %>% group_by(node1, node2) %>% summarise(weight=mean(duration)) %>% ungroup()
head(per_route)

edges <- per_route %>% left_join(nodes_across, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_across, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, weight)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_across, directed=F)
# vertex size
deg <- degree(net, mode="all")
deg
V(net)$size <- deg*2

eigen <- evcent(net, directed = FALSE)
V(net)$eigen <- eigen$vector
sort(eigen$vector, decreasing = TRUE)

# edge width
E(net)$width <- E(net)$weight/20

# Generate vertex colors based on HH:
v_colrs <-  brewer.pal(3, "Blues")
V(net)[V(net)$h == "E"]$color <- v_colrs[1]
V(net)[V(net)$h == "F"]$color <- v_colrs[2]
V(net)[V(net)$h == "L"]$color <- v_colrs[3]

#identify high eigen centrality
V(net)$label <- ifelse(V(net)$eigen > 0.7, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.0, y=1.3, c("E", "F", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on sex:
v_colors2 <- c("lightpink", "grey")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
plot(net,edge.curved=.1, layout = layout_with_mds)
legend(x=1.1, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Pastel1")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.1, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)
##################### all together ###################################################################################
##################### df degree frequency ######################################################################################
df <- rbind(within, across)

# nodes list
node1 <- df %>% distinct(node1) %>% rename(label=node1) 
node2 <- df %>% distinct(node2) %>% rename(label=node2) 

nodes_all <- full_join(node1, node2, by="label")
nodes_all <- nodes_all %>% left_join(df, by=c("label"="node1")) %>% select(label, h1, age1, sex1)
nodes_all2 <- nodes_all[c(71:75),] %>% left_join(df, by=c("label"="node2")) %>% select(label, h2, age2, sex2)
nodes_all <- nodes_all[c(1:70),]
colnames(nodes_all) <- c("label", "h", "age", "sex")
colnames(nodes_all2) <- c("label", "h", "age", "sex")
nodes_all <- rbind(nodes_all, nodes_all2)
nodes_all <- distinct(nodes_all)

# edges list
per_route <- df %>% group_by(node1, node2, type) %>% summarise(weight=n()) %>% ungroup()
head(per_route)
per_route$weight[per_route$type=="W"] <- per_route$weight/3
per_route$weight[per_route$type=="A"] <- per_route$weight/2

edges <- per_route %>% left_join(nodes_all, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_all, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, weight, type)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_all, directed=F)

# calculate degree centriacity
# vertex size
deg <- degree(net, mode="all")
deg
V(net)$size <- deg/2

eigen <- evcent(net, directed = FALSE)
V(net)$eigen <- eigen$vector
sort(round(eigen$vector,4), decreasing = TRUE)

#edge width
E(net)[E(net)$type == "W"]$width <- E(net)[E(net)$type == "W"]$weight/40
E(net)[E(net)$type == "A"]$width <- E(net)[E(net)$type == "A"]$weight/20

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(5, "Pastel2")
V(net)[V(net)$h == "B"]$color <- v_colrs[1]
V(net)[V(net)$h == "E"]$color <- v_colrs[2]
V(net)[V(net)$h == "F"]$color <- v_colrs[3]
V(net)[V(net)$h == "H"]$color <- v_colrs[4]
V(net)[V(net)$h == "L"]$color <- v_colrs[5]

# Generate edge colors based on type:
e_colrs <- c("rosybrown", "skyblue")
E(net)[E(net)$type == "W"]$color <- e_colrs[1]
E(net)[E(net)$type == "A"]$color <- e_colrs[2]

#identify high eigen centrality
V(net)$label <- ifelse(V(net)$eigen > 0.8, V(net)$name, NA)
V(net)$label.color <- "black"

plot(net, layout = layout_with_kk)
legend(x=1.3, y=0.8, c("B", "E", "F", "H", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("rosybrown", "skyblue"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


# Generate vertex colors based on sex:
v_colors2 <- c("lightpink", "grey")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
V(net)$label <- NA
plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.3, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("rosybrown", "skyblue"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Spectral")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net, layout = layout_with_kk)
legend(x=1.3, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("rosybrown", "skyblue"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


##################### df degree avg duration #############################################
# edges list
per_route <- df %>% group_by(node1, node2, type) %>% summarise(duration=mean(duration)) %>% ungroup()
head(per_route)
# per_route$weight[per_route$type=="W"] <- per_route$weight/3
# per_route$weight[per_route$type=="A"] <- per_route$weight/2

edges <- per_route %>% left_join(nodes_all, by=c("node1"="label")) %>% rename(from=node1)
edges <- edges %>% left_join(nodes_all, by=c("node2"="label")) %>% rename(to=node2)
edges <- select(edges, from, to, duration, type)
head(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes_all, directed=F)

# calculate normalized degree centrality
# vertex size
deg <- degree(net, mode="all")
deg
V(net)$size <- deg/2

# edge width = duration
E(net)$width <- E(net)$duration/30

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(5, "Pastel2")
V(net)[V(net)$h == "B"]$color <- v_colrs[1]
V(net)[V(net)$h == "E"]$color <- v_colrs[2]
V(net)[V(net)$h == "F"]$color <- v_colrs[3]
V(net)[V(net)$h == "H"]$color <- v_colrs[4]
V(net)[V(net)$h == "L"]$color <- v_colrs[5]

# Generate edge colors based on type:
e_colrs <- c("grey", "orange")
E(net)[E(net)$type == "W"]$color <- e_colrs[1]
E(net)[E(net)$type == "A"]$color <- e_colrs[2]

V(net)$label <- NA

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.0, y=0.8, c("B", "E", "F", "H", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("grey40", "orange"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


# Generate vertex colors based on sex:
v_colors2 <- c("red", "blue")
V(net)[V(net)$sex == "F"]$color <- v_colors2[1]
V(net)[V(net)$sex == "M"]$color <- v_colors2[2]
plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.0, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("grey40", "orange"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Spectral")
V(net)[V(net)$age == 0]$color <- v_colors3[1]
V(net)[V(net)$age == 1]$color <- v_colors3[2]
V(net)[V(net)$age == 2]$color <- v_colors3[3]
V(net)[V(net)$age == 3]$color <- v_colors3[4]
V(net)[V(net)$age == 4]$color <- v_colors3[5]

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.0, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("grey40", "orange"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)



##################### df eigen #########################################################################################
# edges list
per_route <- df %>% group_by(node1, node2, type) %>% summarise(weight=n()) %>% ungroup()
head(per_route)
per_route$weight[per_route$type=="W"] <- per_route$weight/3
per_route$weight[per_route$type=="A"] <- per_route$weight/2

edges <- per_route %>% left_join(nodes_all, by=c("node1"="label")) %>% rename(from=id)
edges <- edges %>% left_join(nodes_all, by=c("node2"="label")) %>% rename(to=id)
edges <- select(edges, from, to, weight, type)
head(edges)
net <- graph_from_data_frame(d=edges, vertices=nodes_all, directed=F)

# calculate degree centriacity
# vertex size
eigen <- evcent(net, directed = FALSE)
eigen$vector
V(net)$size <- 1+eigen$vector*10

# edge width 
E(net)[E(net)$type == "W"]$width <- E(net)[E(net)$type == "W"]$weight/30
E(net)[E(net)$type == "A"]$width <- E(net)[E(net)$type == "A"]$weight/10

# Generate vertex colors based on HH:
v_colrs <- brewer.pal(5, "Spectral")
V(net)[V(net)$h == "B"]$color <- v_colrs[1]
V(net)[V(net)$h == "E"]$color <- v_colrs[2]
V(net)[V(net)$h == "F"]$color <- v_colrs[3]
V(net)[V(net)$h == "H"]$color <- v_colrs[4]
V(net)[V(net)$h == "L"]$color <- v_colrs[5]

# Generate edge colors based on type:
e_colrs <- c("grey40", "orange")
E(net)[E(net)$type == "W"]$color <- e_colrs[1]
E(net)[E(net)$type == "A"]$color <- e_colrs[2]

V(net)$label <- NA
plot(net, layout = layout_with_kk)
legend(x=1.0, y=0.8, c("B", "E", "F", "H", "L"), pch=21,
       col="#777777", pt.bg=v_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("grey40", "orange"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


# Generate vertex colors based on sex:
v_colors2 <- c("red40", "blue40")
V(net)[V(net)$sex == "F"]$color <- v_colrs[1]
V(net)[V(net)$sex == "M"]$color <- v_colrs[2]
plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.0, y=0.8, c("F", "M"), pch=21,
       col="#777777", pt.bg=v_colors2, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("grey40", "orange"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Generate vertex colors based on age:
v_colors3 <- brewer.pal(5, "Spectral")
V(net)[V(net)$age == 0]$color <- v_colrs[1]
V(net)[V(net)$age == 1]$color <- v_colrs[2]
V(net)[V(net)$age == 2]$color <- v_colrs[3]
V(net)[V(net)$age == 3]$color <- v_colrs[4]
V(net)[V(net)$age == 4]$color <- v_colrs[5]

plot(net,edge.curved=.1, layout = layout_with_kk)
legend(x=1.0, y=0.8, c("0-5", "6-14", "15-19", "20-49", ">50"), pch=21,
       col="#777777", pt.bg=v_colors3, pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=1.0, y=1.3, c("W", "A"), lty=c(1,1), lwd=c(2.5,2.5),
       col=c("grey40", "orange"), pt.bg=e_colrs, pt.cex=2, cex=.8, bty="n", ncol=1)



