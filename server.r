server <- function(input,output){
  output$view_1 <- renderTable({
    infile_1 <- input$Emp
    if (is.null(infile_1)) return(NULL)
    data_1 <- read.table(infile_1$datapath)
    colnames(data_1) <- c("Employee_From","Employee_To")
    head(data_1, n = input$Number)
  })
  output$view_2 <- renderTable({
    infile_2 <- input$Dept
    if (is.null(infile_2)) return(NULL)
    data_2 <- read.table(infile_2$datapath)
    colnames(data_2) <- c("Employee", "Department")
    head(data_2, n = input$Number)
  })
  output$simple <- renderVisNetwork({
    infile_3 <- input$Emp
    if (is.null(infile_3)) return(NULL)
    data_3 <- read.table(infile_3$datapath)
    graph <- graph_from_data_frame(data_3[1:input$obs_1,], directed = FALSE)
    data_3 <- toVisNetworkData(graph)
    visNetwork(nodes = data_3$nodes, edges = data_3$edges)
  })
  output$table_sent <- renderTable({
    infile_4 <- input$Emp
    if (is.null(infile_4)) return(NULL)
    data_4 <- read.table(infile_4$datapath)
    colnames(data_4) <- c("Emp_From","Emp_to")
    email_sent <- sqldf("SELECT Emp_From as Emp,COUNT(Emp_From) as No_Sent 
                        FROM data_4 
                        GROUP BY Emp_From 
                        ORDER BY No_Sent DESC")
    head(email_sent, n = input$obs_2)
  })
  output$Plot_sent <- renderForceNetwork({
    infile_5 <- input$Emp  
    if (is.null(infile_5)) return(NULL)
    data_5 <- read.table(infile_5$datapath)
    graph <- graph_from_data_frame(data_5[1:input$Number_1,], directed = FALSE)
    colnames(data_5) <- c("Emp_From","Emp_to")
    email_sent <- sqldf("SELECT Emp_From as Emp,COUNT(Emp_From) as No_Sent 
                        FROM data_5 
                        GROUP BY Emp_From 
                        ORDER BY No_Sent DESC")
    Count_snt_T10 <- email_sent[1:10,]
    Top_10_nodes_sent <- as.vector(Count_snt_T10[,1])
    graph_sent <- make_ego_graph(graph, order = 2, nodes = Top_10_nodes_sent, mode = "all")
    graph_sent_Union <- graph.union(graph_sent[[1]], graph_sent[[2]], graph_sent[[3]],
                                    graph_sent[[4]],graph_sent[[5]],graph_sent[[6]], 
                                    graph_sent[[7]], graph_sent[[8]],graph_sent[[9]],
                                    graph_sent[[10]], byname = TRUE)
    graph_sent_Union_S <- simplify(graph_sent_Union, remove.multiple = TRUE, remove.loops = TRUE)# graph object # check it for loops removal
    cluster_sent <- cluster_walktrap(graph_sent_Union_S)
    membership_sent <- membership(cluster_sent)
    graph_sent <- igraph_to_networkD3(graph_sent_Union_S, 
                                      group = membership_sent, what = "both")
    forceNetwork(Links = graph_sent$links, Nodes = graph_sent$nodes,
                                    Source = 'source', Target = 'target', NodeID = 'name',
                                    Group = 'group')
    })
  output$table_Recvd <- renderTable({
    infile_5 <- input$Emp
    if (is.null(infile_5)) return(NULL)
    data_5 <- read.table(infile_5$datapath)
    colnames(data_5) <- c("Emp_From","Emp_to")
    email_rcvd <- sqldf("SELECT Emp_to as Emp, COUNT(Emp_to) as No_Received 
                  FROM data_5 
                        GROUP BY Emp_to 
                        ORDER BY No_Received DESC")
    head(email_rcvd, n = input$obs_2)
  }) 
  output$Plot_Recvd <- renderForceNetwork({
    infile_5 <- input$Emp  
    if (is.null(infile_5)) return(NULL)
    data_5 <- read.table(infile_5$datapath)
    graph <- graph_from_data_frame(data_5[1:input$Number_1,], directed = FALSE)
    colnames(data_5) <- c("Emp_From","Emp_to")
    email_rcvd <- sqldf("SELECT Emp_to as Emp, COUNT(Emp_to) as No_Received 
                  FROM data_5 
                        GROUP BY Emp_to 
                        ORDER BY No_Received DESC")
    Count_Recvd_T10 <- email_rcvd[1:10,]
    Top_10_nodes_Recvd <- as.vector(Count_Recvd_T10[,1])
    graph_Recvd <- make_ego_graph(graph, order = 2, nodes = Top_10_nodes_Recvd, mode = "all")
    graph_Recvd_Union <- graph.union(graph_Recvd[[1]], graph_Recvd[[2]], graph_Recvd[[3]], 
                                     graph_Recvd[[4]], graph_Recvd[[5]],graph_Recvd[[6]], 
                                     graph_Recvd[[7]], graph_Recvd[[8]], graph_Recvd[[9]],
                                     graph_Recvd[[10]], byname = TRUE)
    graph_Recvd_Union_S <- simplify(graph_Recvd_Union, remove.multiple = TRUE, remove.loops = TRUE)# graph object # check it for loops removal
    cluster_Recvd <- cluster_walktrap(graph_Recvd_Union_S)
    membership_Recvd <- membership(cluster_Recvd)
    graph_Recvd <- igraph_to_networkD3(graph_Recvd_Union_S, group = membership_Recvd, what = "both")
    Top10_Recvd_Plot <- forceNetwork(Links = graph_Recvd$links, Nodes = graph_Recvd$nodes, Source = 'source', Target = 'target', NodeID = 'name',
                                     Group = 'group')
  })
  output$`Total - Degree` <- renderVisNetwork({
    infile_3 <- input$Emp
    if (is.null(infile_3)) return(NULL)
    infile_2 <- input$Dept
    if (is.null(infile_2)) return(NULL)
    data_3 <- read.table(infile_3$datapath)
    graph <- graph_from_data_frame(data_3[1:input$Number_2,], directed = FALSE)
    Emp_FT_degree <- as.data.frame(as.matrix(igraph::degree(graph, v = V(graph), mode = "total", normalized = FALSE)))
    NodeID <- rownames(Emp_FT_degree)
    rownames(Emp_FT_degree) <- NULL
    Emp_FT_degree <- cbind(NodeID,Emp_FT_degree)
    colnames(Emp_FT_degree) <- c("NodeID", "Degree_Centrality")
    Emp_FT_degree <- sqldf("SELECT *
                            FROM Emp_FT_degree
                            ORDER BY Degree_Centrality DESC")
    data_4 <- read.table(infile_2$datapath)
    colnames(data_4) <- c("Emp", "Dept")
    Emp_Dept_degree <- sqldf("SELECT data_4.Emp as Emp, 
                                     data_4.Dept as Dept,
                                     Emp_FT_degree.Degree_Centrality as Degree_Centrality
                          FROM Emp_FT_degree JOIN data_4
                             ON data_4.Emp = Emp_FT_degree.NodeID
                             ORDER BY Degree_Centrality DESC")
    Emp_Dept_degree_Top10 <- Emp_Dept_degree[1:10,]
    Top_10_nodes_degree <- as.vector(Emp_Dept_degree_Top10[,1])
    graph_degree <- make_ego_graph(graph, order = 2, nodes = Top_10_nodes_degree, mode = "all")
    graph_degree_Union <- graph.union(graph_degree[[1]], graph_degree[[2]], graph_degree[[3]], graph_degree[[4]], 
                                      graph_degree[[5]],graph_degree[[6]], graph_degree[[7]], graph_degree[[8]],
                                      graph_degree[[9]], graph_degree[[10]], byname = TRUE)
    graph_degree_Union_S <- simplify(graph_degree_Union, remove.multiple = TRUE, remove.loops = TRUE) # graph object
    data_7 <- toVisNetworkData(graph_degree_Union_S)
    nodes_degree <- merge.data.frame(data_4, data_7$nodes, by.x = "Emp", by.y = "id", sort = FALSE)
    colnames(nodes_degree) <- c("id", "group", "label")
    visNetwork(nodes = nodes_degree, edges = data_7$edges)%>%
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE, selectedBy = "group")%>%
      visPhysics(stabilization = FALSE)
    # %>%
    #   visGroups(groupname = "36", color = "darkblue", shape = "square")%>%
    #   visGroups(groupname = "34", color = "red", shape = "triangle")%>%
    #   visGroups(groupname = "4", color = "green", shape = "circle")%>%
    #   visGroups(groupname = "25", color = "yellow", shape = "rectangle")
    })
  output$`In - Degree` <- renderVisNetwork({
    infile_3 <- input$Emp
    if (is.null(infile_3)) return(NULL)
    infile_2 <- input$Dept
    if (is.null(infile_2)) return(NULL)
    data_3 <- read.table(infile_3$datapath)
    graph <- graph_from_data_frame(data_3[1:input$Number_2,], directed = TRUE)
    Emp_FT_Indegree <- as.data.frame(as.matrix(igraph::degree(graph, v = V(graph), mode = "in", normalized = FALSE)))
    NodeID <- rownames(Emp_FT_Indegree)
    rownames(Emp_FT_Indegree) <- NULL
    Emp_FT_Indegree <- cbind(NodeID,Emp_FT_Indegree)
    colnames(Emp_FT_Indegree) <- c("NodeID", "Indegree_Centrality")
    Emp_FT_Indegree <- sqldf("SELECT *
                           FROM Emp_FT_Indegree
                           ORDER BY Indegree_Centrality DESC")
    data_4 <- read.table(infile_2$datapath)
    colnames(data_4) <- c("Emp", "Dept")
    Emp_Dept_Indegree <- sqldf("SELECT data_4.Emp as Emp, 
                             data_4.Dept as Dept,
                             Emp_FT_Indegree.Indegree_Centrality as Indegree_Centrality
                             FROM Emp_FT_Indegree JOIN data_4
                             ON data_4.Emp = Emp_FT_Indegree.NodeID
                             ORDER BY Indegree_Centrality DESC")
    Emp_Dept_Indegree_Top10 <- Emp_Dept_Indegree[1:10,]
    Top_10_nodes_Indegree <- as.vector(Emp_Dept_Indegree_Top10[,1])
    graph_Indegree <- make_ego_graph(graph, order = 2, nodes = Top_10_nodes_Indegree, mode = "in")
    graph_Indegree_Union <- graph.union(graph_Indegree[[1]], graph_Indegree[[2]], graph_Indegree[[3]], graph_Indegree[[4]], 
                                      graph_Indegree[[5]],graph_Indegree[[6]], graph_Indegree[[7]], graph_Indegree[[8]],
                                      graph_Indegree[[9]], graph_Indegree[[10]], byname = TRUE)
    graph_Indegree_Union_S <- simplify(graph_Indegree_Union, remove.multiple = TRUE, remove.loops = TRUE) # graph object
    data_7 <- toVisNetworkData(graph_Indegree_Union_S)
    nodes_Indegree <- merge.data.frame(data_4, data_7$nodes, by.x = "Emp", by.y = "id", sort = FALSE)
    colnames(nodes_Indegree) <- c("id", "group", "label")
    visNetwork(nodes = nodes_Indegree, edges = data_7$edges)%>%
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE, selectedBy = "group")%>%
      visPhysics(stabilization = FALSE)
    # %>%
    #   visGroups(groupname = "36", color = "darkblue", shape = "square")%>%
    #   visGroups(groupname = "34", color = "red", shape = "triangle")%>%
    #   visGroups(groupname = "4", color = "green", shape = "circle")%>%
    #   visGroups(groupname = "25", color = "yellow", shape = "rectangle")
  })
  output$Betweenness <- renderVisNetwork({
    infile_3 <- input$Emp
    if (is.null(infile_3)) return(NULL)
    infile_2 <- input$Dept
    if (is.null(infile_2)) return(NULL)
    data_3 <- read.table(infile_3$datapath)
    graph <- graph_from_data_frame(data_3[1:input$Number_3,], directed = FALSE)
    Emp_FT_Betweenness <- as.data.frame(as.matrix(igraph::betweenness(graph, v = V(graph), directed = FALSE, normalized = FALSE)))
    NodeID <- rownames(Emp_FT_Betweenness)
    rownames(Emp_FT_Betweenness) <- NULL
    Emp_FT_Betweenness <- cbind(NodeID,Emp_FT_Betweenness)
    colnames(Emp_FT_Betweenness) <- c("NodeID", "Betweenness_Centrality")
    Emp_FT_Betweenness <- sqldf("SELECT *
                             FROM Emp_FT_Betweenness
                             ORDER BY Betweenness_Centrality DESC")
    data_4 <- read.table(infile_2$datapath)
    colnames(data_4) <- c("Emp", "Dept")
    Emp_Dept_Betweenness <- sqldf("SELECT data_4.Emp as Emp, data_4.Dept as Dept,Emp_FT_Betweenness.Betweenness_Centrality as Betweenness_Centrality
                                   FROM Emp_FT_Betweenness JOIN data_4
                                   ON data_4.Emp = Emp_FT_Betweenness.NodeID
                                   ORDER BY Betweenness_Centrality DESC")
    Emp_Dept_Betweenness_Top10 <- Emp_Dept_Betweenness[1:10,]
    Top_10_nodes_Betweenness <- as.vector(Emp_Dept_Betweenness_Top10[,1])
    graph_Betweenness <- make_ego_graph(graph, order = 2, nodes = Top_10_nodes_Betweenness, mode = "all")
    graph_Betweenness_Union <- graph.union(graph_Betweenness[[1]], graph_Betweenness[[2]], graph_Betweenness[[3]], graph_Betweenness[[4]], 
                                        graph_Betweenness[[5]],graph_Betweenness[[6]], graph_Betweenness[[7]], graph_Betweenness[[8]],
                                        graph_Betweenness[[9]], graph_Betweenness[[10]], byname = TRUE)
    graph_Betweenness_Union_S <- simplify(graph_Betweenness_Union, remove.multiple = TRUE, remove.loops = TRUE) # graph object
    data_7 <- toVisNetworkData(graph_Betweenness_Union_S)
    nodes_Betweenness <- merge.data.frame(data_4, data_7$nodes, by.x = "Emp", by.y = "id", sort = FALSE)
    colnames(nodes_Betweenness) <- c("id", "group", "label")
    visNetwork(nodes = nodes_Betweenness, edges = data_7$edges)%>%
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE, selectedBy = "group")%>%
      visPhysics(stabilization = FALSE)
    # %>%
    #   visGroups(groupname = "36", color = "darkblue", shape = "square")%>%
    #   visGroups(groupname = "34", color = "red", shape = "triangle")%>%
    #   visGroups(groupname = "4", color = "green", shape = "circle")%>%
    #   visGroups(groupname = "25", color = "yellow", shape = "rectangle")
    
  })
  output$`Tabular Representation` <- renderTable({
    infile_1 <- input$Emp
    if (is.null(infile_1)) return(NULL)
    data_1 <- read.table(infile_1$datapath)
    colnames(data_1) <- c("E_From","E_To")
    infile_2 <- input$Dept
    if (is.null(infile_2)) return(NULL)
    data_2 <- read.table(infile_2$datapath)
    colnames(data_2) <- c("Emp", "Dept")
    Dept.sent <- sqldf("SELECT data_2.Dept as Sent_Dept, 
                               data_1.E_From as E_From, 
                              data_1.E_To as E_To 
                        FROM data_1 JOIN data_2
                        ON data_2.Emp = data_1.E_From")
    Dept.rcvd <- sqldf("SELECT data_2.Dept as rcvd_Dept, 
                               data_1.E_To as E_To, 
                               data_1.E_From as E_From  
                        FROM data_1 JOIN data_2
                        ON data_2.Emp = data_1.E_To")
    Dept_sent_recvd <- as.data.frame(cbind(Dept.sent[,1],Dept.rcvd[,1]))
    colnames(Dept_sent_recvd) <- c("Dept_From", "Dept_To")
    Dept_count_snt <- sqldf("SELECT Dept_From, Dept_To, COUNT(Dept_From) as No_Sent
                         FROM Dept_sent_recvd
                            GROUP BY Dept_From, Dept_To 
                            ORDER BY No_Sent DESC")
    head(Dept_count_snt, n = input$Number_4)})
  output$Plot_Dept <- renderVisNetwork({
    infile_1 <- input$Emp
    if (is.null(infile_1)) return(NULL)
    data_1 <- read.table(infile_1$datapath)
    colnames(data_1) <- c("E_From","E_To")
    infile_2 <- input$Dept
    if (is.null(infile_2)) return(NULL)
    data_2 <- read.table(infile_2$datapath)
    colnames(data_2) <- c("Emp", "Dept")
    Dept.sent <- sqldf("SELECT data_2.Dept as Sent_Dept, 
                       data_1.E_From as E_From, 
                       data_1.E_To as E_To 
                       FROM data_1 JOIN data_2
                       ON data_2.Emp = data_1.E_From")
    Dept.rcvd <- sqldf("SELECT data_2.Dept as rcvd_Dept, 
                       data_1.E_To as E_To, 
                       data_1.E_From as E_From  
                       FROM data_1 JOIN data_2
                       ON data_2.Emp = data_1.E_To")
    Dept_sent_recvd <- as.data.frame(cbind(Dept.sent[,1],Dept.rcvd[,1]))
    colnames(Dept_sent_recvd) <- c("Dept_From", "Dept_To")
    Dept_count_snt <- sqldf("SELECT Dept_From, Dept_To, COUNT(Dept_From) as No_Sent
                            FROM Dept_sent_recvd
                            GROUP BY Dept_From, Dept_To 
                            ORDER BY No_Sent DESC")
    graph <- graph_from_data_frame(Dept_count_snt[1:input$Number_5,1:2], directed = TRUE)
    data_7 <- toVisNetworkData(graph)
    visNetwork(nodes = data_7$nodes, edges = data_7$edges)%>%
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE)%>%
      visPhysics(stabilization = FALSE)
    
    # cluster_sent <- cluster_walktrap(graph)
    # membership_sent <- membership(cluster_sent)
    # graph_sent <- igraph_to_networkD3(graph, 
    #                                   group = membership_sent, what = "both")
    # forceNetwork(Links = graph_sent$links, Nodes = graph_sent$nodes,
    #              Source = 'Dept_From', Target = 'Dept_To', NodeID = 'name',
    #              Group = 'group')
    
  })
  }

