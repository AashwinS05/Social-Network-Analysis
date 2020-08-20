# Social-Network-Analysis Application - SNApp

Performed Social Network Analysis using various measures of centrality to find crucial links within a dataset of inter-organization emails. Provided the results through a RShiny app.
Use both the Server.r & ui.r to run the shiny App


# Description of Dataset

The network was generated using email data from a large European research institu-tion. We have anonymized information about all incoming and outgoing email between
members of the research institution. There is an edge (u, v) in the network if person u sent person v at least one email. The e-mails only represent communication between institution members (the core), and the dataset does not contain incoming messages from or outgoing messages to the rest of the world. The dataset also contains "ground-truth" community memberships of the nodes. Each individual belongs to exactly one of 42 departments at the research institute. This network represents the "core" of the email-Eu All network, which also contains links between members of the institution and people outside of the institution (although the node IDs are not the same).


# Features of the RShiny App

> User is able to select and laod relevant files for this project through the UI of the application. For the purpose of this example we will be using the files attached (email-Eu-core-department-labels.txt and email-Eu-core.txt)
> The app is able to show any 'n' connections contained within the file - email-Eu-core.txt using user provided slider input tab.  
> Programatically provide summary information about the count of emails sent and received by the users within the data. 
> Display network diagram to show 2-hop neighbors of the top 10 users with highest emails received or sent.
> The SNApp Programatically computes different measures of centrality to help determine crucial links within the dataset.
