# Get data

## Wrky33 e.g.
### GoMapMan
Obtain Ath geneID using [gomapman.nib.si](https://gomapman.nib.si/search)

<img src="https://github.com/NIB-SI/DiNAR/blob/master/subApps/GMM-SKM-KnetMiner/figs/GMM.png" width=75% height=75%>

### KnetMiner
Obtain Knowledge graph from [KnetMiner](https://knetminer.com/poaceae/html/genepage.jsp?list=AT2G38470)

<img src="https://github.com/NIB-SI/DiNAR/blob/master/subApps/GMM-SKM-KnetMiner/figs/KnetMiner.png" width=75% height=75%>

### SKM PSS
Obtain first neighbours of your gene of interest from [Mechanistic Plant Stress Signalling model](https://skm.nib.si/biomine/)

<img src="https://github.com/NIB-SI/DiNAR/blob/master/subApps/GMM-SKM-KnetMiner/figs/SKM-PSS.png" width=75% height=75%>

</br>

# Create and visualise custom network

1. [Download](https://github.com/NIB-SI/DiNAR/blob/master/subApps/GMM-SKM-KnetMiner_customNet/scripts/CustomNetwork_KnetMiner-SKM.R) and run [CustomNetwork_KnetMiner-SKM](https://github.com/NIB-SI/DiNAR/tree/master/subApps/GMM-SKM-KnetMiner/) app localy or from [shinyapps](https://nib-si.shinyapps.io/GMM-SKM-KnetMiner/)
2. Load [files](https://github.com/NIB-SI/DiNAR/tree/master/subApps/GMM-SKM-KnetMiner/input)
3. Follow instructions and save Custom Network Tables (Nodes/Vertices and Reactions/Edges)
4. Load [Custom Network](https://github.com/NIB-SI/DiNAR/tree/master/subApps/GMM-SKM-KnetMiner/output-for-DiNAR) and Expression Tables into [DiNAR](https://github.com/NIB-SI/DiNAR)
5. Export [results](https://github.com/NIB-SI/DiNAR/tree/master/subApps/GMM-SKM-KnetMiner/output-from-DiNAR) as wished




(*) Take care of empty strings in tables in general, replate them with ```-```

(**) Try to keep shorName short
