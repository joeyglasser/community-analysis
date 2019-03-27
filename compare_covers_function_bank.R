#setup
library(shiny)
library(rgdal)
library(leaflet)

################################################################################################

#functions 

cover_analysis = function(cover, include.histogram = TRUE){
  # Gives basic summary statistics of a cover 
  #
  # Arguments:
  #   cover: list of communities 
  #   include.historgram: indicates whether or not historgram of community sizes will be include 
  
  print(paste("Number of communities:", length(cover)))
  community_sizes = sapply(cover,length)
  print(paste("Mean community size:", mean(community_sizes)))
  print(paste("Standard Deviation of community size: ", sd(community_sizes), sep = ""))
  if(include.histogram){
    hist(community_sizes, main = "Distribution of Community Sizes", xlab = "Number of Counties", freq = FALSE)
  }
}

compare = function(a,b){
  # Helper function for more general comparisons 
  result = a == b
  result[is.na(result)] = FALSE
  return(result)
}

belongs.in = function(node, group){
  # Helper function for membership.vector functions 
  return(as.numeric(node %in% group))
}

membership.vector = function(node, cover){
  # Creates a membeship vector for a given node and cover 
  #
  # Arguments
  #   node: fips number 
  #   cover: list of communities 
  # Returns
  #   A numeric vector representing membership vector of given node and cover 
  
  vector = sapply(cover, belongs.in, node = node)
  if(sum(vector) != 0){
    vector = vector / sum(vector)
  }
  return(vector)
}


#Functions to extract state groups 

#state.fips.key = read.csv(url("https://www2.census.gov/geo/docs/reference/state.txt"), sep = "|") # Creating state fips key based off gov document 

state.fips.text = "STATE|STUSAB|STATE_NAME|STATENS
01|AL|Alabama|01779775
02|AK|Alaska|01785533
04|AZ|Arizona|01779777
05|AR|Arkansas|00068085
06|CA|California|01779778
08|CO|Colorado|01779779
09|CT|Connecticut|01779780
10|DE|Delaware|01779781
11|DC|District of Columbia|01702382
12|FL|Florida|00294478
13|GA|Georgia|01705317
15|HI|Hawaii|01779782
16|ID|Idaho|01779783
17|IL|Illinois|01779784
18|IN|Indiana|00448508
19|IA|Iowa|01779785
20|KS|Kansas|00481813
21|KY|Kentucky|01779786
22|LA|Louisiana|01629543
23|ME|Maine|01779787
24|MD|Maryland|01714934
25|MA|Massachusetts|00606926
26|MI|Michigan|01779789
27|MN|Minnesota|00662849
28|MS|Mississippi|01779790
29|MO|Missouri|01779791
30|MT|Montana|00767982
31|NE|Nebraska|01779792
32|NV|Nevada|01779793
33|NH|New Hampshire|01779794
34|NJ|New Jersey|01779795
35|NM|New Mexico|00897535
36|NY|New York|01779796
37|NC|North Carolina|01027616
38|ND|North Dakota|01779797
39|OH|Ohio|01085497
40|OK|Oklahoma|01102857
41|OR|Oregon|01155107
42|PA|Pennsylvania|01779798
44|RI|Rhode Island|01219835
45|SC|South Carolina|01779799
46|SD|South Dakota|01785534
47|TN|Tennessee|01325873
48|TX|Texas|01779801
49|UT|Utah|01455989
50|VT|Vermont|01779802
51|VA|Virginia|01779803
53|WA|Washington|01779804
54|WV|West Virginia|01779805
55|WI|Wisconsin|01779806
56|WY|Wyoming|01779807
60|AS|American Samoa|01802701
66|GU|Guam|01802705
69|MP|Northern Mariana Islands|01779809
72|PR|Puerto Rico|01779808
74|UM|U.S. Minor Outlying Islands|01878752
78|VI|U.S. Virgin Islands|01802710"

state.fips.key <- read.table(textConnection(state.fips.text), sep = "|", header = T)

extract.state.code = function(fips.string){
  # Gets state fips number from county fips 
  #
  # Arguments
  #   fips.string: string of county fips number 
  # Returns
  #   state fips number
  if((nchar(fips.string) < 4) | suppressWarnings(is.na(as.numeric(fips.string)))){
    return(-1)
  } else {
    return(as.numeric(substring(fips.string,1,nchar(fips.string) - 3)))
  }
}

convert.to.fips = function(state){
  # Helper function for extract.state.groups 
  if(sum(state.fips.key$STUSAB %in% state) == 1){
    return(state.fips.key[state.fips.key$STUSAB %in% state,"STATE"])
  }
  if(sum(state.fips.key$STATE_NAME %in% state) == 1){
    return(state.fips.key[state.fips.key$STATE_NAME  %in% state,"STATE"])
  } else {
    return(-1)
  }
}

extract.state.groups = function(cover, state.fips){
  # Extracts communities found in state from cover
  # 
  # Arguments
  #   cover: list of communities
  #   state.fips: fips code of state to be extracted 
  # Returns
  #   state.list: list of communities where any county is found in inputted state
  
  state.fips = convert.to.fips(state.fips)
  state.list = list()
  count = 1
  for(i in 1:length(cover)){
    if(sum(as.numeric(state.fips == sapply(cover[[i]],extract.state.code))) > 0){
      state.list[[count]] = cover[[i]]
      count = count + 1
    } 
  }
  return(state.list[sapply(state.list, length) > 0])
}


extract_cover = function(county_msa_data, group_col_name, county_col_name){
  # Extracts cover from county msa data
  # 
  # Arguments 
  #   county_msa_data: msa data containing community structure for counties 
  #   group_col_name: name of column that contains commuinty/group number for each county
  #   county_col_name: name of column that contains the county fips number 
  # Returns
  #   cover: list of communities 
  
  
  county_data = county_msa_data[,c(group_col_name,county_col_name)]
  states = suppressWarnings(sapply(as.character(county_data[,county_col_name]),extract.state.code))
  continental.states = state.fips.key$STATE[(state.fips.key$STATE %in% c(1,5,4,6,8,9,10,12,13,19,16,17,18,20,21,22,25,24,23,26,27,29,28,30,37,38,31,33,34,35,32,36,39,40,41,42,44,45,46,47,48,49,51,50,53,55,54,56))]
  
  county_data = county_data[states %in% continental.states, ]
 
  groups = suppressWarnings(as.numeric(as.character(county_data[,group_col_name])))
  unique_groups = unique(groups[!is.na(groups)])
  
  cover = list()
  
  for(i in 1:length(unique_groups)){ 
    if(!(unique_groups[[i]] == 9999)){
      cover[[i]] = as.character(county_data[compare(groups,unique_groups[[i]]),county_col_name])
    }
  }
  
  cover = cover[sapply(cover,length) > 0]
  cover = lapply(cover, as.numeric)
  cover = lapply(cover,unique)
  cover = lapply(cover, as.character)
  return(cover)
}

################################################################################################

# Region functions 

region.extraction = function(cover, region.number){
  # Extracts communities in given regions as defined by US Census Bureau 
  #
  # Arguments
  #   cover: list of communities
  #   region.number: number of the region to be extracted. More detail about regions here: https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States
  # Returns:
  #   region.list: list of communities that are found in inputted region. No cross-over between borders 
  
  
  # Getting states for each region 
  if(region.number == 1){
    states = c('Connecticut','Maine','Massachusetts','New Hampshire','Rhode Island','Vermont')
  } else if (region.number == 2){
    states = c('New Jersey','New York','Pennsylvania')
  } else if (region.number == 3){
    states = c('Illinois','Indiana','Michigan','Ohio','Wisconsin')
  } else if (region.number == 4){
    states = c('Iowa','Kansas','Minnesota','Missouri','Nebraska','North Dakota','South Dakota')
  } else if (region.number == 5){
    states = c('Delaware','Florida','Georgia','Maryland','North Carolina','South Carolina','Virginia','District of Columbia','West Virginia')
  } else if (region.number == 6){
    states =c('Alabama','Kentucky','Mississippi','Tennessee')
  } else if (region.number == 7){
    states = c('Arkansas','Louisiana','Oklahoma','Texas')
  } else if (region.number == 8){
    states = c('Arizona','Colorado','Idaho','Montana','Nevada','New Mexico','Utah','Wyoming')
  } else if (region.number == 9){
    states = c('California','Oregon','Washington')
  } 
  
  states = as.numeric(lapply(states,convert.to.fips))
  
  
  # Initializing variables to build up region list 
  region.list = list()
  count = 1
  
  # Iterating over each community
  for(i in 1:length(cover)){
    community = cover[[i]]
    community.states = as.numeric(lapply(community,extract.state.code))
    if(sum(community.states %in% states) > 0){ # If a community has a county in a state of the region
      region.list[[count]] = community[community.states %in% states] # Add counties that are in the region 
      count = count + 1
    }
  }
  return(region.list)
}

region.statistics = function(cover){
  # Gives summary statistics of size of communities in cover in every region
  #
  # Arguments
  #   cover: list of communitites
  # Returns
  #   results: a dataframe containing summary statistics of communities sizes in each region
  results = data.frame(1:9)
  colnames(results) = "Region Number"
  results['Community Count'] = 0
  results['Mean Community Size'] = 0
  results$sd = 0
  for(i in 1:9){
    groups = region.extraction(cover,i)
    g.count = sapply(groups,length)
    results[i,1] = i
    results[i,2] = length(g.count)
    if(length(g.count) > 0){
      results[i,3] = mean(g.count)
      results[i,4] = sd(g.count)
    }
  }
  return(results)
}



fuzzy.rand.index = function(cover1, cover2){
  # Calculates fuzzy rand index similarity between two covers. If 
  # there are counties belonging to no community, they should still show
  # up in the cover list as if they all belong to a "no-community" community
  
  
  cover1 = lapply(cover1, as.integer)
  cover2 = lapply(cover2, as.integer)
  
  nodes = sort(unique(c(unique(unlist(cover1)), unique(unlist(cover2)))))
  dissimilarity = 0
  
  for(i in 1:length(nodes)){
    if(i < length(nodes)){
      for(j in (i+1):length(nodes)){
        node1 = nodes[[i]]
        node2 = nodes[[j]]
        
        cover1.similarity = 1 - sum((membership.vector(node1,cover1) - membership.vector(node2,cover1))^2)
        cover2.similarity = 1 - sum((membership.vector(node1,cover2) - membership.vector(node2,cover2))^2)
        
        dissimilarity = dissimilarity + abs(cover1.similarity - cover2.similarity)
      }
    }
  }
  
  dissimilarity = dissimilarity / (length(nodes)*(length(nodes) - 1) / 2)
  
  return(1 - dissimilarity)
}

cover.2010.adder = function(cover){
  cover.counties = as.numeric(unique(unlist(msa2010)))
  missing.counties = list(sapply(all.counties2010[!(all.counties2010 %in% cover.counties)], as.character))
  if(length(missing.counties) > 0){
    cover[[length(cover) + 1]] <- missing.counties[[1]]
  } 
  return(cover)
}



fuzzy.rand.index.region = function(cover1, cover2, region, year){
  # Calculates fuzzy rand index similarity between two covers in a given region
  # 
  # Arguments
  #   cover1: list of communities
  #   cover2: list of communities
  #   region: number of region to be comapred for each cover
  #   year: year of data so that counties not belonging to any communties can be found 
  # Returns
  #   fuzzy rand index similarity as a float 
  
  if(year == 2010){
    region.counties = unlist(region.extraction(all.counties2010, region))
  } else if (year == 2000){
    region.counties = unlist(region.extraction(all.counties2000, region))
  } else if (year == 1990){
    region.counties = unlist(region.extraction(all.counties1990, region))
  } else {
    print("No year data")
    return(F)
  }
  
  region.cover1 = lapply(region.extraction(cover1,region), as.integer)
  region.cover2 = lapply(region.extraction(cover2,region), as.integer)
  
  
  cover.nodes = sort(unique(c(unique(unlist(region.cover1)), unique(unlist(region.cover2)), region.counties)))
  
  
  
  dissimilarity = 0
  for(i in 1:(length(cover.nodes)-1)){
    for(j in (i+1):length(cover.nodes)){
      node1 = cover.nodes[[i]]
      node2 = cover.nodes[[j]]
      
      cover1.similarity = 1 - sum(abs((membership.vector(node1,region.cover1) - membership.vector(node2,region.cover1))))/2
      
      
      cover2.similarity = 1 - sum(abs((membership.vector(node1,region.cover2) - membership.vector(node2,region.cover2))))/2
      
      
      dissimilarity = dissimilarity + abs(cover1.similarity - cover2.similarity)
      
    }
  }
  
  dissimilarity = dissimilarity / (length(cover.nodes)*(length(cover.nodes) - 1) / 2)
  
  
  return(1 - dissimilarity)
}

fuzzy.rand.index.regions.results = function(cover1, cover2, year){
  # Calculates fuzzy rand index similarity between all regions in two covers for a given year. Takes
  # a long time to run
  #
  # Arguments: 
  #   cover1: list of communities 
  #   cover2: list of communities
  #   year: year of data for each covers 
  # Returns
  #   results: dataframe of similarity number between each region for the two covers 
  results = data.frame(1:9)
  colnames(results) = "Region Number"
  results$similarity = 0
  for(i in 1:9){
    results[i,1] = i
    print(paste("Workning on Region:",as.character(i)))
    results[i, 2] = fuzzy.rand.index.region(cover1,cover2,i,year)
  }
  return(results)
}

fuzzy.rand.index.state = function(cover1, cover2, state, year){
  if(year == 2010){
    state.counties = unlist(extract.state.groups(all.counties2010, state))
  } else if (year == 2000){
    state.counties = unlist(extract.state.groups(all.counties2000, state))
  } else if (year == 1990){
    state.counties = unlist(extract.state.groups(all.counties1990, state))
  } else {
    print("No year data")
    return(F)
  }
  
  state.cover1 = lapply(extract.state.groups(cover1,state), as.integer)
  state.cover2 = lapply(extract.state.groups(cover2,state), as.integer)
  
  
  cover.nodes = sort(unique(c(unique(unlist(state.cover1)), unique(unlist(state.cover2)), state.counties)))
  
  
  
  dissimilarity = 0
  
  for(i in 1:(length(cover.nodes)-1)){
    print(i)
    for(j in (i+1):length(cover.nodes)){
      node1 = cover.nodes[[i]]
      node2 = cover.nodes[[j]]
      
      cover1.similarity = 1 - sum(abs((membership.vector(node1,state.cover1) - membership.vector(node2,state.cover1))))/2
      
      
      cover2.similarity = 1 - sum(abs((membership.vector(node1,state.cover2) - membership.vector(node2,state.cover2))))/2
      
      
      
      
      dissimilarity = dissimilarity + abs(cover1.similarity - cover2.similarity)
      
    }
  }
  
  dissimilarity = dissimilarity / (length(cover.nodes)*(length(cover.nodes) - 1) / 2)
  
  
  return(1 - dissimilarity)
  
}


plot.comparison = function(cover1, cover2){
  shinyApp(
    
    
    ui = fluidPage(column(width = 6,leafletOutput("map")),column(width = 6,leafletOutput("map2"))),
    
    
    server = function(input, output, session) {
      
      output$map = renderLeaflet({CreateMap(cover1)})
      output$map2 = renderLeaflet({CreateMap(cover2)})
      main = TRUE
    }
    
    
  )
}

region.compare = function(cover1,cover2,region){
  return(CreateMap(region.extraction(cover1,region),region.extraction(cover2,region)))
}




state.analysis = function(cover){
  states = state.fips.key$STATE_NAME
  num_communities = numeric(length(state.fips.key$STATE_NAME))
  mean_size = numeric(length(state.fips.key$STATE_NAME))
  std_size = numeric(length(state.fips.key$STATE_NAME))
  
  result = data.frame(states,num_communities,mean_size)
  
  print("Overall Statistics")
  cover_analysis(cover,FALSE)
  for(i in 1:nrow(state.fips.key)){
    community_sizes = sapply(extract.state.groups(cover,state.fips.key[i,"STATE_NAME"]),length)
    result[i,2] = length(community_sizes) 
    if(result[i,2] > 0){
      result[i,3] = mean(community_sizes)
      result[i,4] = sd(community_sizes)
    }
  }
  return(result[!is.na(result[,3]) & (result[,2] > 0),])
}

################################################################################################

#data load 

setwd(paste(user.directory, "Dropbox/regiondemarcation/code/git/community-analysis/data", sep = ''))
county_msa = read.csv("final_merge_county_msa.csv")
msa.micro.deliniations = read.csv("geocorr14.csv")

metro.counties.2010 = as.numeric(as.character(msa.micro.deliniations[as.character(msa.micro.deliniations$cbsatype10) == "Metro", 1]))

msa2010 = extract_cover(county_msa[county_msa[,"FIPS.2010"] %in% metro.counties.2010,], "CBSA.2010","FIPS.2010")
msa2000 = extract_cover(county_msa, "MSA.Code.2000","FIPS.2000")
msa1990 = extract_cover(county_msa, "MSA.1990", "FIPS.1990")

#2010
msa2010.states = sapply(as.character(county_msa[,"FIPS.2010"]),extract.state.code) 
all.counties2010 = county_msa[!(msa2010.states %in% c(-1,2,15,60,66,69,72,74,78)),"FIPS.2010"]

#2000
msa2000.states = sapply(as.character(county_msa[,"FIPS.2000"]),extract.state.code) 
all.counties2000 = county_msa[!(msa2000.states %in% c(-1,2,15,60,66,69,72,74,78)),"FIPS.2000"]

#1990
msa1990.states = sapply(as.character(county_msa[,"FIPS.1990"]),extract.state.code) 
all.counties1990 = county_msa[!(msa1990.states %in% c(-1,2,15,60,66,69,72,74,78)),"FIPS.1990"]