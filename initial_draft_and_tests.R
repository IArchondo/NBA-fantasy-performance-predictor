############################################################################
##        Initial draft + tests - CARMELO algorithm replication           ##
##                       Ignacio Archondo                                 ##
##                           2018                                         ##
############################################################################

## load required packages
list_of_packages <- c("ggplot2","dplyr","data.table","FNN","qdapTools")
lapply(list_of_packages,function(x) if(!require(x,character.only=TRUE)) install.packages(x))

## define required functions
normalize <- function(x){ (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}


## ---------------------- 1.0 DATA PRE PROCESSING ------------------------
##--------------------------------------------------------------------------

## 1.1 Load data

## Load the file with all the player data
df.comp <- getwd() %>% paste("database_comp.csv",sep="/") %>% read.csv2 %>% data.table

## Load the file with all the draft data
df.drafts <- getwd() %>% paste("drafts.csv",sep="/") %>% read.csv2 %>% data.table
df.comp.use <- df.comp[MPPG>=10]
## We add the draft data to the player data
df.comp.use <- df.drafts[,.(Pk,Player)][df.comp.use,on=.(Player)]

## 1.2 Data cleanup

## some repetitions appear, since we cant test them one by one, we only keep the ones with the lowest draft position
df.comp.use[,"min_draft":=(min(Pk)),by=Player]
df.comp.use <- df.comp.use[Pk==min_draft]
df.comp.use[,"min_draft":=NULL]

## we fix the small amount of cases where two players who had the same name played in the same season
df.comp.use[Player=="Eddie Johnson" & Pos=='SG' & season!='1990-1991']$Player<- c("Eddie Johnson (SG)")
df.comp.use[Player=="George Johnson" & Pos=='C']$Player <- "George Johnson (C)"
df.comp.use[Player=="Charles Jones" & (Pos=='SG'|Pos=='PG')]$Player<- "Charles Jones (G)"
df.comp.use[Player=='Charles Jones'][c(1,3,7),]$Player <- "Charles Jones (1962)"
df.comp.use[Player=='Michael Smith'][c(1,3),]$Player <- "Michael Smith (1965)"

#some cleaning up
df.comp.use$Player <- as.character(df.comp.use$Player)
df.comp.use <- df.comp.use[order(df.comp.use$Player,df.comp.use$season),]

## 1.3 Generation of extra variables

df.comp.use <- cbind(df.comp.use,df.comp.use$Pos %>% mtabulate) #we create a dummy for each position
df.comp.use$Pk <- ifelse(df.comp.use$Pk>60|is.na(df.comp.use$Pk),80,df.comp.use$Pk)
df.comp.use$drafted <- ifelse(df.comp.use$Pk<=60,1,0)

## create years active variable
df.comp.use[,"years_active":=rank(-season),by="Player"]

## we create a dummy for each players' last season on the dataset
df.comp.use[,"last_year":=(max(years_active)),by=Player]
df.comp.use[,"last_year_dummy":=(ifelse(last_year==years_active,1,0))]


##df.comp.use$Pos.simp <- df.comp.use$Pos
##levels(df.comp.use$Pos.simp) <- c("C","F","G","F","G")

## ---------------------- 2.0 OUTPUT GENERATOR ------------------------
##--------------------------------------------------------------------------

## 2.1 generate calculation function

## function to
##      1. find all related players of a certain player,
##      2. check how they performed the following year
##      3. calculate a weighted average of their following year's stats to
##             -->generate a prediction for the chosen player
f.find_neighbor_vars<- function(dataset,s.player,s.season,n_neighbors,print_results=TRUE,varlist){
    i.playerseason <- which(dataset$Player==s.player & dataset$season==s.season)
    i.player <- which(dataset$Player==s.player)
    ## remove all player registries form dataset except the chosen one
    removeplayers <- i.player[!i.player %in% i.playerseason]
    input_dataset <- dataset[-removeplayers,]
    input_dataset_simp<- input_dataset[,varlist,with=FALSE]

    ## normalize values
    df.knn <- sapply(input_dataset_simp,function(k) (k-min(k,na.rm = TRUE))/(max(k,na.rm=TRUE)-min(k,na.rm=TRUE))) %>% data.table %>% sapply(function(k) ifelse(is.na(k),0,k)) %>% data.table

    ## calculate distances
    dist_mat <- get.knn(df.knn,k=n_neighbors)

    ##get index of selected player
    comp.player <- which(input_dataset$Player==s.player)

    ##get closest players
    a.nn <- dist_mat$nn.index[comp.player,]

    if(print_results==TRUE){
    cbind(input_dataset$Player[c(comp.player,a.nn)],
          "season"=input_dataset$season[c(comp.player,a.nn)],
          input_dataset_simp[c(comp.player,a.nn)]) %>% head(10) %>% print

    dist_mat$nn.dist[comp.player,] %>% head(10) %>% print}
    
    ## calculate players' projected FPPM
    pred_actual <- input_dataset[a.nn,]
    pred_futura <- input_dataset[a.nn+1,][,.(Player,season,FPPM)]
    pred_futura[,"active":=(ifelse(Player %in% pred_actual$Player,1,0))]
    pred_futura[,"distance":=dist_mat$nn.dist[comp.player,]]
    pred_futura[,"inv_distance":=(1/distance*active)]
    pred_futura[,"weight":=(inv_distance/sum(inv_distance))]
    pred_futura[,"weighted_FPPM":=FPPM*weight]

    prediction <- pred_futura$weighted_FPPM %>% sum %>% round(2)
    real_value <- dataset[i.playerseason,]$FPPM

    ## print results
    if(print_results==TRUE){
    print(pred_futura %>% head(10))
    print("PREDICCIÓN: " %>% paste0(prediction %>% round(2)))
    print("VALOR REAL" %>% paste0(real_value))}

    out <- cbind(s.player,s.season,real_value,prediction) %>% data.table

    out$prediction <- out$prediction  %>% as.numeric %>% round(2)
    out$real_value <- out$real_value %>% as.numeric

    return(out)

    
#    return(list("distancias"=dist_mat$nn.dist[comp.player,],
 #               "FPPM"=input_dataset$FPPM[a.nn],
  #              "output"=dist_mat,
   #             "input_dataset"=input_dataset,
    #            "input_dataset_simp"=input_dataset_simp,
     #           "index_player"=comp.player))
}

## 2.2 quick check with example model
predictor_string <- c("Age","years_active","Pk","MP","GS","G","FG.","FG_pm","AST_pm","TRB_pm","STL_pm","TOV_pm","adv_USG.","adv_PER","adv_PER","adv_WS.48","FPPM","FPPM")

f.find_neighbor_vars(df.comp.use,"Randy Smith","1980-1981",50,FALSE,predictor_string) 

## ---------------------- 3.0 DETERMINE BEST PERFORMING MODEL -------------
##--------------------------------------------------------------------------

## 3.1 Generate testing sample

## remove players of last season and players on their last year, we cant evaluate their prediction
df.comp_test <-  df.comp.use[last_year_dummy!=1]

random_players <- sample(1:nrow(df.comp_test),500)
random_players <- df.comp_test[random_players,][,.(Player,season)]

## 3.2 Generate a testing function

## function:
##      1. inputs a list of players, calculates a performance forecast with f_find_neighbor_vars
##      2. calculates the sqrt error for each one and merges all results in an output table
test_model <- function(rownumber,dataset,n_neighbors,pred_vars){
    test_player <- dataset[rownumber]
    input_player <- test_player$Player
    input_season <- test_player$season %>% as.character

    out <- f.find_neighbor_vars(df.comp.use,input_player,input_season,n_neighbors,FALSE,pred_vars)

    if (rownumber%%20==0){
        message(rownumber,"/500 done")
    }
    return(out)
}

out <- lapply(1:500,function(k) test_model(k,random_players,50,predictor_string))

## 3.3 Define different models to test

## candidate one: extremely simple. just FPPM and PER
pred_string_A <- c("FPPM","adv_PER","FPPG")
## candidate two: still simple but with some info on age, etc
pred_string_B <- c(pred_string_A,"Age","years_active","Pk","MP","GS","G")
##candidate two: just simple offensive stats (not per minute) and FPPM and PER
pred_string_C <- c(pred_string_B,"FG","FG.",
                   "T3P","T3P.","FT","FT.","AST","TOV","PTS")
##candidate three: just simple offensive stats (not per minute) and FPPM and PER
pred_string_D <- c(pred_string_B,"Age","years_active","Pk","MP","GS","G","STL","BLK","PF")
## candidate four: combination of last two
pred_string_E <- c(pred_string_B,pred_string_B,pred_string_C) %>% unique
## candidate five: just advanced offensive stats
pred_string_F <- c(pred_string_B,"adv_TS.","adv_3PAr","adv_FTr","adv_AST.","adv_TOV.",
                   "adv_USG.","adv_OWS","adv_OBPM","adv_VORP")
## candidate six: just advanced offensive stats
pred_string_G <- c(pred_string_B,"adv_ORB.","adv_DRB.","adv_STL.","adv_BLK.","adv_DWS","adv_DBPM")
## candidate seven: combination of last two
pred_string_H <- c(pred_string_F,pred_string_G) %>% unique
## candidate eight: mixture of everything
pred_string_I <- c(pred_string_E,pred_string_H) %>% unique
## candidate nine: everything+ position
pred_string_J <- c(pred_string_I,"C","PF","PG","SF","SG")
## candidate ten: current mixed model
pred_string_K <- predictor_string
## candidate eleven: current mixed model+position
pred_string_L <- c(predictor_string,"C","PF","PG","SF","SG")

## merge them into a list
prediction_strings <- list(pred_string_A,pred_string_B,pred_string_C,pred_string_D,pred_string_E,pred_string_F,pred_string_G,pred_string_H,pred_string_I,pred_string_J,pred_string_K,pred_string_L)


## 3.3 Test models

## function:
##      1. takes a list of players, calculates their performance forecast
##      2. calculates sqrt error of each of the forecasts
##      3. merges results in a table
prediction_test_cruncher <- function(rownumber,dataset,n_neighbors,pred_vars){
    message("Evaluating the following model:", deparse(substitute(pred_vars)))
    out <- lapply(rownumber,function(k) test_model(k,dataset,n_neighbors,pred_vars))

    ## merge all players into a table
    out_table <- do.call("rbind",out) %>% data.table
    ## calculate error
    out_table$error <-((out_table$real_value-out_table$prediction)^2) %>% sqrt
    message("\n-----> DONE")
    return(out_table)
}

## calculate results for the same sample using all eleven models
output_model_ev <- lapply(prediction_strings, function(k) prediction_test_cruncher(1:500,random_players,50,k))



### is the error term correlated to players quality?
## first check: real value vs error
ggplot(data=out_table,aes(x=real_value,y=error))+
    geom_point()+theme_bw()
## both players that are very bad and players that are very good are mispredicted

## second check: real value v predictio
ggplot(data=out_table,aes(x=real_value,y=prediction))+
    geom_point()+geom_abline(slope=1,intercept=0)+theme_bw()
# model is too optimistic vor very bad players and a little pesimistic for everyone else


