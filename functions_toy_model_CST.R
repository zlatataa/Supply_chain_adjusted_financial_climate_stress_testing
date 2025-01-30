##cst source functions

library(Matrix)
library("readxl")
library("gridExtra")
library("data.table")
library(pdftools)
library("imputeTS")
library("graphics")
library("circlize")
library(devtools)
library("gplots")
library("pheatmap")
library("RColorBrewer")
library(pdftools)
library("colorspace")
library('ggplot2')



#0#

emissions_estimation<-function(firm_network,
                               firm_list,
                               emissions_gas,
                               emissions_oil){
  #Create a custom operator `%!in%` that negates the `%in%` operator # The `%in%` operator checks 
   #if an element (or elements) is present in a vector or collection, returning TRUE if found, otherwise FALSE
  `%!in%` = Negate(`%in%`)
  
  oil_connections_dt <- firm_network[supplier_nace4 %in% oil_nace4_list & buyer_nace4 %!in% oil_nace4_list]
  firms_oil_exposure <- oil_connections_dt[,.(sum_oil_input = sum(weight, na.rm = TRUE)),  by=buyer]
  firms_oil_exposure[,oil_input_normalized := sum_oil_input/sum(firms_oil_exposure$sum_oil_input)] # sum of in-strengths of oil consumers
  firms_oil_exposure[,emission_from_oil := oil_input_normalized*(emissions_oil)] # emission from oil
  
  
  gas_connections_dt <- firm_network[supplier_nace4 %in% gas_nace4_list & buyer_nace4 %!in% gas_nace4_list]
  firms_gas_exposure <- gas_connections_dt[,.(sum_gas_input = sum(weight, na.rm = TRUE)),  by=buyer]
  firms_gas_exposure[,gas_input_normalized := sum_gas_input/sum(sum_gas_input)] 
  firms_gas_exposure[,emission_from_gas := gas_input_normalized*(emissions_gas)] 
  
  
  firm_list[, emission_from_oil := firms_oil_exposure[firm_list, emission_from_oil, on = c("buyer" = "id")]]
  firm_list[is.na(emission_from_oil), emission_from_oil := 0 ]
  
  firm_list[ , emission_from_gas := firms_gas_exposure[firm_list, emission_from_gas, on = c("buyer" = "id")]]
  firm_list[ is.na(emission_from_gas), emission_from_gas := 0 ]
  
  firm_list[ , dirty_emission := emission_from_oil + emission_from_gas ]
  firm_list[ , ETS_participant := 0 ]
  
  firm_list[!is.na(emissions_reported), ETS_participant := 1]
  
  return(firm_list)
}




#1#
#outputs vector of nace4 market share to every firm in firm list
market_share<-function(firm_list){
  
  nace_4_dt<-as.data.table(firm_list$nace4)
  nace_4_dt$out_str<-firm_list$out_strength
  nace_4_aggr<-nace_4_dt[, lapply(.SD, sum, na.rm=TRUE), by=V1]
  
  nace4ag<-nace_4_dt$out_str
  
  for( i in 1:length(nace_4_dt$out_str)){
    nace4ag[i]<-nace_4_aggr$out_str[which(nace_4_aggr$V1 == nace_4_dt$V1[i])[1]]
  }
  
  nace_4_dt$market_share<-nace_4_dt$out_str/nace4ag
  
  market_shares<-nace_4_dt$market_share
  
  market_shares[is.na(market_shares)]<-0
  
  return(market_shares)
}


#2#
#redistributes carbon costs downstream the SCN based on market shares of firms
distribute_costs<-function(W_w, emissions, carbon_prices, exch_rate, market_shares){
  
  self_costs_s<-1-market_shares
  costs_distr<-rep(0,length(market_shares))
  costs_not_distr<-rep(0,length(market_shares))
  
  
  for( i in 1:length(carbon_prices)){
    
    emissions_costs<-emissions * exch_rate * carbon_prices[i] / 1000 # (divide by 1000 to have units in 1000 HUF) # 400 exchange rate to eur as in 2022
    
    c<-emissions_costs
    stays<-rep(0,length(market_shares))
    k<-0
    
    while(sum(c)>sum(emissions_costs)/10000){
      
      stays<-self_costs_s*c + stays
      goes<-c*market_shares
      
      c<-crossprod(W_w,goes)
      k<-k+1
    }
    #print(k)
    costs_distr<-cbind(costs_distr,stays)
    costs_not_distr<-cbind(costs_not_distr,c)
    
  }
  
  
  
  return(list(costs_distr=costs_distr[,2:(length(carbon_prices)+1)],
              costs_not_distr=costs_not_distr[,2:(length(carbon_prices)+1)],
              num_iter=k)) 
}



#3#
#prepares and cleans financial variables used in calculation of defaults
prepare_fin_data<-function(firm_list){
  
  equity<-firm_list$equity + firm_list$retained_earnings
  
  eq_inv<-1/equity
  eq_inv[is.infinite(eq_inv)]<-0
  eq_inv[firm_list$filtrating==0]<-0
  
  liq_inv<-1/firm_list$liquidity
  liq_inv[is.infinite(liq_inv)]<-0
  liq_inv[firm_list$filtrating==0]<-0
  
  fs_profit_inv<-1/firm_list$net_profit
  fs_profit_inv[is.infinite(fs_profit_inv)]<-0
  fs_profit_inv[firm_list$filtrating==0]<-0
  
  profit<-firm_list$operational_profit
  profit[firm_list$filtrating==0]<-0
  
  return(list(eq_inv=eq_inv,
              liq_inv=liq_inv,
              fs_profit_inv=fs_profit_inv,
              profit=profit))
  
}

#4#
#calculates initial defaults of firms (a firm defaults if its carbon costs are higher than its net profit)
initial_defaults<-function(carbon_costs, fs_profit_inv){
  
  N<-dim(carbon_costs)[1]
  M<-dim(carbon_costs)[2]
  
  
  initial_defaults<-matrix(0,nrow=N,ncol=M)
  
  for(i in 1:M){
    
    emissions_costs<-carbon_costs[,i]
    
    initial_defaults[,i]<-(emissions_costs*fs_profit_inv) >= 1
    
  }
  
  return(list(initial_defaults=initial_defaults))
  
}


#5#
#calculates indirect defaults of firms and quantifies losses of banks separately and together
financial_shock_carbon<-function(H = H, # matrix firms x scenaria with production losses from the supply chain network contagion
                                 initial_defaults, # matrix firms x scenaria with boolean entries indicating initially defaulted firms (from carbon costs)
                                 profit,#revenue - material costs       
                                 eq_inv, # 1/equity
                                 liq_inv,# 1/liquidity
                                 param_eq=1,# this parameter controls a share of equity buffer that will used to cover losses (1 is 100%)
                                 param_liq=1, # this parameter controls a share of equity buffer that will used to cover losses (1 is 100%)
                                 lgd=1, #loss given default
                                 loan_fb_matrix_bank_equities=loan_fb_matrix_bank_equities,#firm x bank matrix with loans weighted by equities of respective banks
                                 bank_equities=bank_equities,#vector of bank equities
                                 C # matrix firms x scenaria with carbon costs of firms 
                                 ){
  #financial loss is given by production losses H that proportionally reduce profit and additionally by carbon costs
  loss <- H*profit + C #matrix firms x scenaria
  
  #illiquid firms
  # indirect defaults (loss is compared with liquidity, firms that have higher losses than liquidity buffer will default)
  bad_liq<-(loss*liq_inv)>=param_liq  #matrix firms x scenaria
  #all defaults (adding directly defaulted firms)
  bad_liq<-(bad_liq+initial_defaults)>0 #matrix firms x scenaria
  
  cat("bad_liq",as.character(Sys.time()), "\n")####
  
  #insolvent firms
  bad_eq<-(loss*eq_inv)>=param_eq #matrix firms x scenaria
  
  bad_eq<-(bad_eq+initial_defaults)>0 #matrix firms x scenaria
  
  cat("bad_eq",as.character(Sys.time()), "\n")####
  
  #insolvent or illiquid firms (combining previous results)
  N <- dim(H)[1]
  M <- dim(H)[2]
  
  W_either_ind <- unique(c(which(bad_liq >0), which(bad_eq > 0)))
  
  
  arr_inds <- arrayInd( W_either_ind
                        , .dim = c(N,M))
 
  bad_either <- sparseMatrix(i = arr_inds[,1], #matrix firms x scenaria
                             j = arr_inds[,2],
                             dims = c(N,M))
  
  
  cat("bad_either ",as.character(Sys.time()), "\n")####
  cat("bad_eq na ", sum(is.na(bad_eq)), "\n")
  cat("bad_liq na ", sum(is.na(bad_liq)), "\n")
  
  
  #quantifying system wide losses from defaulted firms
  
  loss_liq<-crossprod(bad_liq,loan_fb_matrix_bank_equities) * lgd # matrix scenaria x banks with direct and indirect losses (from illiquid firms) of individual banks (measured as a share of their respective equities)
  
  loss_liq[which(loss_liq>1)]<-1
  fsri_liq<-loss_liq %*% (bank_equities/sum(bank_equities)) #financial system losses from illiquid firms
  
  loss_eq<-crossprod(bad_eq,loan_fb_matrix_bank_equities) * lgd # matrix scenaria x banks with direct and indirect losses (from insolvent firms) of individual banks (measured as a share of their respective equities)
  loss_eq[which(loss_eq>1)]<-1
  fsri_eq<-loss_eq %*% (bank_equities/sum(bank_equities)) #financial system losses from insolvent firms
  
  loss_either<-crossprod(bad_either,loan_fb_matrix_bank_equities) * lgd # matrix scenaria x banks with direct and indirect losses (from illiquid and insolvent firms) of individual banks (measured as a share of their respective equities)
  loss_either[which(loss_either>1)]<-1
  fsri_either<-loss_either %*% (bank_equities/sum(bank_equities)) #financial system losses from illiquid and insolvent firms together
  
  
  #direct losses
  
  loss_dir<-crossprod(initial_defaults,loan_fb_matrix_bank_equities) * lgd # matrix scenaria x banks with direct losses of individual banks (measured as a share of their respective equities)
  
  return(list(bad_eq = bad_eq,
              bad_liq = bad_liq,
              bad_either = bad_either,
              loss_eq = loss_eq,
              loss_liq = loss_liq,
              loss_either = loss_either,
              fsri_eq = fsri_eq,
              fsri_liq = fsri_liq,
              fsri_either = fsri_either,
              loss_dir = loss_dir))
  
}


#6#
#assigns firms from the supply chain network NACE 1 sectors
get_nace1<-function(sectors,firm_list){
  
  cs_maps_2<-sectors[sectors$Level == '2',]
  sc_nace2<-unique(firm_list$nace2)
  
  nace2_dict<-as.integer(sub('..','',cs_maps_2$NACE))
  nace2_dict[length(nace2_dict)+1]<-95
  nace2_dict[length(nace2_dict)+1]<-96
  nace2_dict[length(nace2_dict)+1]<-98
  nace2_dict[length(nace2_dict)+1]<-99
  nace2_dict[length(nace2_dict)+1]<-9999
  
  
  isic_nace2<-substr(cs_maps_2$NACE,1,1)
  isic_nace2[length(isic_nace2)+1]<-"S"
  isic_nace2[length(isic_nace2)+1]<-"S"
  isic_nace2[length(isic_nace2)+1]<-"T"
  isic_nace2[length(isic_nace2)+1]<-"U"
  isic_nace2[length(isic_nace2)+1]<-"Z"
  
  ind_fix<-sc_nace2 %in% nace2_dict
  
  firm_list_nace2_to_nace1<-firm_list$nace2
  
  
  firm_list_isic<-firm_list_nace2_to_nace1
  
  for( i in 1:length(firm_list$nace2)){
    firm_list_isic[i]<-isic_nace2[which(nace2_dict == firm_list_nace2_to_nace1[i])[1]]
  }
  
  
  return(firm_list_isic) # vector of NACE 1 sectors of firms from firm list (keeps the order)
  
  
}



##############
####colors####
##############

white<-rgb(1,1,1)
black<-rgb(0,0,0)
greydark<-rgb(162/255,169/255,176/255)
greylight<-rgb(235/255,236/255,237/255)
greenlight<-rgb(186/255,240/255,206/255)
bluelight<-rgb(183/255,225/255,255/255)
greennorm<-rgb(42/255,180/255,33/255)
bluenorm<-rgb(0,97/255,210/255)
greenmedium<-rgb(111/255,220/255,140/255)
ohra<-rgb(212/255,178/255,55/255)
yellownorm<-rgb(255/255,245/255,55/255)
bluemedium<-rgb(69/255,137/255,155/255)
purple<-rgb(198/255,119/255,226/255)
purplelight<-rgb(219/255,199/255,255/255)
orangenorm<-rgb(255/255,122/255,0)
rednorm<-rgb(249/255,58/255,46/255)
pinknorm<-rgb(255/255,175/255,210/255)
pinkdark<-rgb(208/255,38/255,112/255)

greys<-brewer.pal(8,"Greys")

cyan<-brewer.pal(12,"Set3")[1]
lightyellow<-brewer.pal(12,"Set3")[2]
lightviolet<-brewer.pal(12,"Set3")[3]
red<-brewer.pal(12,"Set3")[4]
blue<-brewer.pal(12,"Set3")[5]
orange<-brewer.pal(12,"Set3")[6]
green<-brewer.pal(12,"Set3")[7]
pink<-brewer.pal(12,"Set3")[8]
gray<-brewer.pal(12,"Set3")[9]
violet<-brewer.pal(12,"Set3")[10]
lightgreen<-brewer.pal(12,"Set3")[11]
yellow<-brewer.pal(12,"Set3")[12]
lightred <- brewer.pal(6,"Reds")[2]
brown <- brewer.pal(6,"BrBG")[2]
smth <- brewer.pal(6,"Spectral")[1]

