library(fastcascade)
library(GLcascade)
library(data.table)

args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=3){
  print('Wrong number of arguments supplier. Please supply path to nodelist and edgelist')
  stop('Wrong number of args.')
}

path_nodelist <- args[1]
path_edgelist <- args[2]
save_path <- args[3]

# nodelist <- fread('~/Projects/temporal_VATnetwork/model_building/data/sector_C_food_production_configmodel_consecutive_nodelist.csv')
# edgelist <- fread('~/Projects/temporal_VATnetwork/model_building/data/sector_C_food_production_configmodel_consecutive.csv')
nodelist <- fread(path_nodelist)
edgelist <- fread(path_edgelist)



adjMat <- Matrix::sparseMatrix(
  # id of sender firm
  i = edgelist[,supplier]+1,  # correct for pythons 0 indexing
  
  # id of receiver firm
  j = edgelist[,buyer]+1,  # correct for pythons 0 indexing
  
  # weight of goods and services sent
  # x = ,
  
  # number of firms
  dims = rep(dim(nodelist)[1], 2)
)

nodelist[,nace:=formatC(nace, width=2, format = "d", flag = "0")]

p <- nodelist[,nace]
p <- as.character(p)

# ess_mat_n2_ihs <- readRDS('~/Projects/temporal_VATnetwork/model_building/ESRI/ess_mat_n2_ihs.RDS')
ess_mat_n2_ihs <- readRDS('~/bin/ESRI/ess_mat_n2_ihs.RDS')
# naceC_codes10t33 <- colnames(ess_mat_n2_ihs)[9:32]
naceC_codes <- sort(unique(p))
naceC_repl_mat <- ess_mat_n2_ihs[naceC_codes,naceC_codes]
# colnames(naceC_repl_mat) <- nodelist[,nace]
# data.table(nodelist)[,nace]
#   
# image(c_repl_mat,xaxt = "n", yaxt = "n")
# lbls <- naceC_codes#[seq(1, length(naceC_codes))]
# axis(side=1,at=seq(0,1,by=1/length(lbls))[1:length(lbls)],labels=lbls,xpd=NA)

ESRIMixPT <- GL_cascade(W = adjMat,
                           p = p,
                           p_market = as.integer(p),
                           p_sec_impacts = FALSE,
                           ess_mat_sec = naceC_repl_mat,
                           h_weights = FALSE,
                           sec_aggr_weights = FALSE,
                           psi_mat = FALSE,
                           revenue = FALSE,
                           costs = FALSE,
                           track_h = FALSE,
                           track_sector_impacts = FALSE,
                           track_conv = TRUE,
                           conv_type = 1,
                           eps = 10^-2,
                           use_rcpp = TRUE,
                           ncores = 7,
                           run_id = "test_cascade",
                           load_balance = TRUE,
                           prodfun_sets = FALSE, # kill in the future
                           serv_supplier_sets = FALSE # kill in the future
)

ESRIrslt <- ESRIMixPT$ESRI[,1]
cat("### ESRI VECTOR ###")
cat(ESRIrslt)

fwrite(data.table(ESRI=ESRIMixPT$ESRI[,1]), paste0(save_path,'_ESRI_vec.csv'))
saveRDS(ESRIMixPT, paste0(save_path,'_ESRI_result.RDS'))

# print(ESRIMixPT$ESRI)

# # ### ESRI Plots
# sortIndex <- order(ESRIrslt, decreasing = TRUE)
# plot(ESRIrslt[sortIndex],
#      type = "l",
#      col = "#e41a1c",
#      lwd = 10,
#      # ylim = c(0, 0.1),
#      # main = list("ESRI Distributions of German Firms with Rep. Factor", cex = 2),
#      log = "x",
#      # axes = FALSE
# )
