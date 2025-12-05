#==============================================================================#
#################### using the GL cascade package ##############################
#==============================================================================#

# for details on shock propagation see: 
# Diem, C., Borsos, A., Reisch, T., Kertész, J., & Thurner, S. (2022). Quantifying firm-level economic systemic risk from nation-wide supply networks. Scientific reports, 12(1), 7719.
# Diem, C., Borsos, A., Reisch, T., Kertész, J., & Thurner, S. (2024). Estimating the loss of economic predictability from aggregating firm-level production networks. PNAS nexus, 3(3), pgae064.
#==============================================================================#



# 1. install the R packages
# 2. test if code works on dummy example
# 3. create inputs for real data
# 4. run cascade on real data




#-------------------------------------------------------------------------------#
# Download and Install Required Packages
#-------------------------------------------------------------------------------#

# to install packages from source make sure C++/Fortan compilers are there; 
#check https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html

# linear algebra in C++
Sys.time()

install.packages("Rcpp")
Sys.time()

install.packages("RcppArmadillo")
Sys.time()

# sparse matrices
install.packages("Matrix")
Sys.time()

# visualisation
install.packages("igraph")
Sys.time()

install.packages("colorspace")
Sys.time()


# check if it worked
library(Rcpp)
library(RcppArmadillo)
library(Matrix)
library(igraph)
library(colorspace)




# Set paths to the .zip or .tar.gz files (hosted on GitHub)
path_sc <- "https://github.com/ch-diem/misestimation_from_aggregation/raw/refs/heads/main"

# Dynamically detect the operating system
os_type <- ifelse(.Platform$OS.type == "windows", "win", "unix")

# Install the required packages based on the operating system
if (os_type == "win") {
  install.packages(paste0(path_sc, "/fastcascade_0.9.3.1.zip"), 
                   repos = NULL, type = "win.binary")
  install.packages(paste0(path_sc, "/GLcascade_0.9.3.1.zip"), 
                   repos = NULL, type = "win.binary")
}

if (os_type == "unix") {
  install.packages(paste0(path_sc, "/fastcascade_0.9.3.1.tar.gz"), 
                   repos = NULL, type = "source")
  install.packages(paste0(path_sc, "/GLcascade_0.9.3.1.tar.gz"), 
                   repos = NULL, type = "source")
}

#-------------------------------------------------------------------------------#
# Load Installed Packages
#-------------------------------------------------------------------------------#
library(GLcascade)
library(fastcascade)

# Documentation lookup for the GLcascade package
??GLcascade


library(Matrix)



#==============================================================================#
########################### test on minimal example ############################ 
#==============================================================================#

# GLcascade contains a dummy example
GLcascade::example_data

Matrix::image(t(GLcascade::ess_mat_n4_ihs))

# example network stored as sparse matrix from the library(Matrix) package
W <- GLcascade::example_data$W

# W[i,j] ... i supplies j with the amount W[i,j]
W

# example industry affiliation vector
p <- GLcascade::example_data$p

# p[i] ... industry affiliation of firm i, 
# usually a NACE class represented as integer, string also possible (and is probably saver)
p

# example of matrix keeping track of which sector is essential for which other sector
ess_mat_sec <- GLcascade::example_data$ess_mat_sec

# ess_mat_sec[k,l] ... importance of sector k for production of sector l
# controls production functions of sectors
ess_mat_sec

# needs to have the same number of rows and columns as unique industries in vector p
dim(ess_mat_sec) == length(unique(p))

# row and column names refer to the industry 
colnames(ess_mat_sec)
rownames(ess_mat_sec)

# note that each industry in p needs to be contained in ess_mat_sec

p %in% rownames(ess_mat_sec)

# ess_mat_sec[i,j] = 2 ... i is an essential supplier of j (Leontief impact)
# ess_mat_sec[i,j] = 1 ... i is a non-essential supplier of j (linear impact)
# ess_mat_sec[i,j] = 0 ... i is a non-relevant supplier of j (no impact)

# all entries are 1 --> every firm has a linear production function
# all entries are 2 --> every firm has a Leontief production function
ess_mat_sec




### save all code inputs generated
# data_wd <- "C:/Users/CD/Documents/GitHub/amoc_shock/test_data"
# ## 
# write.csv(as.matrix(W),
#           paste0(data_wd, "/W_dummy.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")
# 
# write.csv(p,
#           paste0(data_wd, "/p_dummy.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")
# 
# write.csv(as.matrix(ess_mat_sec),
#           paste0(data_wd, "/ess_mat_sec_dummy.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")




# take a look at the example:
library(igraph)
library(colorspace)

col_vec <- qualitative_hcl(length(unique(p)), palette = "Dark 2")
colrs <- col_vec[as.numeric(p)]


net <- graph_from_adjacency_matrix(W)
V(net)$color <- colrs
V(net)$label.font <- 2
V(net)$label.cex <- 2
V(net)$label.color <- "black"
V(net)$size <- sqrt(Matrix::rowSums(W) + Matrix::colSums(W)) * 15
E(net)$width <- 1.5
E(net)$arrow.size <- 0.3
E(net)$arrow.width <- 3
set.seed(2)
plot(net, #layout=layout_with_lgl(net), 
     vertex.frame.color ="white", edge.color = "black")
legend(x=-3, y=2.2, legend = unique(sort(p)), pch = 16, col = colrs, ncol = 5, cex=1.1, 
       bty="n")


# run GLcascade:

# explain for the input variables
??GLcascade::GL_cascade

ESRI <- GL_cascade(W = example_data$W,
                   p = example_data$p,
                   p_market = FALSE,
                   p_sec_impacts = FALSE,   
                   ess_mat_sec =  example_data$ess_mat_sec,
                   h_weights = FALSE,       
                   sec_aggr_weights = FALSE, 
                   psi_mat = FALSE,
                   revenue = FALSE,
                   costs = FALSE,
                   track_h = TRUE,
                   track_sector_impacts = FALSE, 
                   track_conv = TRUE,
                   conv_type = 1,
                   eps = 10^-2,
                   use_rcpp = FALSE,
                   ncores = 0,
                   run_id = "test_cascade",
                   load_balance = FALSE
)

ESRI$run_info
### output is a list
length(ESRI)
names(ESRI)

## the ESRI itself 
colnames(ESRI$ESRI)

# the ESRI itself (upstream and downstream shocks merged)
ESRI$ESRI[,"ESRI_weight_1"]

# the ESRI downstream shocks only
ESRI$ESRI[,"ESRI_d_weight_1"]

# the ESRI upstream shocks only
ESRI$ESRI[,"ESRI_u_weight_1"]

# explain output to console

ranks <- order(ESRI$ESRI[,1], decreasing = TRUE)
barplot(ESRI$ESRI[ranks,"ESRI_weight_1"],
     ylab= "ESRI", xlab = "firm ID (rank sorted)",
     names = ranks, 
     main = "Systemic Risk Profile")

barplot(ESRI$ESRI[ranks,"ESRI_d_weight_1"],
        ylab= "ESRI downstream", xlab = "firm ID (rank sorted)",
        names = ranks, 
        main = "Systemic Risk Profile")

barplot(ESRI$ESRI[ranks,"ESRI_u_weight_1"],
        ylab= "ESRI upstream", xlab = "firm ID (rank sorted)",
        names = ranks, 
        main = "Systemic Risk Profile")


# convergence statistic: number of iterations
plot(ESRI$ESRI_conv[ranks,3], 
     ylab="number of iterations until convergence")

# h_mat




#==============================================================================#
############# using the dummy network from Session 1 ########################### 
#==============================================================================#

data_wd <- "https://raw.githubusercontent.com/ch-diem/esri_tutorial/refs/heads/main/data/"

W_sf <- as.matrix(read.csv(file = paste0(data_wd, "example_net_scale_free_101_4000.csv")))
W_er <- as.matrix(read.csv(file = paste0(data_wd, "example_net_erdos_reny_101_213.csv") ))
industries <- read.csv(file = paste0(data_wd, "industry_vector.csv"))

sectors <- sort(unique(industries$industry_id))

ess_mat_sec_lin <- matrix(1, nrow = length(sectors), ncol = length(sectors))
rownames(ess_mat_sec_lin) <- colnames(ess_mat_sec_lin) <- sectors


p_sf <- industries$industry_id




ESRI <- GL_cascade(W = W_sf,
                   p = p_sf,
                   p_market = p_sf,
                   p_sec_impacts = FALSE,   
                   ess_mat_sec =  ess_mat_sec_lin,
                   h_weights = FALSE,       
                   sec_aggr_weights = FALSE, 
                   psi_mat = FALSE,
                   revenue = FALSE,
                   costs = FALSE,
                   track_h = TRUE,
                   track_sector_impacts = FALSE, 
                   track_conv = TRUE,
                   conv_type = 1,
                   eps = 10^-2,
                   use_rcpp = FALSE,
                   ncores = 0,
                   run_id = "test_cascade",
                   load_balance = FALSE
)


dev.off()
hist(ESRI$ESRI_conv[,3], 
     xlab = "number of iterations")


ranks <- order(ESRI$ESRI[,1], decreasing = TRUE)
plot(ESRI$ESRI[ranks,"ESRI_weight_1"],
     ylab= "ESRI", xlab = "firm ID (rank sorted)",
     main = "Systemic Risk Profile", log= "x")






### A few sensitivity checks

# use replaceability by setting p_market = p_sf

# use revenue / cost renormalization

set.seed(1)
revenue_sf <- rowSums(W_sf) * (1+ runif(dim(W_sf)[1], 0.2, 0.4))

costs_sf <- rowSums(W_sf) * (1+ runif(dim(W_sf)[1], 0.2, 0.4))


# vary ess_mat_sec elements

# all sectors are non-essential for each other
ess_mat_sec_lin[] <- 1


# increase essentialness of sector 2 for sector 4
ess_mat_sec_lin[2, c(4)] <- 2


ESRI_2 <- GL_cascade(W = W_sf,
                   p = p_sf,
                   p_market = p_sf,
                   p_sec_impacts = FALSE,   
                   ess_mat_sec =  ess_mat_sec_lin,
                   h_weights = FALSE,       
                   sec_aggr_weights = FALSE, 
                   psi_mat = FALSE,
                   revenue = revenue_sf,
                   costs = costs_sf,
                   track_h = TRUE,
                   track_sector_impacts = FALSE, 
                   track_conv = TRUE,
                   conv_type = 1,
                   eps = 10^-2,
                   use_rcpp = FALSE,
                   ncores = 0,
                   run_id = "test_cascade",
                   load_balance = FALSE
)



par(mfrow=c(1,2))
par(mar = c(6,5,0,0)+0.1)

ylim_upper <- max( c( ESRI$ESRI[,1], ESRI_2$ESRI[,1] ))

sizes <- lapply(1:4, function(x) ( ESRI$ESRI[,1])[p==x])
boxplot(sizes,  
        xlab="", ylab="ESRI",
        cex.lab = 1.5, cex.axis = 1.5,
        names = c("Agric.", "Retail", "Food_m", "Whol.s."), 
        las=2, 
        col = col_vec,
        ylim=c(0, ylim_upper))

sizes <- lapply(1:4, function(x) ( ESRI_2$ESRI[,1])[p==x])
boxplot(sizes,  
        xlab="", ylab="ESRI",
        cex.lab = 1.5, cex.axis = 1.5,
        names = c("Agric.", "Retail", "Food_m", "Whol.s."), 
        las =2,
        col = col_vec,
        ylim=c(0, ylim_upper))






#==============================================================================#
############# Calculating Systemic Risk on real data ########################### 
#==============================================================================#

# to dos: 
# allow for submitting sector strings that can not be converted to numeric
# provide also ess_mat_n2_ihs , 


### load the network data and store it as a sparse matrix



icio <- read.csv(file = paste0(data_wd, "2020.SML.csv"))
icio[1:5, 1:5]
rownames(icio) <- icio[,1]
# drop first column 
icio <- icio[,-1]
rownames(icio)
icio[1:5, 1:5]

# get last country/industry pair row index (ROW... rest of the world)
grep("ROW_T", rownames(icio)) == grep("ROW_T", colnames(icio))
grep("ROW_T", colnames(icio))

# convert to matrix
W_icio <- as.matrix(icio[1:grep("ROW_T", rownames(icio)), 1:grep("ROW_T", rownames(icio))])
sum(W_icio == 0)
sum(W_icio <= 1)


# make sparse
W_icio <- Matrix::Matrix(W_icio)
Matrix::image(W_icio[1:300, 1:300])

# threshold links that are smaller than 1 million 
W_icio[W_icio <= 1] <- 0
Matrix::image(W_icio[1:300, 1:300])


country_sec_pair <- rownames(W_icio)

# extract the NACE sectors
p_icio <- substr(country_sec_pair, start = 5, stop = nchar(country_sec_pair))

sectors <- sort(unique(p_icio))
sectors


# take only the first mentioned NACE if the sector refers to more than one
sectors_short <- substr(sectors, 1,3)
# we did not lose any
length(unique(sectors_short))
sort(sectors_short)

# correspondence table to NACE 4 digit entry in the ess_mat_n4_ihs file
icio_secs_to_NACE4 <- sapply(substr(sectors, 1,3), function(x) min(grep(x, (nace_conv_mat[, "nace1_2"]))))


# 
p_icio <- substr(p_icio, 1,3)

# gives the row in the nace_conv_mat[, "nace4_num"]
p_icio <- icio_secs_to_NACE4[p_icio]

# gives the nace4 digit codes and trims it to nace2 digit
p_icio <- substr(nace_conv_mat[p_icio, "nace4_num"], start = 1, stop = 2)





ess_mat_sec_icio <- Matrix(ess_mat_n4_ihs[icio_secs_to_NACE4 , icio_secs_to_NACE4])
rownames(ess_mat_sec_icio) <- colnames(ess_mat_sec_icio) <- substr(colnames(ess_mat_sec_icio), 1, 2)

Matrix::image(ess_mat_sec_icio)



# linear essential non-essential matrix

ess_mat_sec_icio_lin <- ess_mat_sec_icio
ess_mat_sec_icio_lin[ess_mat_sec_icio_lin > 0] <- 1

sort(unique(p_icio)) == rownames(ess_mat_sec_icio)


plot(as.numeric(colnames(ess_mat_sec_icio)) - as.numeric(sort(unique(p_icio))))

dim(ess_mat_sec_lin)
length(sort(unique(p_icio)))


### revenue correction to account for final demand; could integrate final demand as own node

total_output <- icio[1:grep("ROW_T", rownames(icio)), "OUT"]

### material cost correction not needed as all sectors are included already


### save all code inputs generated

## 
# write.csv(as.matrix(W_icio),
#           paste0(data_wd, "/W_icio.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")
# 
# write.csv(p_icio,
#           paste0(data_wd, "/p_icio.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")
# 
# write.csv(as.matrix(ess_mat_sec_icio),
#           paste0(data_wd, "/ess_mat_sec_icio.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")
# 
# write.csv(total_output,
#           paste0(data_wd, "/total_output_icio.csv"), 
#           row.names = FALSE, fileEncoding = "UTF-8")

# specify for how many firms the systemic risk index should be computed (to allow for a runtime guess)
dim(W_icio)
n_test <- 500
psi_mat <- Matrix::Diagonal(dim(W_icio)[1])[,1:n_test] # sub-setting the columns can be used to test the cascade for a subsample of firms (now its 10)

# to calculate the systemic risk for all firms use:
# psi_mat <- Matrix::Diagonal(dim(W)[1])

# run the cascade on a subset of firms first: this is controlled by using only the first 10 columns in the psi_mat argument

ESRI_icio <- GL_cascade(W = W_icio,
                   p = p_icio,
                   p_market = as.numeric(p_icio),
                   p_sec_impacts = FALSE,   
                   ess_mat_sec =  ess_mat_sec_icio,
                   h_weights = cbind(rowSums(W_icio), colSums(W_icio), rowSums(W_icio) + colSums(W_icio)),  #     
                   sec_aggr_weights = FALSE, 
                   psi_mat = psi_mat,
                   revenue = total_output,
                   costs = FALSE,
                   track_h = TRUE,
                   track_sector_impacts = FALSE, 
                   track_conv = TRUE,
                   conv_type = 1,
                   eps = 10^-2,
                   use_rcpp = TRUE,
                   ncores = parallel::detectCores()-1,
                   run_id = "ESRI_icio",
                   load_balance = TRUE
)

ESRI_icio$run_info

hist(ESRI_icio$ESRI_conv[,3], 
     xlab = "number of iterations")


ranks <- order(ESRI_icio$ESRI[,1], decreasing = TRUE)
plot(ESRI_icio$ESRI[ranks,"ESRI_weight_1"],
        ylab= "ESRI", xlab = "firm ID (rank sorted)",
        main = "Systemic Risk Profile", log= "x", 
     cex = 0.5)


esri_country_sector_mat <- cbind(as.matrix(round(ESRI_icio$ESRI[,1], 3)), 
                                 country_sec_pair[1:n_test])[ranks[1:n_test],]
colnames(esri_country_sector_mat) <-  c("ESRI_value", "Country_Sector")
esri_country_sector_mat <- data.frame(esri_country_sector_mat)

unique(nace_conv_mat[, c(5, 9) ])


# # save results 
# saveRDS(ESRI_icio, paste0(data_wd, "/ESRI_result.rds"))
# 
# write.csv(esri_country_sector_mat,
#         paste0(data_wd, "/ESRI_result_icio.csv"), 
#         row.names = FALSE, fileEncoding = "UTF-8")




### cascade without replaceability (sigma)

ESRI_icio_norep <- GL_cascade(W = W_icio,
                        p = p_icio,
                        p_market = FALSE,
                        p_sec_impacts = FALSE,   
                        ess_mat_sec =  ess_mat_sec_icio,
                        h_weights = cbind(rowSums(W_icio), colSums(W_icio), rowSums(W_icio) + colSums(W_icio)),  #     
                        sec_aggr_weights = FALSE, 
                        psi_mat = psi_mat,
                        revenue = total_output,
                        costs = FALSE,
                        track_h = TRUE,
                        track_sector_impacts = FALSE, 
                        track_conv = TRUE,
                        conv_type = 1,
                        eps = 10^-2,
                        use_rcpp = TRUE,
                        ncores = 10,#parallel::detectCores()-3,
                        run_id = "ESRI_icio",
                        load_balance = TRUE
)

ESRI$run_info

hist(ESRI_icio_norep$ESRI_conv[,3], 
     xlab = "number of iterations")


ranks <- order(ESRI_icio_norep$ESRI[,1], decreasing = TRUE)
plot(ESRI_icio_norep$ESRI[ranks,"ESRI_weight_1"],
     ylab= "ESRI", xlab = "firm ID (rank sorted)",
     main = "Systemic Risk Profile", log= "x", 
     cex = 0.5)


cbind(as.matrix(round(ESRI_icio_norep$ESRI[,1], 3)), country_sec_pair[1:n_test])[ranks[1:10],]


unique(nace_conv_mat[, c(5, 9) ])


# save results 
saveRDS(ESRI_icio_norep, paste0(data_wd, "/ESRI_result_norep.rds"))


ess_mat_sec_icio_lin <- ess_mat_sec_icio
ess_mat_sec_icio_lin[] <- 1

ESRI_icio_lin <- GL_cascade(W = W_icio,
                              p = p_icio,
                              p_market = FALSE, as.numeric(p_icio),
                              p_sec_impacts = FALSE,   
                              ess_mat_sec =  ess_mat_sec_icio_lin,
                              h_weights = cbind(rowSums(W_icio), colSums(W_icio), rowSums(W_icio) + colSums(W_icio)),  #     
                              sec_aggr_weights = FALSE, 
                              psi_mat = psi_mat,
                              revenue = total_output,
                              costs = FALSE,
                              track_h = TRUE,
                              track_sector_impacts = FALSE, 
                              track_conv = TRUE,
                              conv_type = 1,
                              eps = 10^-2,
                              use_rcpp = TRUE,
                              ncores = 10,#parallel::detectCores()-3,
                              run_id = "ESRI_icio",
                              load_balance = TRUE
)

ESRI$run_info

hist(ESRI_icio_lin$ESRI_conv[,3], 
     xlab = "number of iterations")


ranks <- order(ESRI_icio_lin$ESRI[,1], decreasing = TRUE)
plot(ESRI_icio_lin$ESRI[ranks,"ESRI_weight_1"],
     ylab= "ESRI", xlab = "firm ID (rank sorted)",
     main = "Systemic Risk Profile", log= "x", 
     cex = 0.5)


cbind(as.matrix(round(ESRI_icio_lin$ESRI[,1], 3)), country_sec_pair[1:n_test])[ranks[1:10],]


unique(nace_conv_mat[, c(5, 9) ])


# save results 
saveRDS(ESRI_icio_lin, paste0(data_wd, "/ESRI_result_lin.rds"))


ess_mat_sec_icio_leo <- ess_mat_sec_icio
ess_mat_sec_icio_leo[] <- 2

ESRI_icio_leo <- GL_cascade(W = W_icio,
                            p = p_icio,
                            p_market = as.numeric(p_icio),
                            p_sec_impacts = FALSE,   
                            ess_mat_sec =  ess_mat_sec_icio_leo,
                            h_weights = cbind(rowSums(W_icio), colSums(W_icio), rowSums(W_icio) + colSums(W_icio)),  #     
                            sec_aggr_weights = FALSE, 
                            psi_mat = psi_mat,
                            revenue = total_output,
                            costs = FALSE,
                            track_h = TRUE,
                            track_sector_impacts = FALSE, 
                            track_conv = TRUE,
                            conv_type = 1,
                            eps = 10^-2,
                            use_rcpp = TRUE,
                            ncores = 10,#parallel::detectCores()-3,
                            run_id = "ESRI_icio",
                            load_balance = TRUE
)

ESRI$run_info

hist(ESRI_icio_leo$ESRI_conv[,3], 
     xlab = "number of iterations")


ranks <- order(ESRI_icio_leo$ESRI[,1], decreasing = TRUE)
plot(ESRI_icio_leo$ESRI[ranks,"ESRI_weight_1"],
     ylab= "ESRI", xlab = "firm ID (rank sorted)",
     main = "Systemic Risk Profile", log= "x", 
     cex = 0.5)


cbind(as.matrix(round(ESRI_icio_leo$ESRI[,1], 3)), country_sec_pair[1:n_test])[ranks[1:10],]


unique(nace_conv_mat[, c(5, 9) ])


# save ESRI_icio_leo 
saveRDS(ESRI_icio_leo, paste0(data_wd, "/ESRI_icio_leo.rds"))
