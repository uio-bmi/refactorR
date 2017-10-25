#' Plot the counts of a gene in contrasting groups given a counts_matrix  and an official gene symbol
#'
#' @description \code{plot_counts} Plot the counts of a gene in contrasting groups given a counts_matrix  and an official gene symbol
#'
#' @param datamat A counts matrix with oficial gene symbol as rownames
#' @param gene An official gene symbol
#' @param group1_cols The column numbers corresponding to group-1
#' @param group2_cols The column numbers corresponding to group-2
#' @param group3_cols The column numbers corresponding to group-3
#' @param group_names Names of the groups to appear in the plot labels
#' @param batch_names A character vector of length two - indicating the names of two batches
#' @param batch1_cols A numeric vector indicating the column indices of batch-1
#' @param batch2_cols A numeric vector indicating the column indices of batch-2
#' @param paired A boolean indicating whether paired samples should be joined by a line plot
#' @param pair_replis A numeric vector of paired samples
#' @return An interactive plot
#' @examples
#' plot_counts(counts_matrix,gene='IL18', group1_cols=c(1,2,3,4),group2_cols=c(5,6,7,8),group3_cols=c(9,10,11,12),group_names=c('A','B','C'),batch_names=c("Batch-1","Batch-2"),batch1_cols=c(1:7,14:18),batch2_cols=c(8:13,19:24),paired = TRUE,pair_replis=c(1,2,3,4,5,6,7,8,9,10,11,5,6,7,8,9,10,11))
#' @export

# library(ggplot2)
# library(ggthemes)
# library(plotly)
library(reshape2)


.preparePlots <- function(datamat,gene,group1_cols,group2_cols,group3_cols,group_names,batch_names,batch1_cols,batch2_cols, paired=FALSE, pair.replis=NULL) {
    gene.data <- melt(datamat[gene,])
    gene.data$group <- 0
    gene.data$group[group1_cols] <- group_names[1]
    gene.data$group[group2_cols] <- group_names[2]
    gene.data$group[group3_cols] <- group_names[3]
    gene.data$sample <- rownames(gene.data)
    gene.data$batch <- 0
    gene.data$batch[batch1_cols] <- batch_names[1]
    gene.data$batch[batch2_cols] <- batch_names[2]
    if (paired == 'TRUE') {
        pair.replis <- paste0("s",pair.replis)
        gene.data$pairs <- pair.replis
    }
    return(gene.data)
}

.plotPreparedDataFrame <- function(gene.data, paired=FALSE) {
  if (paired=="FALSE") {
    p1<-ggplot(data=gene.data, aes(x=group,y=value,label=sample,color=batch))+geom_point()+xlab('groups')+ylab('log expression')+theme_minimal()
  }
  if (paired=='TRUE') {
    p1<-ggplot(data=gene_data,aes(x=group,y=value,label=sample,color=batch))+geom_point()+geom_line(aes(group=pairs))+xlab('groups')+ylab('log expression')+theme_minimal()
  }
  ggplotly(p1)
}

plotCounts <- function(datamat, gene, group1_cols, group2_cols,group3_cols,
                       group_names,batch_names,batch1_cols,batch2_cols,
                       paired = FALSE,pair.replis=NULL) {
    gene.data <- .preparePlots(datamat,gene,group1_cols,group2_cols,group3_cols,group_names,batch_names,batch1_cols,batch2_cols, paired, pair.replis)
    return(.plotPreparedDataFrame(gene.data, paired))
}

