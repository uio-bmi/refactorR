source("plot_counts.r")

# Create Data Frame
mat <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=2, ncol=6)
test.frame <- as.data.frame(mat)
rownames(test.frame) <- c("a", "b")

identity.func <- function(x, .) {
    return(x)
}

# Test Unpaired
.plotPreparedDataFrame <- identity.func
prepared.matrix <- plotCounts(test.frame,
                              "a",
                              c(1,2), c(3, 4), c(5),
                              c("col1", "col2", "col3"),
                              c("b1", "b2"),
                              c(1,2,3), c(4,5,6))
values <- c(1,3,5,7,9,11)
col.names <- c("col1", "col1", "col2", "col2", "col3", 0)
batch.names <- c("b1", "b1", "b1", "b2", "b2", "b2")

stopifnot(prepared.matrix$value == values)
stopifnot(prepared.matrix$group == col.names)
stopifnot(prepared.matrix$batch == batch.names)

# Test Paired
pair.replis <- c(1,2,3)
true.pair.replis <- c("s1", "s2", "s3")
prepared.matrix <- plotCounts(test.frame,
                              "a",
                              c(1,2), c(3, 4), c(5),
                              c("col1", "col2", "col3"),
                              c("b1", "b2"),
                              c(1,2,3), c(4,5,6),
                              paired=TRUE, pair.replis=pair.replis)

stopifnot(prepared.matrix$value == values)
stopifnot(prepared.matrix$group == col.names)
stopifnot(prepared.matrix$batch == batch.names)
stopifnot(prepared.matrix$pairs == true.pair.replis)



                                
                                


