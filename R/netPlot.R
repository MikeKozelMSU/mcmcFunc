#' run netPlot function
#'
#' More details
#'
#' @param beta
#'
#' @return a graph
#'
#' @export



# netplot func
netPlot = function(beta){
  lapply(BPS, function(beta){
    # fname=paste0(outPath, beta[[4]], '_net.pdf')
    # pdf(file=fname, width=12, height=5)
    fname=paste0(outPath, beta[[4]], '_net.eps')
    #postscript(file=fname, width=12, height=5, fonts=c("serif", "Palatino"), horizontal=FALSE, onefile = FALSE, paper = "special")
    par(mfrow=c(1,2), mar=c(1,1,1,1), mgp=c(1.5,.5,0))
    B = beta[[1]]
    LB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=alpha )
    UB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=1-alpha )
    BSIG = 1*( LB*UB >0 )
    BPOS = 1*( LB>0 )
    rownames(BPOS)=colnames(BPOS)=dimnames(beta[[1]])[[1]]
    g = graph.adjacency(BPOS, mode='directed', diag=FALSE)
    set.seed(6886)
    plot.igraph(g,
                layout=layout.auto,
                vertex.label.color=tcols,
                vertex.color=ccols,
                vertex.label.cex=(igraph::degree(g)+3.5)/19,
                vertex.size=igraph::degree(g)+3.5,
                edge.arrow.size=0.4,
                asp=FALSE
    )

    B = beta[[2]]
    LB = apply(B,c(1,2),quantile,prob=alpha)
    UB = apply(B,c(1,2),quantile,prob=1-alpha)
    BSIG = 1*( LB*UB >0 )
    BPOS = 1*(LB>0)
    rownames(BPOS)=colnames(BPOS)=dimnames(beta[[1]])[[1]]
    g = graph.adjacency(BPOS, mode='directed', diag=FALSE)
    set.seed(6886)
    plot.igraph(g,
                layout=layout.auto,
                vertex.label.color=tcols,
                vertex.color=ccols,
                vertex.label.cex=(igraph::degree(g)+3.5)/19,
                vertex.size=igraph::degree(g)+3.5,
                edge.arrow.size=0.4,
                asp=FALSE
    )
    #dev.off()
  })
}
