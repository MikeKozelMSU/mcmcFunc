#' run tracePlot function
#'
#' More details
#'
#' @param beta
#'
#' @return a graph
#'
#' @export


# tracePlot func
tracePlot = function(beta){
  dvs=dim(beta[[3]])[1]
  traceData = NULL
  for(ii in 1:dvs){
    tbeta = data.frame( t(beta[[3]][ii,,]) )
    tbeta = cbind(dv=dimnames(beta[[3]])[[1]][ii], tbeta, Iteration=1:nrow(tbeta))
    traceData = rbind(traceData, tbeta)
  }
  traceData=traceData[traceData$Iteration %% 5 == 0,] # thin
  traceData = melt(traceData, id=c('dv', 'Iteration'))

  traceData$dv = makeLabel(traceData$dv, long=TRUE)

  ggTrace=ggplot(traceData, aes(x=Iteration, y=value, color=variable))
  ggTrace=ggTrace + geom_line() + facet_wrap(~dv)
  ggTrace=ggTrace + xlab('') + ylab('')
  ggTrace=ggTrace + scale_color_brewer(name='', type='qual')
  ggTrace=ggTrace + theme(
    axis.ticks=element_blank(),
    legend.position='none',
    panel.background=element_blank()
  )
  return(ggTrace)
}
