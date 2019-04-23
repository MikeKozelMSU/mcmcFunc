#' run postDistSumm function
#'
#' More details
#'
#' @param beta
#'
#' @return a graph
#'
#' @export


# postDistSumm func
postDistSumm = function(beta){
  burn = 1
  tmp=beta[[3]]
  tmp=tmp[,,burn:dim(tmp)[3]] # Burn 1k
  # Assemble data for coef
  coefData=NULL
  for(ii in 1:dim(tmp)[1]){
    mod=t(tmp[ii,,])
    modSumm=apply(mod, 2, summStats)
    rownames(modSumm)=c('mu', paste0(c('lo','up'),95), paste0(c('lo','up'),90))
    coefSlice = data.frame(iv=rownames(t(modSumm)), t(modSumm), row.names=NULL)
    coefSlice = cbind(dv = dimnames(tmp)[[1]][ii], coefSlice)
    coefData = rbind(coefData, coefSlice) }

  # Add in variable for colors
  coefData$sig = NULL
  coefData$sig[coefData$lo90 > 0 & coefData$lo95 < 0] = "Positive at 90"
  coefData$sig[coefData$lo95 > 0] = "Positive"
  coefData$sig[coefData$up90 < 0 & coefData$up95 > 0] = "Negative at 90"
  coefData$sig[coefData$up95 < 0] = "Negative"
  coefData$sig[coefData$lo90 < 0 & coefData$up90 > 0] = "Insig"
  coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255),
                   "Negative"= rgb(222, 45, 38, maxColorValue=255),
                   "Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
                   "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
                   "Insig" = rgb(150, 150, 150, maxColorValue=255))

  # Map variables to labels (very lazy way)
  coefData$dv = makeLabel(coefData$dv, long=TRUE)
  coefData$dv = factor(coefData$dv, levels=unique(coefData$dv))
  coefData$iv = makeLabel(coefData$iv, long=FALSE)
  coefData$iv = factor(coefData$iv, levels=unique(coefData$iv))

  # Plot
  # coefData$iv = factor(coefData$iv, levels=dimnames(beta[[3]])[[2]])
  ggCoef=ggplot(coefData, aes(x=iv, y=mu, color=sig))
  ggCoef=ggCoef + geom_point() + facet_wrap(~dv, nrow=2, scales = 'free_y')
  ggCoef = ggCoef + geom_linerange(aes(ymin=lo95, ymax=up95), alpha = .3, size = 0.3)
  ggCoef = ggCoef + geom_linerange(aes(ymin=lo90, ymax=up90),alpha = 1, size = 1)
  ggCoef = ggCoef + geom_errorbar(aes(ymin=lo95,ymax=up95),linetype = 1,width = 0.1)
  ggCoef=ggCoef + geom_hline(yintercept=0, color='red', linetype='dashed')
  ggCoef=ggCoef + geom_vline(xintercept=c(2.5, 4.5), color='grey', linetype='dashed')
  ggCoef=ggCoef + ylab('') + scale_colour_manual(values = coefp_colors)
  ggCoef=ggCoef + scale_x_discrete("", labels = parse(text = levels(coefData$iv)))
  ggCoef=ggCoef + theme(
    axis.ticks=element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    panel.background = element_blank(),
    legend.position='none',
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank()
  )
  return(ggCoef)
}
