#' @title Define Segments in y-Axis for 'ggplot2'
#' @description Easy to define segments in y-axis for 'ggplot2'.
#' @param plot A 'ggplot2' plot.
#' @param ylim The y-axis limits.
#' @param segments The interval of a segment. If more than one intervals are given, please use list() to concatenate them.
#' @param tick_width One or more numbers for each segmented y-axis.
#' @param rel_heights Numerical vector of relative segmented y-axis and segments heights, default is 1 and 0.
#' @param vjust Vertical justification. Default = 0 (baseline at y).
#' @param margin Margins around the text.
#' @param ... Arguments will be handed to plot_grid() in 'cowplot'.
#' @importFrom ggplot2 coord_cartesian theme scale_y_continuous ylab element_blank element_line unit
#' @importFrom cowplot plot_grid draw_label
#' @return A segmented picture.
#' @export
#'
#' @examples
#' data(mtcars)
#' library(ggplot2)
#' p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
#'     geom_bar() +
#'     ggtitle("Number of Cars by Gear") +
#'     xlab("Gears")
#'
#' #single segments and missing tick_width
#' gg.gap(plot=p,
#'        segments=c(5,10),
#'        ylim=c(0,50))
#' #tick_width can be one or more numbers
#' gg.gap(plot=p,
#'        segments=c(5,10),
#'        tick_width = c(1,10),
#'        ylim=c(0,50))
#' #segments list cantains more than one number vectors
#' gg.gap(plot=p,
#'        segments=list(c(2.5,4),c(5,10)),
#'        tick_width = c(1,0.5,10),
#'        ylim=c(0,50))
#' #rel_heights can set the relative height for segments and segmented y-axis
#' gg.gap(plot=p,
#'        segments=list(c(2.5,4),c(5,10)),
#'        tick_width = c(1,0.5,10),
#'        rel_heights=c(0.2,0,0.2,0,1),
#'        ylim=c(0,50))
#' #reversed y-axis
#' p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
#'     geom_bar() +
#'     ggtitle("Number of Cars by Gear") +
#'     xlab("Gears")+
#'     scale_y_continuous(trans = 'reverse')
#' #single segments and missing tick_width
#' gg.gap(plot=p,
#'        segments=c(10,5),
#'        ylim=c(15,0))
gg.gap <- function(plot,ylim,segments,tick_width,rel_heights,vjust=0,margin=c(top=1,right=2,bottom=1,left=1),...){
    #check whether segments is list
    if (!is.list(segments)){
        segments=list(segments)
    }
    #get and check y limits
    if (all(missing(ylim),is.null(plot$coordinates$limits$y))){
        stop('ylim is undefined')
    }else if(ylim[1]==ylim[2]){
        stop('ylim should not be the same number')
    }else if (missing(ylim)){
        ylim=plot$coordinates$limits$y
    }
    #check segments in order from small to large or from large to small
    for (j in 1:length(segments)) {
        seg1=segments[[j]][1]
        seg2=segments[[j]][2]
        if (seg1 > seg2){
            if (ylim[1]<ylim[2]){ #y-axis is from small to large
               msg=paste0('No.',j,' segment: c(',seg1,',',seg2,') is wrong. It should be ','c(',seg2,',',seg1,')')
                stop(msg)
            }
        }else if(seg1 < seg2){
            if (ylim[1]>ylim[2]){ #y-axis is from large to small
                msg=paste0('No.',j,' segment: c(',seg1,',',seg2,') is wrong. It should be ','c(',seg2,',',seg1,')')
                stop(msg)
            }
        }else if(seg1==seg2){
            msg=paste0('No.',j,' segment: c(',seg1,',',seg2,') is wrong. tick_width should not be equal')
            stop(msg)
        }
    }
    # check segments vectors sequence from large to small or from small to large
    if (length(segments)>=2){
        if (ylim[1] < ylim[2]){
            for (k in 2:length(segments)) {
                pre.2=segments[[k-1]][2]
                suf.1=segments[[k]][1]
                if (pre.2 > suf.1){
                    pre=paste0('c(',segments[[k-1]][1],',',segments[[k-1]][2],')')
                    suf=paste0('c(',segments[[k]][1],',',segments[[k]][2],')')
                    msg=paste0('Segments ',k-1,' and ',k,': ',pre,',',suf,' are wrong. They should be ',suf,',',pre)
                    stop(msg)
                }
            }
        }else if (ylim[1] > ylim[2]){
            for (k in 2:length(segments)) {
                pre.2=segments[[k-1]][2]
                suf.1=segments[[k]][1]
                if (pre.2 < suf.1){
                    pre=paste0('c(',segments[[k-1]][1],',',segments[[k-1]][2],')')
                    suf=paste0('c(',segments[[k]][1],',',segments[[k]][2],')')
                    msg=paste0('Segments ',k-1,' and ',k,': ',pre,',',suf,' are wrong. They should be ',suf,',',pre)
                    stop(msg)
                }
            }
        }
    }
    if (ylim[1] < ylim[2]){
        #check the minimum of segments must be more than min of ylim
        if (min(unlist(segments)) <= ylim[1]) stop('the minimum of segments must be more than the minium of ylim')
        #check the maximum of segments must be lower than maximum of ylim
        if (max(unlist(segments)) > ylim[2]) stop('the maximum of segments must be lower than maximum of ylim')
    }else if (ylim[1] > ylim[2]){
        #check the minimum of segments must be more than min of ylim
        if (min(unlist(segments)) <= ylim[2]) stop('the minimum of segments must be more than the minium of ylim')
        #check the maximum of segments must be lower than maximum of ylim
        if (max(unlist(segments)) > ylim[1]) stop('the maximum of segments must be lower than maximum of ylim')
    }
    #auto add tick_width if missing
    if (missing(tick_width)){
        tick_width=rep(abs(ylim[2]-ylim[1])/10,(length(segments)+1))
    }
    #check and add tick_width
    if ((length(tick_width)-length(segments)) < 1){
        int_len=length(tick_width)
        for (m in (int_len+1):(length(segments)+1)) {
            tick_width[m]=tick_width[int_len]
        }
    }
    seg_heights=0
    y_heights=1
    #check and add seg_heights
    if (length(seg_heights)<length(segments)){
        seg_heights_len=length(seg_heights)
        for (m in (seg_heights_len+1):length(segments)) {
            seg_heights[m]=seg_heights[seg_heights_len]
        }
    }
    #check and add y_heights
    if (length(y_heights)<(length(segments)+1)){
        y_heights_len=length(y_heights)
        for (m in (y_heights_len+1):(length(segments)+1)) {
            y_heights[m]=y_heights[y_heights_len]
        }
    }
    #####          plot            ##########
    #get elements
    ##trans
    if (length(plot$scales$scales)==0){
        trans="identity"
    }else if ('trans' %in% names(plot$scales$scales[[1]])){
        trans=plot$scales$scales[[1]]$trans
    }else{
        trans="identity"
    }
    if ('reverse' %in% trans){
        if (ylim[1] < ylim[2]){
            msg=paste0('ylim: ','c(',ylim[1],',',ylim[2],')',' is wrong. It should be ','c(',ylim[2],',',ylim[1],')')
            stop(msg)
        }
    }
    if ('identity' %in% trans){
        if (ylim[1] > ylim[2]){
            msg=paste0('ylim: ','c(',ylim[1],',',ylim[2],')',' is wrong. It should be ','c(',ylim[2],',',ylim[1],')')
            stop(msg)
        }
    }
    #loop to plot 3 parts
    #the lowest, median and the toppest part by segments
    for (i in 1:length(segments)) {
        gap=unlist(segments[i])
        if (i==1){
            #plot the lowest part
            if (ylim[1] < ylim[2]){
                breaks=seq(ylim[1],gap[1],by=tick_width[i])
            }else if (ylim[1] > ylim[2]){
                breaks=seq(gap[1],ylim[1],by=tick_width[i])
            }
            p_segment.i<-plot+coord_cartesian(ylim=c(ylim[1],gap[1]))+
                theme(panel.border = element_blank())+
                theme(axis.line.y=element_line(),
                      axis.line.x.bottom = element_line(),
                      plot.title = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank())+
                scale_y_continuous(expand = c(0,0),
                                   trans = trans,
                                   breaks = breaks)+
                ylab(label=NULL)
            p_segment=list(p_segment.i)
            names(p_segment)[length(p_segment)]=i
            rel_heigh=c(y_heights[i],seg_heights[i])
        }else{
            #plot the median part
            if (ylim[1] < ylim[2]){
                breaks=seq(ylim[1],gap[1],by=tick_width[i])
            }else if (ylim[1] > ylim[2]){
                breaks=seq(gap[1],ylim[1],by=tick_width[i])
            }
            p_segment.i<-plot+
                coord_cartesian(ylim=c(unlist(segments[i-1])[2],
                                       gap[1]))+
                theme(panel.border = element_blank())+
                theme(axis.line.y=element_line(),
                      #axis.line.x.bottom = element_line(),
                      legend.position = "none",
                      axis.text.x=element_blank(),
                      axis.ticks.x =element_blank(),
                      title = element_blank(),
                      axis.title.x=element_blank(),
                      strip.text.x = element_blank())+
                scale_y_continuous(expand = c(0,0),
                                   breaks = breaks,
                                   trans = trans)+
                ylab(label=NULL)
            #add y label in the middle median part
            p_segment=c(p_segment,list(NULL),list(p_segment.i))
            names(p_segment)[length(p_segment)]=i
            rel_heigh=c(rel_heigh,y_heights[i],seg_heights[i])
        }
        #plot the toppest part in the end
        if (i==length(segments)){
            if (ylim[1]<ylim[2]){
                breaks=seq(gap[2],ylim[2],by=tick_width[i+1])
            }else if (ylim[1]>ylim[2]){
                breaks=seq(ylim[2],gap[2],by=tick_width[i+1])
            }

            p_segment.i<-plot+
                coord_cartesian(ylim=c(gap[2],ylim[2]))+
                theme(panel.border = element_blank())+
                theme(axis.line.y=element_line(),
                      axis.line.x.top = element_line(),
                      #axis.line.x.bottom = element_line(),
                      legend.position = "none",
                      axis.text.x=element_blank(),
                      axis.ticks.x =element_blank(),
                      axis.title.x=element_blank())+
                scale_y_continuous(expand = c(0,0),
                                   breaks = breaks,
                                   trans = trans)+
                ylab(label=NULL)
            p_segment=c(p_segment,list(NULL),list(p_segment.i))
            names(p_segment)[length(p_segment)]=i+1
            rel_heigh=c(rel_heigh,y_heights[i])
        }
    }
    #reverse order
    p_segment=rev(p_segment)
    if (missing(rel_heights)){
        rel_heights=rev(rel_heigh)
    }else{
        rel_heights=rev(rel_heights)
    }
    if (is.null(plot$theme$axis.title.y$angle)) {
        angle=90
    }else{
        angle=plot$theme$axis.title.y$angle
    }
    plot_grid(plotlist = p_segment,ncol = 1,align = "v",
              rel_heights=rel_heights)+
        theme(plot.margin = unit(margin, "cm"))+
        draw_label(label = plot$labels$y,
                   x = 0,
                   hjust = plot$theme$axis.title.y$hjust,
                   vjust = vjust,
                   fontfamily = plot$theme$axis.title.y$family,
                   fontface = plot$theme$axis.title.y$face,
                   size = plot$theme$axis.title.y$size,
                   angle = angle,
                   lineheight = plot$theme$axis.title.y$lineheight,
                   colour = plot$theme$axis.title.y$colour)
}
