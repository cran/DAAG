"hardcopy" <-
function(width=3.75, height=3.75, color=F, trellis=F,
             device=c("","pdf","ps"), path="", pointsize=c(8,4), horiz=F){

        ## 1 x 1: 2.25" x 2.25"
        ## 2 x 2: 2.75" x 2.75"
        ## 3 x 3: 3.75" x 3.75" or 3.25" x 3.25" for simple scatterplots
        ## 1 x 2: 4" x 2.25"
        ## 2 x 3: 4" x 2.8"
        ## 3 x 4: 4.5" x 3.25
        if(!trellis)pointsize <- pointsize[1]
        funtxt <- sys.call(1)
        fnam <- strsplit(as.character(funtxt), "(", fixed=T)[[1]][1]
        dotsplit <- strsplit(fnam, "\\.")[[1]]
        dotsplit[1] <- substring(dotsplit[1], 2)
        prefix1 <- paste(if(nchar(dotsplit[1])==1)"0" else "", dotsplit[1],
                         sep="")
        prefix2 <- paste(if(nchar(dotsplit[2])==1)"0" else "", dotsplit[2],
                         sep="")
        suffix <- switch(device, ps=".eps", pdf=".pdf")
        fnam <- paste("~/r-book/second/Art/",prefix1,"-",prefix2,
                      suffix, sep="")
        print(fnam)
        dev.out <- device[1]
        dev.fun <- switch(dev.out, pdf=pdf, ps=postscript)
        if(trellis){
            library(lattice)
            trellis.device(file=fnam, device=dev.fun,
                           bg="white", color = color,
                           width=width, height=height, horiz=horiz)
            lset(list(fontsize=list(text=pointsize[1], points=pointsize[2])))
        }
        else 
            if (dev.out!=""){
                print(c(width, height))
                dev.fun(file=fnam, paper="special",
                        enc="MacRoman", horiz=horiz,
                        width=width, height=height, pointsize=pointsize[1])
            }
    }

