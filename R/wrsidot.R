## wrsi: weighted relative suitability index
.wrsi <-
function(Y, X)
{
    ## relative suitability index
    ## # of available units of type k / total # of available units (any type)
    Pavail <- colSums(X) / sum(X)
    ## # of used units of type k / total # of used units (any type)
    Xu <- X * Y
    ## sum(Xu) = sum(Y) except when rowsum != 1
    Pused <- colSums(Xu) / sum(Xu)
    ## crude weighted p-occ
    Pw <- colSums(Xu) / colSums(X)
    ## Weighted Relative Suitability Index
    WRSI <- Pused / Pavail
    #Var <- (1/colSums(Xu)) - (1/sum(Xu)) + (1/colSums(X)) - (1/sum(X))
    cbind(
        WRSI=WRSI,
        zWRSI=log(WRSI),
        rWRSI=(exp(2 * log(WRSI)) - 1)/(1 + exp(2 * log(WRSI))),
        Pused=Pused,
        Pavail=Pavail,
        Pw=Pw,
        u=colSums(Xu),
        a=colSums(X))
}
