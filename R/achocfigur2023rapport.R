#' Adhocfunksjon for årsrapportfigur
#'
#' @export
#'
achocfigur2023rapport <- function(valgtVar = "Malign",
                                    tittel = "Testtittel",
                                    Grvar1 = "Op_gr",
                                    Grvar2 = "Tilgang_utvidet",
                                    outfile='', ...)
{
  NorgastUtvalg <- NorgastUtvalg(...)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt
  NutvTxt <- length(utvalgTxt)

  aux <- RegData %>%
    dplyr::filter(!is.na(!! sym(valgtVar )),
                  !is.na(!! sym(Grvar1 )),
                  !is.na(!! sym(Grvar2 ))) %>%
    dplyr::summarise(N = dplyr::n(),
                     Antall = sum(!! sym(valgtVar )),
                     Andel = Antall/N*100,
                     .by = c(!! sym(Grvar1 ), !! sym(Grvar2 )))

  Andeler <- aux %>% dplyr::select(-N, -Antall) %>%
    tidyr::pivot_wider(names_from = !! sym(Grvar2 ),
                       values_from = Andel,
                       names_sort = TRUE) %>%
    dplyr::arrange(desc(!! sym(Grvar1 )))

  N <- aux %>% dplyr::select(-Andel, -Antall) %>%
    tidyr::pivot_wider(names_from = !! sym(Grvar2 ),
                       values_from = N,
                       names_sort = TRUE) %>%
    dplyr::arrange(desc(!! sym(Grvar1 )))

  PlotAndeler <- as.data.frame(Andeler)
  rownames(PlotAndeler) <- PlotAndeler[, Grvar1]
  PlotAndeler<- as.matrix(PlotAndeler[,-1]) %>% t()

  ngrvar2level <- dim(Andeler)[2]-1
  ymax <- 1.5 + (nlevels(RegData[[Grvar2]])+1) * nlevels(RegData[[Grvar1]])

  FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                  pointsizePDF=11, fargepalett='BlaaOff')
  farger <- rev(FigTypUt$farger)
  par('oma'=c(0,1,NutvTxt,0))
  par('mar'=c(5.1, 5.1, 5.1, 2.1))

  pos <- barplot(PlotAndeler, horiz=T, beside=TRUE, border=NA, col=farger[1:ngrvar2level],
                 main='', font.main=1, ylim = c(0,ymax),
                 xlim = c(0, min(max(PlotAndeler)*1.2, 100)),
                 xlab='', las=1, cex.names = 1.2,
                 names.arg=rep('',dim(PlotAndeler)[2]))

  mtext(colnames(PlotAndeler), side=2, line=0.2, las=1, at=colMeans(pos), col=1, cex=1)

  Nlang <- N %>% pivot_longer(cols = -1,
                              names_to = "Grvar2", values_to = "N")
  Nlang$pos <- as.vector(pos)
  Nlang$N <- paste0("N=", Nlang$N)
  text(x = 0, y = Nlang$pos, labels = Nlang$N, pos = 4)

  title(tittel)
  legend("top",
         legend = levels(RegData[, Grvar2]),
         pch=15, col=farger[1:ngrvar2level], ncol = ngrvar2level,
         bty='n')

  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=(NutvTxt-1):0, outer=TRUE)


  if ( outfile != '') {dev.off()}

}


# achocfigur2023rapport <- function(outfile='', ...)
# {
#   NorgastUtvalg <- NorgastUtvalg(...)
#   RegData <- NorgastUtvalg$RegData
#   utvalgTxt <- NorgastUtvalg$utvalgTxt
#   NutvTxt <- length(utvalgTxt)
#
#   aux <- RegData %>%
#     dplyr::filter(!is.na(KumAcc),
#                   Op_gr %in% 1:5) %>%
#     dplyr::mutate(Op_gr = factor(Op_gr, levels = 1:5,
#                                  labels = c("Kolon", "Rektum", "Øsofagus",
#                                             "Ventrikkel", "Lever")),
#                   Tilgang_utvidet = dplyr::case_when(
#                     Tilgang_utvidet == 4 ~ 2,
#                     Tilgang_utvidet == 5 ~ 3,
#                     Tilgang_utvidet %in% 1:3 ~ Tilgang_utvidet
#                   ),
#                   Tilgang_utvidet = factor(
#                     Tilgang_utvidet, levels = 1:3,
#                     labels = c("Åpen",
#                                "Laparoskopisk",
#                                "Robotassistert"))) %>%
#     dplyr::summarise(N = dplyr::n(),
#                      Antall = sum(KumAcc),
#                      Andel = Antall/N*100,
#                      .by = c(Tilgang_utvidet, Op_gr))
#
#   Andeler <- aux %>% dplyr::select(-N, -Antall) %>%
#     tidyr::pivot_wider(names_from = Tilgang_utvidet,
#                        values_from = Andel,
#                        names_sort = TRUE) %>%
#     dplyr::arrange(desc(Op_gr))
#
#   N <- aux %>% dplyr::select(-Andel, -Antall) %>%
#     tidyr::pivot_wider(names_from = Tilgang_utvidet,
#                        values_from = N,
#                        names_sort = TRUE) %>%
#     dplyr::arrange(desc(Op_gr))
#
#   PlotAndeler <- as.data.frame(Andeler)
#   rownames(PlotAndeler) <- PlotAndeler$Op_gr
#   PlotAndeler<- as.matrix(PlotAndeler[,-1]) %>% t()
#
#
#   FigTypUt <- rapFigurer::figtype(outfile=outfile,
#                                   pointsizePDF=11, fargepalett='BlaaOff')
#   farger <- rev(FigTypUt$farger)
#   par('oma'=c(0,1,NutvTxt,0))
#   par('mar'=c(5.1, 4.1, 5.1, 2.1))
#
#   pos <- barplot(PlotAndeler, horiz=T, beside=TRUE, border=NA, col=farger[1:3],
#                  main='', font.main=1, ylim = c(0,23),
#                  xlim = c(0, min(max(PlotAndeler)*1.2, 100)),
#                  xlab='', las=1, cex.names = 1.2,
#                  names.arg=rep('',dim(PlotAndeler)[2]))
#
#   mtext(colnames(PlotAndeler), side=2, line=0.2, las=1, at=colMeans(pos), col=1, cex=1)
#
#   Nlang <- N %>% pivot_longer(cols = c(Åpen, Laparoskopisk, Robotassistert),
#                               names_to = "Tilgang", values_to = "N")
#   Nlang$pos <- as.vector(pos)
#   Nlang$N <- paste0("N=", Nlang$N)
#   text(x = 0, y = Nlang$pos, labels = Nlang$N, pos = 4)
#
#   title("Andel komplikasjoner (Accordion >=3)")
#   legend("top",
#          legend = c("Åpen",
#                     "Laparoskopisk",
#                     "Robotassistert"),
#          pch=15, col=farger[1:3], ncol = 3,
#          bty='n')
#
#   # mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=-1, outer=TRUE)
#   mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=(NutvTxt-1):0, outer=TRUE)
#
#   if ( outfile != '') {dev.off()}
#
# }

