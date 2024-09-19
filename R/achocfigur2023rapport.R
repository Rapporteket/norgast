#' Adhocfunksjon for årsrapportfigur
#'
#' @export
#'
norgastAndelGruppert2Gr <- function(valgtVar = "Malign",
                                    tittel = "Testtittel",
                                    Grvar1 = "Op_gr",
                                    Grvar2 = "Tilgang_utvidet",
                                    xtxt = "Andel (%)",
                                    ytxt = "",
                                    inkl_konf = FALSE,
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
                     konf_lav = binom.test(Antall,N, alternative = 'two.sided')$conf.int[1]*100,
                     konf_hoy = binom.test(Antall,N, alternative = 'two.sided')$conf.int[2]*100,
                     .by = c(!! sym(Grvar1 ), !! sym(Grvar2 )))


  Andeler <- aux %>% dplyr::select(!! sym(Grvar1 ), !! sym(Grvar2 ), Andel) %>%
    tidyr::pivot_wider(names_from = !! sym(Grvar2 ),
                       values_from = Andel,
                       names_sort = TRUE) %>%
    dplyr::arrange(desc(!! sym(Grvar1 )))

  N <- aux %>% dplyr::select(!! sym(Grvar1 ), !! sym(Grvar2 ), N) %>%
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
  par('mar'=c(5.1, 5.1, 5.1, 3.1))

  if (inkl_konf) {
    xmax <- min(max(aux$konf_hoy, na.rm = T)*1.2, 100)
  } else {xmax <- min(max(PlotAndeler, na.rm = T)*1.2, 100)}

  pos <- barplot(PlotAndeler, horiz=T, beside=TRUE, border=NA, col=farger[1:ngrvar2level],
                 main='', font.main=1, ylim = c(0,ymax),
                 xlim = c(0, xmax),
                 xlab=xtxt, las=1, cex.names = 1.2,
                 names.arg=rep('',dim(PlotAndeler)[2]))

  mtext(colnames(PlotAndeler), side=2, line=0.2, las=1, at=colMeans(pos), col=1, cex=1)
  mtext(ytxt, side=2, line=5, las=0, col=1, cex=1)


  Nlang <- N %>% pivot_longer(cols = -1,
                              names_to = Grvar2, values_to = "N")
  Nlang$pos <- as.vector(pos)
  # Nlang$N <- paste0("N=", Nlang$N)
  Nlang <- merge(Nlang, aux[, c(Grvar1, Grvar2, "Andel", "konf_lav", "konf_hoy")], by = c(Grvar1, Grvar2))
  Nlang$pst_txt <- paste0(sprintf("%.1f", Nlang$Andel), " %")
  Nlang <- Nlang %>% arrange(-pos)
  text(x = 0, y = Nlang$pos, labels = Nlang$pst_txt, pos = 4)
  mtext(Nlang$N , side=4, line=1, las=1,
        at=Nlang$pos, col=1, cex=0.75, adj = 1, xpd = T)
  mtext(expression(bold("N")), side=4, line=1, las=1, at=Nlang$pos[1]+1,
        col=1, cex=0.75, adj = 1, xpd = T)

  if (inkl_konf){
    arrows(x0 = Nlang$konf_lav, y0 = Nlang$pos, x1 = Nlang$konf_hoy, y1 = Nlang$pos,
           length=0.5/max(Nlang$pos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
    legend('bottom', cex=0.9, bty='n',
           lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
           legend= 'Konfidensintervall')
  }

  title(tittel)
  legend("top",
         legend = levels(RegData[, Grvar2]),
         pch=15, col=farger[1:ngrvar2level], ncol = ngrvar2level,
         bty='n')

  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=(NutvTxt-1):0, outer=TRUE)

  if ( outfile != '') {dev.off()}

}


#' Andeler gruppert på noe annet enn sykehusnavn
#'
#' @export
#'
norgastAndelGruppert1Gr <- function(valgtVar = "Anastomoselekkasje",
                                    tittel = "Testtittel",
                                    Grvar1 = "Robot",
                                    xtxt = "Andel (%)",
                                    ytxt = "",
                                    inkl_konf = TRUE,
                                    outfile='', ...)
{
  NorgastUtvalg <- NorgastUtvalg(...)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt
  NutvTxt <- length(utvalgTxt)

  aux <- RegData %>%
    dplyr::filter(!is.na(!! sym(valgtVar )),
                  !is.na(!! sym(Grvar1 ))) %>%
    dplyr::summarise(N = dplyr::n(),
                     Antall = sum(!! sym(valgtVar )),
                     Andel = Antall/N*100,
                     konf_lav = binom.test(Antall,N, alternative = 'two.sided')$conf.int[1]*100,
                     konf_hoy = binom.test(Antall,N, alternative = 'two.sided')$conf.int[2]*100,
                     .by = c(!! sym(Grvar1 )))

  plotvektor <- aux$Andel
  grtxt <- paste0(aux[[1]], "\n (N=", aux$N, ")")
  FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                  pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  par('oma'=c(0,1,NutvTxt,0))
  par('mar'=c(5.1, 5.1, 5.1, 2.1))

  if (inkl_konf) {
    xmax <- min(max(aux$konf_hoy, na.rm = T)*1.2, 100)
  } else {xmax <- min(max(PlotAndeler, na.rm = T)*1.2, 100)}

  pos <- barplot(plotvektor, horiz=T, border=NA, col=farger[3],
                 font.main=1,
                 xlim = c(0, xmax),
                 xlab=xtxt, las=1,
                 names.arg=rep('',length(plotvektor)))
  title(main = tittel)
  mtext(grtxt, side=2, line=0.2, las=1, at=pos, col=1, cex=1)
  mtext(ytxt, side=2, line=5, las=0, col=1, cex=1)

  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[2], line=(NutvTxt-1):0, outer=TRUE)

  if (inkl_konf){
    arrows(x0 = aux$konf_lav, y0 = pos, x1 = aux$konf_hoy, y1 = pos,
           length=0.5/max(pos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
    # legend('bottom', cex=0.9, bty='n',
    #        lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
    #        legend= 'Konfidensintervall')
  }

  if ( outfile != '') {dev.off()}

}


#' Fordeling av operasjoner over valgt variabel
#' og splittet på grupperingsvariabel
#'
#' @export
#'
norgastFordelingOpGruppert <- function(outfile = "",
                                       Grvar1 = "AvstandAnalVerge_fakt",
                                       Grvar2 = "Tilgang_utvidet",
                                       tittel = c("Fordeling av operasjoner",
                                                  "over avstand fra analkanten"),
                                       ytxt = "Andel (%)",
                                       xtxt = "Avstand analkanten (cm)",
                                       ...) {

  NorgastUtvalg <- NorgastUtvalg(...)

  aux <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt
  NutvTxt <- length(utvalgTxt)

  antall <- aux %>%
    dplyr::filter(!is.na(!! sym(Grvar1 )),
                  !is.na(!! sym(Grvar2 ))) %>%
    dplyr::summarise(N = dplyr::n(),
                     .by = c(!! sym(Grvar1 ), !! sym(Grvar2 ))) %>%
    pivot_wider(names_from = Grvar2, values_from = N) %>%
    arrange(!! sym(Grvar1 ))

  legendTxt <- paste0(names(colSums(antall[,-1])),
                      paste0(" (N = ", colSums(antall[,-1]), ")"))

  andeler <- antall
  andeler[,-1] <- t(t(antall[,-1])/colSums(antall[,-1]))*100

  tmp <- andeler[,2:dim(andeler)[2]] %>% as.matrix() %>% t()
  colnames(tmp) <- andeler[[1]]
  plotdata <- tmp

  FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                  pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger %>% rev()
  par('oma'=c(0,1,NutvTxt,0))

  xpos <- barplot( plotdata, beside=T, las=1, main = tittel,
                   ylim = c(0,max(plotdata)*1.2),
                   horiz=F,  space=c(0,0.3),names.arg=rep('',dim(plotdata)[2]),
                   col=farger[1:2], border=NA, ylab = ytxt, xlab = xtxt)
  mtext(colnames(plotdata), at = colMeans(xpos), side = 1, line = 0)
  legend("topright", legend = legendTxt, pch=15, col = farger[1:2], bty='n')

  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=(NutvTxt-1):0, outer=TRUE)

  if ( outfile != '') {dev.off()}

}










#' Adhocfunksjon for årsrapportfigur
#'
#' @export
#'
norgastAndelGruppert2Gr_ggplot <- function(valgtVar = "Malign",
                                           tittel = "Testtittel",
                                           Grvar1 = "Op_gr",
                                           Grvar2 = "Tilgang_utvidet",
                                           xtxt = "Andel (%)",
                                           ytxt = "",
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
                     konf_lav = binom.test(Antall,N, alternative = 'two.sided')$conf.int[1]*100,
                     konf_hoy = binom.test(Antall,N, alternative = 'two.sided')$conf.int[2]*100,
                     .by = c(!! sym(Grvar1 ), !! sym(Grvar2 )))

  p <- ggplot(aux, aes(fill=!! sym(Grvar2 ), x=Andel, y=!! sym(Grvar1 ))) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_text(stat = "identity", aes())
  geom_errorbarh( aes(y=!! sym(Grvar1 ), xmin=konf_lav, xmax=konf_hoy),
                  position=position_dodge(.9), height=0.2, size=0.5, alpha=0.5) +
    scale_fill_brewer(palette = "Blues") +
    labs(title = tittel) +
    # labs(tag = paste0(utvalgTxt, collapse = "\n"), ) +
    # theme(tag)
    xlab(xtxt) +
    ylab(ytxt) +
    annotate("text", x=0, y=.5, label = "noe tekst her")+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.title = element_text(size = 18))


  # +
  #   annotate("text", x=0, y=14:16, label = utvalgTxt)
  #

  # Andeler <- aux %>% dplyr::select(-N, -Antall) %>%
  #   tidyr::pivot_wider(names_from = !! sym(Grvar2 ),
  #                      values_from = Andel,
  #                      names_sort = TRUE) %>%
  #   dplyr::arrange(desc(!! sym(Grvar1 )))
  #
  # N <- aux %>% dplyr::select(-Andel, -Antall) %>%
  #   tidyr::pivot_wider(names_from = !! sym(Grvar2 ),
  #                      values_from = N,
  #                      names_sort = TRUE) %>%
  #   dplyr::arrange(desc(!! sym(Grvar1 )))
  #
  # PlotAndeler <- as.data.frame(Andeler)
  # rownames(PlotAndeler) <- PlotAndeler[, Grvar1]
  # PlotAndeler<- as.matrix(PlotAndeler[,-1]) %>% t()
  #
  # ngrvar2level <- dim(Andeler)[2]-1
  # ymax <- 1.5 + (nlevels(RegData[[Grvar2]])+1) * nlevels(RegData[[Grvar1]])
  #
  # FigTypUt <- rapFigurer::figtype(outfile=outfile,
  #                                 pointsizePDF=11, fargepalett='BlaaOff')
  # farger <- rev(FigTypUt$farger)
  # par('oma'=c(0,1,NutvTxt,0))
  # par('mar'=c(5.1, 5.1, 5.1, 3.1))
  #
  # pos <- barplot(PlotAndeler, horiz=T, beside=TRUE, border=NA, col=farger[1:ngrvar2level],
  #                main='', font.main=1, ylim = c(0,ymax),
  #                xlim = c(0, min(max(PlotAndeler, na.rm = T)*1.2, 100)),
  #                xlab=xtxt, las=1, cex.names = 1.2,
  #                names.arg=rep('',dim(PlotAndeler)[2]))
  #
  # mtext(colnames(PlotAndeler), side=2, line=0.2, las=1, at=colMeans(pos), col=1, cex=1)
  # mtext(ytxt, side=2, line=5, las=0, col=1, cex=1)
  #
  #
  # Nlang <- N %>% pivot_longer(cols = -1,
  #                             names_to = Grvar2, values_to = "N")
  # Nlang$pos <- as.vector(pos)
  # # Nlang$N <- paste0("N=", Nlang$N)
  # Nlang <- merge(Nlang, aux[, c(Grvar1, Grvar2, "Andel")], by = c(Grvar1, Grvar2))
  # Nlang$pst_txt <- paste0(sprintf("%.1f", Nlang$Andel), " %")
  # Nlang <- Nlang %>% arrange(-pos)
  # text(x = 0, y = Nlang$pos, labels = Nlang$pst_txt, pos = 4)
  # mtext(Nlang$N , side=4, line=1, las=1,
  #       at=Nlang$pos, col=1, cex=0.75, adj = 1, xpd = T)
  # mtext(expression(bold("N")), side=4, line=1, las=1, at=Nlang$pos[1]+1,
  #       col=1, cex=0.75, adj = 1, xpd = T)
  #
  # title(tittel)
  # legend("top",
  #        legend = levels(RegData[, Grvar2]),
  #        pch=15, col=farger[1:ngrvar2level], ncol = ngrvar2level,
  #        bty='n')
  #
  # mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=(NutvTxt-1):0, outer=TRUE)
  #
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

