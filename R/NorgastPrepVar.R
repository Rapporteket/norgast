#' Preparer variabler for plotting
#'
#' Denne funksjonen grupperer og klargjør variabler for andelsplot
#'
#' Her kan detaljer skrives
#'
#' @inheritParams FigAndeler
#'
#' @return PrepData En liste med plotrelevante størrelser
#'
#' @export
#'
NorgastPrepVar <- function(RegData, valgtVar, enhetsUtvalg=1)
{
  stabel=FALSE; incl_N=FALSE; incl_pst=FALSE; retn= 'V'; tittel <- ''; inkl_konf=0;
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- ''; VarTxt <- '';


  RegData$Variabel <- NA
  if (valgtVar %in% c('Alder', 'Vektendring', 'MedDiabetes','WHOECOG', 'ASA', 'ModGlasgowScore', 'Forbehandling',
                      'BMI_kodet', 'Op_gr', 'Hastegrad', 'Tilgang', 'ThoraxTilgang', 'AccordionGrad', 'ReLapNarkose',
                      'AvlastendeStomiRektum', 'PermanentStomiColorektal', 'RegMnd', 'Robotassistanse', 'erMann', 'PRSScore',
                      'NyAnastomose','Anastomoselekkasje', 'Avdod', 'OpDoedTid', 'LapTilgang', 'KumAcc')) {
    RegData$Variabel <- RegData[ ,valgtVar]
  }

  if (valgtVar=='Avdod') {
    tittel <- c('Andel avdøde uansett årsak', '(Egenregistrerte og fra folkeregister)')
    VarTxt <- 'avdøde'
    grtxt <- c('I live', 'Avdød')
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='KumAcc') {
    tittel <- c('Accordion score \u2265 3')
    VarTxt <- 'med accordion score \u2265 3'
    grtxt <- c('Nei', 'Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }


  if (valgtVar=='OpDoedTid') {
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('Tid fra operasjon til død', '(Fordeling av de med registrert dødsdato)')
    RegData$Variabel <- as.numeric(RegData$Variabel)
    gr <- c(0, 10, 20, 30, 40, 100, 10000)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- levels(RegData$VariabelGr)
    grtxt[length(grtxt)] <- paste0('\u2265', as.character(gr[length(gr)-1])) # Større eller lik unicode symbol
    subtxt <- 'Tid i dager'
  }

  if (valgtVar=='Vektendring') {  # NA fikset
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Fra premorbid til preoperativ vektendring'
    RegData$Variabel <- as.numeric(RegData$Variabel)
    gr <- c(-100, -10, -5, -2, 2, 5, 10, 200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('<-10','[-10,-5)', '[-5,-2)', '[-2,2)', '[2,5)', '[5,10)','>=10')
    subtxt <- 'Vektendring %'
  }

  if (valgtVar=='Op_gr') {
    tittel <- 'Operasjonsgrupper'
    gr <- c(1:11,99)
    grtxt <- c('Kolonreseksjoner','Rektumreseksjoner','Øsofagusreseksjoner','Ventrikkelreseksjoner',
               'Leverreseksjoner',"Whipples operasjon", 'Cholecystektomi', 'Appendektomi', 'Tynntarmsreseksjon',
               'Gastric bypass', 'Gastric sleeve', 'Annet')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    subtxt <- 'Operasjonsgrupper'
    incl_N <- T
    retn <- 'H'
  }

  if (valgtVar=='Alder') {
    tittel <- 'Aldersfordeling'
    gr <- c(0, seq(45, 85, 10), 120)  #c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('<45','45-54','55-64','65-74','75-84','85+')
    subtxt <- 'Aldersgrupper'
  }


  if (valgtVar=='PRSScore') {
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'mE-PASS'
    gr <- seq(0, 2, .4)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- levels(RegData$VariabelGr)
    subtxt <- 'PRS-score'
  }


  if (valgtVar=='WHOECOG') {
    tittel <- 'WHO-ECOG FUNKSJONSSCORE'
    grtxt <- c('0: Fullt aktiv', '1: Lett husarbeid og sittende arbeid', '2: Oppe > 50% av dagen, selvstelt',
               '3: Oppe < 50% av dagen, delvis selvstelt', '4: Kun i stol/seng, hjelp til alt stell', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% c(0:4,9)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0:4,9), labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='BMI_kodet') {
    tittel <- 'BMI'
    subtxt <- expression(BMI (kg/m^2))
    grtxt <- c('Alvorlig undervekt (<16)','Undervekt (16-17)','Mild undervekt (17-17.5)','Normal (18.5-25)','Overvekt (25-30)',
               'Moderat fedme, klasse I (30-35)','Fedme, klasse II (35-40)','Fedme, klasse III (40-50)')
    RegData <- RegData[which(RegData$Variabel %in% c(1:8)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:8), labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar=='ASA') {
    tittel <- 'ASA-score'
    subtxt <- 'ASA-score gruppe'
    grtxt <- c('1', '2', '3', '4', '5')
    RegData <- RegData[which(RegData$Variabel %in% c(1:5)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5), labels = grtxt)
  }

  if (valgtVar=='erMann') {
    tittel <- 'Kjønn'
    grtxt <- c('Kvinne', 'Mann')
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='ModGlasgowScore') {
    tittel <- 'Modified Glasgow Prognostic Score'
    grtxt <- c('0', '1', '2')
    # grtxt <- c('0', '1', '2', 'Ukjent')
    # RegData$Variabel[is.na(RegData$Variabel)] <- 99
    RegData <- RegData[which(RegData$Variabel %in% c(0:2)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0:2), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='Forbehandling') {
    tittel <- 'Neoadjuvant behandling siste 3 mnd.'
    grtxt <- c('Cytostatika', 'Stråleterapi', 'Komb. kjemo/radioterapi', 'Ingen')
    RegData <- RegData[which(RegData$Variabel %in% 1:4), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=1:4, labels = grtxt)
    retn <- 'H'
    incl_pst <- T
  }

  if (valgtVar=='Hastegrad') {
    tittel <- 'Elektiv kirurgi'
    grtxt <- c('Ø-hjelp', 'Elektiv')
    VarTxt <- 'med elektiv kirurgi'
    RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=0:1, labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='Tilgang') {
    tittel <- 'Tilgang i abdomen'
    grtxt <- c('Åpen', 'Laparoskopisk', 'Konvertert')
    RegData <- RegData[which(RegData$Variabel %in% 1:3), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=1:3, labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='ThoraxTilgang') {
    tittel <- 'Tilgang i thorax v/ øsofaguskirurgi'
    grtxt <- c('Thoracotomi', 'Thorakoskopi', 'Ingen (transhiatal)')
    RegData <- RegData[which(RegData$Variabel %in% 4:6), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=4:6, labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='AccordionGrad') {
    tittel <- 'Komplikasjoner'
    grtxt <- c('<3', '3', '4', '5', '6')
    subtxt <- 'Accordion score'
    RegData <- RegData[which(RegData$Variabel %in% c(1, 3:6)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1, 3:6), labels = grtxt)
  }

#   if (valgtVar=='KumAcc') {
#     tittel <- 'Accordion score 3-6'
#     VarTxt <- 'med accordion score 3-6'
# #     grtxt <- c('<3', '3', '4', '5', '6')
#     grtxt <- c('Nei','Ja')
#     RegData$Variabel <- RegData$AccordionGrad
#     RegData$Variabel[RegData$Variabel == 1] <- 0
#     RegData$Variabel[RegData$Variabel %in% 3:6] <- 1
#     RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
#     RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
#     if (enhetsUtvalg==1) {stabel=T}
#   }

  if (valgtVar=='MedDiabetes') {
    tittel <- 'Medisinert mot diabetes'
    VarTxt <- 'med diabetes'
    # grtxt <- c('Nei','Ja', 'Ikke registrert')
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    # RegData$Variabel[is.na(RegData$Variabel)] <- 99
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='LapTilgang') {
    tittel <- 'Laparoskopisk tilgang'
    # grtxt <- c('Nei','Ja', 'Ikke registrert')
    VarTxt <- 'laparoskopi mot åpen/konvertert'
    grtxt <- c('Åpen/konvertert','Laparoskopisk')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    # RegData$Variabel[is.na(RegData$Variabel)] <- 99
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='Anastomoselekkasje') {
    tittel <- 'Anastomoselekkasje, ny anastomose'
    VarTxt <- 'anastomoselekkasjer'
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='Robotassistanse') {
    tittel <- 'Robotassistert laparoskopi'
    VarTxt <- 'robotassistert laparoskopi'
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Tilgang %in% c(2,3)), ]
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='ReLapNarkose') {
    tittel <- 'Relaparotomi/-laparoskopi'
    VarTxt <- 'relaparotomier/-laparoskopier'
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
    inkl_konf <- 1
  }

  if (valgtVar=='NyAnastomose') {
    tittel <- 'Ny anastomose'
    VarTxt <- 'nye anastomoser'
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
    inkl_konf <- 0
  }

  if (valgtVar=='AvlastendeStomiRektum') {
    tittel <- 'Stomi ved rektumreseksjon med ny anastomose'
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='PermanentStomiColorektal') {
    tittel <- 'Permanent stomi'
    grtxt <- c('Nei','Ja')
    RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
    if (enhetsUtvalg==1) {stabel=T}
  }


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, stabel=stabel, subtxt=subtxt,
                   incl_N=incl_N, incl_pst=incl_pst, retn=retn, cexgr=cexgr, VarTxt=VarTxt, inkl_konf=inkl_konf)

  return(invisible(PlotParams))
}
