#' Funksjon som definerer brukervalgene til bruk i nedtrekksmenyer i Shiny app
#'
#' @return BrValg En liste bestående av brukervalgene
#'
#' @export

BrValgNorgastShiny <- function(RegData) {

  varvalg <- c('Alder', 'BMI_kodet', 'Vektendring', 'Op_gr', 'AccordionGrad', 'Forbehandling',
               'WHOECOG', 'ASA', 'Hastegrad_tid', 'Hastegrad', 'erMann', 'MedDiabetes', 'PRSScore', 'Robotassistanse',
               'Tilgang', 'NyAnastomose', 'ModGlasgowScore', 'ReLapNarkose', 'Anastomoselekkasje',
               'mortalitet90', 'Saarruptur', 'laerebok')
  names(varvalg) <- c('Alder', 'BMI', 'Vektendring', 'Operasjonsgrupper', 'Komplikasjoner', 'Forbehandling',
                      'WHO-ECOG', 'ASA-grad', 'Tidspunkt for operasjonsstart', 'Hastegrad',  'Kjønn', 'Diabetes', 'mE-PASS', 'Robotassistanse',
                      'Tilgang i abdomen', 'Ny anastomose', 'Glasgow score', 'Relaparotomi', 'Anastomoselekkasje',
                      '90-dagers mortalitet', 'Sårruptur', 'Lærebokforløp')

  aux<-c('Anastomoselekkasje', 'Anastomoselekkasje', '90-dagers mortalitet', 'mortalitet90',
         'Relaparotomi', 'ReLapNarkose', 'Robotassistanse', 'Robotassistanse', 'Sårruptur',
         'Saarruptur', 'Laparoskopisk operert', 'LapTilgang', 'Tidspunkt for operasjonsstart', 'Hastegrad_tid',
         'Hastegrad', 'Hastegrad', 'Ny anastomose', 'NyAnastomose',
         'Kummulativ accordion score', 'KumAcc', 'Andel maligne', 'Malign',
         'Lærebokforløp', 'laerebok')
  varvalg_andel <- aux[seq(2,length(aux), by = 2)]
  names(varvalg_andel) <- aux[seq(1,length(aux), by = 2)]

  varvalg_andel_stabel <- setNames(c('ModGlasgowScore', 'AccordionGrad', 'Tilgang'),
                                   c('Modified Glasgow Score', 'Komplikasjoner', 'Tilgang i abdomen'))
  varvalg_gjsn <- setNames(c('BMI', 'VekttapProsent', 'ModGlasgowScore', 'Alder', 'PRSScore'),
                           c('BMI', 'Vekttap i prosent', 'Modified Glasgow Score', 'Alder', 'mE-PASS'))


  reseksjonsgrupper <- sort(unique(RegData$Op_gr))
  names(reseksjonsgrupper) <- RegData$Operasjonsgrupper[match(reseksjonsgrupper, RegData$Op_gr)]

  sykehus <- RegData$AvdRESH[match(sort(unique(RegData$Sykehusnavn)), RegData$Sykehusnavn)]
  names(sykehus) <- sort(unique(RegData$Sykehusnavn))

  bmi_valg <- 1:8
  names(bmi_valg) <- levels(RegData$BMI_kategori)
  tilgang_valg <- c(1,2,3)
  names(tilgang_valg) <- c('Åpen', 'Laparoskopisk', 'Konvertert')
  ASA_valg <- 1:5
  names(ASA_valg) <- c('Grad I', 'Grad II', 'Grad III', 'Grad IV', 'Grad V')
  whoEcog_valg <- c(0:4, 9)
  names(whoEcog_valg) <- c('0: Fullt aktiv', '1: Lett husarbeid og sittende arbeid', '2: Oppe > 50% av dagen, selvstelt',
                           '3: Oppe < 50% av dagen, delvis selvstelt', '4: Kun i stol/seng, hjelp til alt stell', 'Ukjent')



  Valg <- list(varvalg=varvalg, varvalg_andel=varvalg_andel, varvalg_andel_stabel=varvalg_andel_stabel, varvalg_gjsn=varvalg_gjsn,
               reseksjonsgrupper=reseksjonsgrupper, sykehus=sykehus, bmi_valg=bmi_valg, tilgang_valg=tilgang_valg,
               ASA_valg=ASA_valg, whoEcog_valg=whoEcog_valg)


}



