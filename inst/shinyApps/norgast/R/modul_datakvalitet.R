





enfunksjon <- function() { # for å forhindre feilmelding ved autoload fra R-folder
  flere_sammedato_v2 <- RegData %>% group_by(PasientID, HovedDato) %>% summarise(Op_pr_dag = n())
  flere_sammedato_v2 <- flere_sammedato_v2[flere_sammedato_v2$Op_pr_dag > 1, ]

  flere_sammedato_v3 <- merge(flere_sammedato_v2, RegData, by = c('PasientID', 'HovedDato'), all.x = T)
  flere_sammedato_v3 <- flere_sammedato_v3[order(flere_sammedato_v3$PasientID), ]
  flere_sammedato_v3 <- flere_sammedato_v3[ , c("PasientID", "ForlopsID", "OperasjonsDato", "AvdRESH", "Sykehusnavn","Hovedoperasjon", "Operasjonsgrupper", "Hoveddiagnose")]
}


