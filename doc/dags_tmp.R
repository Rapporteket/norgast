

dag {
  bb="-3.116,-3.501,3.249,3.191"
  ASA [adjusted,pos="-1.203,-2.322"]
  Alder [adjusted,pos="0.840,0.280"]
  Avlastende_stomi [adjusted,pos="-0.019,-1.312"]
  BMI [adjusted,pos="1.739,0.682"]
  Kjonn [adjusted,pos="-0.712,0.768"]
  Robot [exposure,pos="-2.749,-0.150"]
  anastomoselekk [outcome,pos="2.158,-0.774"]
  avstand_analkant [adjusted,pos="0.297,-2.453"]
  straaling_preop. [adjusted,pos="-2.247,0.828"]
  ASA -> Avlastende_stomi
  ASA -> Robot
  ASA -> anastomoselekk
  Alder -> BMI
  Alder -> Robot
  Alder -> anastomoselekk
  Avlastende_stomi -> anastomoselekk
  BMI -> Robot
  BMI -> anastomoselekk
  Kjonn -> Avlastende_stomi
  Kjonn -> BMI
  Kjonn -> Robot
  Kjonn -> anastomoselekk
  Robot -> Avlastende_stomi
  Robot -> anastomoselekk
  avstand_analkant -> Robot
  avstand_analkant -> anastomoselekk
  straaling_preop. -> Avlastende_stomi
  straaling_preop. -> Robot
  straaling_preop. -> anastomoselekk
}


