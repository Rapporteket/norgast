-- This file contains views for fairly complex to complex queries or queries that return interesting stuff (more than one report)

DROP VIEW IF EXISTS user_centre_list;
DROP VIEW IF EXISTS AlleVarNum; 
DROP VIEW IF EXISTS SkjemaOversikt;
DROP VIEW IF EXISTS ForlopsOversikt;
DROP VIEW IF EXISTS BrukerListe;
DROP VIEW IF EXISTS tablelist_restricted;

CREATE VIEW SkjemaOversikt AS
SELECT
    CAST((SELECT t.text FROM text t WHERE t.ID = (
        SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'READMISSION'
    )
                                      AND t.LANGUAGEID='no') AS CHAR(20)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
          skjema.CREATEDBY AS OpprettetAv,
          skjema.TSCREATED AS OpprettetDato,
          skjema.UPDATEDBY AS SistLagretAv,
          skjema.TSUPDATED AS SistLagretDato,
    getFriendlyName(c.ID) AS Sykehusnavn,
    (SELECT registration.OPERATION_DATE FROM registration WHERE registration.MCEID = skjema.MCEID) AS HovedDato,
          c.ID AS AvdRESH,
    CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'READMISSION'
    ) AS CHAR(2)) AS SkjemaRekkeflg
FROM
    readmission skjema,
    centre c
WHERE skjema.CENTREID = c.ID

UNION

SELECT
    CAST((SELECT t.text FROM text t WHERE t.ID = (
        SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'REGISTRATION'
    )
                                      AND t.LANGUAGEID='no') AS CHAR(20)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    getFriendlyName(c.ID) AS Sykehusnavn,
    (SELECT registration.OPERATION_DATE FROM registration WHERE registration.MCEID = skjema.MCEID) AS HovedDato,
    c.ID AS AvdRESH,
    CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'REGISTRATION'
    ) AS CHAR(2)) AS SkjemaRekkeflg
FROM
    registration skjema,
    centre c
WHERE skjema.CENTREID = c.ID
;   -- Konsesjonsdato


-- Genererated as version 1.0
CREATE VIEW ForlopsOversikt AS
SELECT
-- Hospital/centre stuff
m.CENTREID AS AvdRESH,
getFriendlyName(m.CENTREID) AS SykehusNavn,
-- Patient stuff
CAST(p.ID AS CHAR(10)) AS PasientID,
-- NEXT 6 left empty for now
CAST(NULL AS CHAR(4)) AS PostNr,
CAST(NULL AS CHAR(50)) AS PostSted,
CAST(NULL AS CHAR(50)) AS Kommune,
CAST(NULL AS CHAR(4)) AS Kommunenr,
CAST(NULL AS CHAR(50)) AS Fylke,
CAST(NULL AS CHAR(2)) AS Fylkenr,
p.SSN AS KryptertFnr,
CASE
    WHEN IFNULL(p.GENDER,0) = 0 THEN 'Ikke angitt'
    WHEN p.GENDER = 1 THEN 'Mann'
    WHEN p.GENDER = 2 THEN 'Kvinne'
    WHEN p.GENDER = 9 THEN 'Ikke relevant'
    ELSE 'Ukjent'
    END AS PasientKjonn,
CASE
    WHEN p.GENDER = 1 THEN '1'
    WHEN p.GENDER = 2 THEN '0'
    ELSE NULL
    END AS erMann,
datediff(registration.OPERATION_DATE, p.BIRTH_DATE) / 365.25 AS PasientAlder,
p.BIRTH_DATE AS Fodselsdato,
CAST(NULL AS CHAR(10)) AS Norsktalende,
CAST(NULL AS CHAR(30)) AS Sivilstatus,
CAST(NULL AS CHAR(50)) AS Utdanning,
getListText('PATIENT_DECEASED',p.DECEASED) AS Avdod,
p.DECEASED_DATE AS AvdodDato,
-- Event stuff
CAST(m.MCEID AS CHAR(10)) AS ForlopsID,
CAST(registration.STATUS AS CHAR(2)) AS BasisRegStatus,
getListText('MCE_MCETYPE', m.MCETYPE) AS ForlopsType1,
m.MCETYPE AS ForlopsType1Num, -- CHECKME
CAST(NULL AS CHAR(25)) AS ForlopsType2,
CAST(NULL AS UNSIGNED) AS ForlopsType2Num,
readmission.MCEID AS KobletForlopsID,
registration.OPERATION_DATE AS HovedDato,

-- Followup stuff
readmission.STATUS AS OppflgRegStatus, -- TODO: check/fix followup stuff
'0' AS ErOppflg,
CASE
    WHEN readmission.STATUS = 1 THEN 'Oppfølging fullført'
    ELSE 'Ingen oppfølging fullført'
    END  AS OppflgStatus,
NULL AS OppflgSekNr
FROM
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
          INNER JOIN registration registration ON m.MCEID = registration.MCEID
          LEFT OUTER JOIN readmission on m.MCEID = readmission.MCEID
WHERE
        registration.OPERATION_DATE > "2014.01.01";   -- Konsesjonsdato
;

create view user_centre_list AS
select 
  u.ID AS USER_ID,
  u.FIRSTNAME, 
  u.LASTNAME, 
  u.STATUS, 
  u.TITLE, 
  u.TSCREATED, 
  u.TSLASTLOGIN, 
  u.PHONE1, 
  u.PHONE2, 
  u.EMAIL,
  ug.ID as USERGROUP_ID,
  ug.DESCRIPTION AS USERGROUP_NAME, 
  c.ID AS CENTRE_ID,
  c.CENTRENAME, 
  c.CENTRESHORTNAME, 
  ct.NAME AS CENTRE_TYPE
from 
  user u,
  usergroup ug, 
  centre c,
  centretype ct
where 
  u.GROUPID = ug.ID
  and u.CENTREID = c.ID
  and c.TYPEID = ct.ID;
  
CREATE VIEW AlleVarNum AS
SELECT 
	mce.MCEID AS ForlopsID,
	mce.PATIENT_ID AS PasientId,
	mce.CENTREID AS AvdRESH,
	
	-- Patient stuff
	patient.SSN AS Fodselsnummer,
	patient.DECEASED AS Avdod, 
	patient.DECEASED_DATE AS AvdodDato,
	centre.CENTRENAME AS SenterNavn,

	-- Registration stuff
	-- Innleggelse
	registration.PREVIOUS_WEIGHT AS Vekt6MndFoer,
	registration.PREVIOUS_WEIGHT_MISS AS Vekt6MndFoerUkjent,
	registration.ADMISSION_WEIGHT AS VektVedInnleggelse,
	registration.ADMISSION_WEIGHT_MISS AS VektVedInnleggelseUkjent,
	registration.HEIGHT AS Hoyde,
	registration.HEIGHT_MISS AS HoydeUkjent,
	registration.BMI AS BMI,
	registration.BMI_CATEGORY AS BMIKategori,
	registration.WEIGHTLOSS AS VekttapProsent,
	registration.DIABETES AS MedDiabetes,
	registration.CHEMOTHERAPY_ONLY AS KunCytostatika,
	registration.RADIATION_THERAPY_ONLY AS KunStraaleterapi,
	registration.CHEMORADIOTHERAPY AS KjemoRadioKombo,
	registration.WHO_ECOG_SCORE AS WHOECOG,
	registration.ALBUMIN AS Albumin,
	registration.CRP AS CRP,
	registration.GLASGOW_SCORE AS GlasgowScore,
	registration.MODIFIED_GLASGOW_SCORE AS ModGlasgowScore,
	
	-- Anestesi
	registration.ASA AS ASA,
	registration.LUNG_DISEASE AS Lungesykdom,
	registration.HEART_DISEASE AS Hjertesykdom,
	registration.URGENCY AS Hastegrad,
	registration.ANESTHESIA_START AS AnestesiStartKl,
	registration.PRS_SCORE AS PRSScore,

	-- Intervensjonen
	registration.OPERATION_DATE AS OpDato,
	registration.NCSP AS Hovedoperasjon,
    registration.ABLATION AS LeverAblasjon,
	registration.RECONSTRUCTION AS Rekonstruksjon,
	registration.RECONSTRUCTION_TYPE AS Rekonstruksjonstype,
	registration.ANASTOMOSIS_LEVEL AS Anastomoseniva,
	registration.ANASTOMOSIS AS NyAnastomose,
    registration.ANAL_GUARD_DISTANCE AS AvstandAnalVerge,
    registration.ANAL_GUARD_DISTANCE_MISS AS AvstandAnalVergeIkkeAkt,
	registration.TATME AS TaTME,
	registration.OSTOMY AS NyStomi,
	registration.ABDOMINAL_ACCESS AS Tilgang,
	registration.ROBOTASSISTANCE AS Robotassistanse,
	registration.THORAX_ACCESS AS ThoraxTilgang,
	
	-- Komplikasjoner
	registration.RELAPAROTOMY AS ReLapNarkose,
	registration.RELAPAROTOMY_YES AS ViktigsteFunn,
    registration.FINDINGS_SPESIFISER AS FunnSpesifiser,
	registration.RELAPAROTOMY_NO AS AnnenOpIAnestsi,
	registration.INTERVENTION_WITHOUT_ANESTHESIA AS IntUtenAnestesi,
	registration.PERCUTANEOUS_DRAINAGE AS PerkDrenasje,
	registration.HIGH_AMYLASE_CONCENTRATION AS HoyAmylaseKons,
	registration.LEAK_INTERVENTION AS EndoInterLekkasje,
	registration.BLEED_INTERVENTION AS EndoInterBlod,
	registration.ANGIO_INTERVENTION AS AngioInter,
	registration.LIQUID_DRAINAGE AS KunDrenasje,
	registration.SINGLE_ORGAN_FAILURE AS EttOrganSvikt,
	registration.MULTI_ORGAN_FAILURE AS MultiOrganSvikt,
	registration.IN_HOUSE_DEATH AS DodUnderOpphold,
	registration.IN_HOUSE_DEATH_DATE AS DodUnderOppholdDato,
	registration.ACCORDION_SCORE AS AccordionGrad,

	-- Utskrivelse
	registration.DISCHARGE_DATE AS UtskrivelseDato,
	registration.BED_DAYS AS PostopLiggedogn,
	registration.ICD10 AS Hoveddiagnose,
	registration.DISCHARGE_TO AS UtskrevetTil,
	registration.STATUS AS RegistreringStatus,
	
	-- Oppfølging
	-- Reinnleggelse/oppfølging
	readmission.OWN_INSTITUTION AS ReinnlEgenInst,
	readmission.OTHER_INSTITUTIONS AS ReinnlAndreInst,
	readmission.CONTROL AS AktivKontroll,
	readmission.PHYSICAL_CONTROL AS FysiskKontroll,
	readmission.PHONE_CONTROL AS TelefonKontroll,

	-- Komplikasjoner
	readmission.RELAPAROTOMY AS OppfReLapNarkose,
	readmission.RELAPAROTOMY_YES AS OppfViktigsteFunn,
    readmission.FINDINGS_SPESIFISER AS OppfFunnSpesifiser,
	readmission.RELAPAROTOMY_NO AS OppfAnnenOpIAnestsi,
	readmission.INTERVENTION_WITHOUT_ANESTHESIA AS OppfIntUtenAnestesi,
	readmission.PERCUTANEOUS_DRAINAGE AS OppfPerkDrenasje,
	readmission.HIGH_AMYLASE_CONCENTRATION AS OppfHoyAmylaseKons,
	readmission.LEAK_INTERVENTION AS OppfEndoInterLekkasje,
	readmission.BLEED_INTERVENTION AS OppfEndoInterBlod,
	readmission.ANGIO_INTERVENTION AS OppfAngioInter,
	readmission.LIQUID_DRAINAGE AS OppfKunDrenasje,
	readmission.SINGLE_ORGAN_FAILURE AS OppfEttOrganSvikt,
	readmission.MULTI_ORGAN_FAILURE AS OppfMultiOrganSvikt,
	readmission.IN_HOUSE_DEATH AS OppfDodUnderOpphold,
	readmission.IN_HOUSE_DEATH_DATE AS OppfDodUnderOppholdDato,
	readmission.ACCORDION_SCORE AS OppfAccordionGrad,
	getStatusText(readmission.STATUS) AS OppfStatus
	
FROM
	mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID
    INNER JOIN registration on mce.MCEID = registration.MCEID
    INNER JOIN centre on centre.ID=mce.CENTREID
    LEFT OUTER JOIN readmission on mce.MCEID = readmission.MCEID
WHERE
	registration.OPERATION_DATE > "2014.01.01";   -- Konsesjonsdato


DROP VIEW IF EXISTS Brukerliste;
create view Brukerliste AS
select
    u.ID AS BrukerID,
    u.FIRSTNAME AS Fornavn,
    u.LASTNAME AS Etternavn,
    u.TITLE AS Tittel,
    u.TSCREATED AS OpprettetDato,
    u.TSLASTLOGIN AS SistInnlogget,
    u.PHONE1 AS Tlf1,
    u.PHONE2 AS Tlf2,
    u.EMAIL AS Epost,
    ug.ID as Rolle,
    ug.DESCRIPTION AS RolleBeskrivelse,
    c.ID AS AvdRESH,
    c.CENTRENAME AS RESHNavnFullt,
    c.CENTRESHORTNAME SykehusKort,
    NULL AS Enhetstype,
    c.CENTRENAME AS Sykehusnavn
from
    user u,
    usergroup ug,
    centre c
where
        u.GROUPID = ug.ID
  and u.CENTREID = c.ID
  and u.GROUPID not like 'SYSTEMSOWNUSER'
  AND c.ID not like '106006';

-- View for Datadump dropdown view
DROP VIEW IF EXISTS tablelist;
CREATE VIEW tablelist AS
SELECT DISTINCT TABLE_NAME AS TABLE_NAME
FROM information_schema.tables
WHERE TABLE_TYPE IN ('BASE TABLE','VIEW')
  AND TABLE_NAME IN ('ForlopsOversikt','SkjemaOversikt','AlleVarNum', 'mce', 'registration', 'readmission', 'friendlycentre', 'user', 'mce_patient_data')
ORDER BY TABLE_NAME;

DROP VIEW IF EXISTS PasientListe;
CREATE VIEW PasientListe AS
SELECT
    ID AS PasientID,
    NULL AS RegistreringsDato,
    BIRTH_DATE AS Fodselsdato,
    getListText('PATIENT_GENDER', GENDER) AS Kjonn,
    getListText('PATIENT_DECEASED', DECEASED) AS Avdod,
    DECEASED_DATE AS Dodsdato,
    NULL AS KommuneNr,
    NULL AS Kommune,
    NULL AS Fylke
FROM patient;

DROP VIEW IF EXISTS patientlist;
CREATE VIEW patientlist AS
SELECT
    ID,
    NULL as REGISTERED_DATE,
    BIRTH_DATE ,
    GENDER,
    DECEASED,
    DECEASED_DATE,
    NULL as MUNICIPALITY_NUMBER,
    NULL as MUNICIPALITY_NAME,
    NULL as COUNTY
FROM patient;

-- CHARTS
DROP VIEW IF EXISTS chart_skjemaoversikt;

-- Genererated as version 1.0
CREATE VIEW chart_skjemaoversikt AS
SELECT
	CAST((SELECT t.text FROM text t WHERE t.ID = (
		SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'READMISSION'
	)
	AND t.LANGUAGEID='no') AS CHAR(20)) AS skjemanavn,
	skjema.STATUS AS skjemastatus,
	skjema.MCEID AS forlopsid,
	skjema.CREATEDBY AS opprettetav,
	skjema.TSCREATED AS opprettetdato,
	skjema.UPDATEDBY AS sistlagretav,
	skjema.TSUPDATED AS sistlagretdato,
	getFriendlyName(c.ID) AS sykehusnavn,
	(SELECT registration.OPERATION_DATE FROM registration WHERE registration.MCEID = skjema.MCEID) AS hoveddato,
	c.ID AS avdresh,
		(SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'READMISSION'
	) AS skjemarekkeflg
FROM
	readmission skjema,
	centre c
	WHERE skjema.CENTREID = c.ID

UNION

SELECT
	CAST((SELECT t.text FROM text t WHERE t.ID = (
		SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'REGISTRATION'
	)
	AND t.LANGUAGEID='no') AS CHAR(20)) AS skjemanavn,
	skjema.STATUS AS skjemastatus,
	skjema.MCEID AS forlopsid,
	skjema.CREATEDBY AS opprettetav,
	skjema.TSCREATED AS opprettetdato,
	skjema.UPDATEDBY AS sistlagretav,
	skjema.TSUPDATED AS sistlagretdato,
	getFriendlyName(c.ID) AS sykehusnavn,
	(SELECT registration.OPERATION_DATE FROM registration WHERE registration.MCEID = skjema.MCEID) AS hoveddato,
	c.ID AS avdresh,
		(SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'REGISTRATION'
	) AS skjemarekkeflg
FROM
	registration skjema,
	centre c
	WHERE skjema.CENTREID = c.ID
;

DROP VIEW IF EXISTS chart_forlopsoversikt;

-- Genererated as version 1.0
CREATE VIEW chart_forlopsoversikt AS
SELECT
m.CENTREID AS avdresh,
getFriendlyName(m.CENTREID) AS sykehusnavn,
p.ID AS pasientid,
p.SSN AS kryptertfnr,
CASE
	WHEN IFNULL(p.GENDER,0) = 0 THEN 'Ikke angitt'
	WHEN p.GENDER = 1 THEN 'Mann'
	WHEN p.GENDER = 2 THEN 'Kvinne'
	WHEN p.GENDER = 9 THEN 'Ikke relevant'
	ELSE 'Ukjent'
END AS pasientkjonn,
datediff(registration.OPERATION_DATE, p.BIRTH_DATE) / 365.25 AS pasientalder,
p.BIRTH_DATE AS fodselsdato,
NULL AS utdanningssb,
getListText('PATIENT_DECEASED',p.DECEASED) AS avdod,
p.DECEASED_DATE AS avdoddato,
m.MCEID AS forlopsid,
LEAST(registration.STATUS, readmission.STATUS) AS basisregstatus,
CASE
    WHEN m.MCETYPE = 0 THEN 'Forløp'
    ELSE 'Ukjent'
    END AS ForlopsType1,
m.MCETYPE AS forlopstype1num,
registration.OPERATION_DATE AS hoveddato,
m.CREATEDBY AS registreringsholder
FROM
mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
INNER JOIN readmission readmission ON m.MCEID = readmission.MCEID
INNER JOIN registration registration ON m.MCEID = registration.MCEID
;

CREATE VIEW tablelist_restricted AS
SELECT DISTINCT TABLE_NAME AS TABLE_NAME
FROM information_schema.tables
WHERE TABLE_TYPE IN ('BASE TABLE','VIEW')
  AND TABLE_NAME IN ('')
  AND TABLE_SCHEMA ='name_of_registry_db' -- TODO: change to actual value
ORDER BY TABLE_NAME;