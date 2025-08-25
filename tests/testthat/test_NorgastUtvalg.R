
# Create a mock dataset for testing
mock_data <- data.frame(
    Alder = c(25, 40, 65, 80),
    OperasjonsDato = as.Date(c("2015-01-01", "2016-06-15", "2018-09-30", "2020-12-31")),
    erMann = c(1, 0, 1, 0),
    Op_gr = c(1, 2, 3, 4),
    Hastegrad_tid = c(1, 0, 1, 0),
    BMI_kodet = c(1, 2, 3, 4),
    PRSScore = c(0.5, 1.0, 1.5, 2.0),
    ASA = c(1, 2, 3, 4),
    WHOECOG = c(0, 1, 2, 3),
    ModGlasgowScore = c(1, 2, 3, 4),
    Hovedoperasjon = c("NCSP1", "NCSP2", "NCSP3", "NCSP4"),
    Forbehandling = c(1, 2, 3, 4),
    Malign = c(0, 1, 0, 1),
    Hoveddiagnose2 = c("ICD1", "ICD2", "ICD3", "ICD4"),
    Robotassistanse = c(0, 1, 0, 1),
    OppfStatus = c(1, NA, 1, 0),
    NyStomi = c(0, 1, 0, 1),
    AccordionGrad = c(1, 2, 3, 4),
    NyAnastomose = c(0, 1, 0, 1)
)

# Define test cases
test_that("NorgastUtvalg filters data correctly by age", {
    result <- NorgastUtvalg(RegData = mock_data, minald = 30, maxald = 70)
    expect_true(all(result$RegData$Alder >= 30 & result$RegData$Alder <= 70))
})

test_that("NorgastUtvalg filters data correctly by gender", {
    result <- NorgastUtvalg(RegData = mock_data, erMann = 1)
    expect_true(all(result$RegData$erMann == 1))
})

test_that("NorgastUtvalg filters data correctly by operation date", {
    result <- NorgastUtvalg(RegData = mock_data, datoFra = "2016-01-01", datoTil = "2019-01-01")
    expect_true(all(result$RegData$OperasjonsDato >= as.Date("2016-01-01") &
                                    result$RegData$OperasjonsDato <= as.Date("2019-01-01")))
})

test_that("NorgastUtvalg returns correct PRSScore range", {
    result <- NorgastUtvalg(RegData = mock_data, minPRS = 0.5, maxPRS = 1.5)
    expect_true(all(result$RegData$PRSScore >= 0.5 & result$RegData$PRSScore <= 1.5))
})

test_that("NorgastUtvalg handles empty filters correctly", {
    result <- NorgastUtvalg(RegData = mock_data)
    expect_equal(nrow(result$RegData), nrow(mock_data) - 1)
    result <- NorgastUtvalg(RegData = mock_data, kun_ferdigstilte = FALSE)
    expect_equal(nrow(result$RegData), nrow(mock_data))
})
test_that("NorgastUtvalg filters data correctly by BMI category", {
    result <- NorgastUtvalg(RegData = mock_data, BMI = c(1, 2))
    expect_true(all(result$RegData$BMI_kodet %in% c(1, 2)))
})

test_that("NorgastUtvalg filters data correctly by ASA grade", {
    result <- NorgastUtvalg(RegData = mock_data, ASA = c(1, 3))
    expect_true(all(result$RegData$ASA %in% c(1, 3)))
})

test_that("NorgastUtvalg filters data correctly by WHO ECOG score", {
    result <- NorgastUtvalg(RegData = mock_data, whoEcog = c(0, 2))
    expect_true(all(result$RegData$WHOECOG %in% c(0, 2)))
})

test_that("NorgastUtvalg filters data correctly by malignancy", {
    result <- NorgastUtvalg(RegData = mock_data, malign = 1)
    expect_true(all(result$RegData$Malign == 1))
})

test_that("NorgastUtvalg filters data correctly by robot assistance", {
    result <- NorgastUtvalg(RegData = mock_data, robotassiastanse = 1)
    expect_true(all(result$RegData$Robotassistanse == 1))
})

test_that("NorgastUtvalg filters data correctly by new stoma", {
    result <- NorgastUtvalg(RegData = mock_data, ny_stomi = 1)
    expect_true(all(result$RegData$NyStomi == 1))
})

test_that("NorgastUtvalg filters data correctly by Accordion grade", {
    result <- NorgastUtvalg(RegData = mock_data, accordion = c(2, 3))
    expect_true(all(result$RegData$AccordionGrad %in% c(2, 3)))
})

test_that("NorgastUtvalg filters data correctly by new anastomosis", {
    result <- NorgastUtvalg(RegData = mock_data, ny_anastomose = 1)
    expect_true(all(result$RegData$NyAnastomose == 1))
})

test_that("NorgastUtvalg handles multiple filters correctly", {
    result <- NorgastUtvalg(RegData = mock_data, minald = 30, maxald = 70, erMann = 1, malign = 0)
    expect_true(all(result$RegData$Alder >= 30 & result$RegData$Alder <= 70))
    expect_true(all(result$RegData$erMann == 1))
    expect_true(all(result$RegData$Malign == 0))
})
