############################################################
# Installation av Keras 3 och TensorFlow för R
#
# Kurs: 732G57
#
# Kör skriptet rad för rad i RStudio.
#
# Installationen kan ta flera minuter första gången.
#
# OBS!
# Om install_keras() redan har körts tidigare behöver
# du normalt inte köra den igen.
#
# Vid problem:
#
# install_keras()
#
# kan oftast köras på nytt för att reparera installationen.
############################################################


############################################################
# Steg 1: Installera keras3
#
# Behöver normalt bara göras en gång.
############################################################

install.packages("keras3")


############################################################
# Steg 2: Ladda paketet
############################################################

library(keras3)


############################################################
# Steg 3: Installera Keras, TensorFlow, Python och
# övriga beroenden
#
# Detta kan ta flera minuter första gången.
#
# När installationen är klar kan RStudio startas om
# automatiskt. Om det inte sker:
#
# Välj Session -> Restart R i RStudio.
############################################################

install_keras(backend = "tensorflow")


############################################################
# Steg 4: Kontrollera att Keras fungerar
#
# Kör dessa rader efter att RStudio har startats om.
############################################################

library(keras3)

keras_version()


############################################################
# Steg 5: Skapa ett enkelt neuralt nätverk
#
# Om information om modellen skrivs ut utan ett
# felmeddelande fungerar installationen.
############################################################

model <- keras_model_sequential(
  input_shape = c(5)
) |>
  layer_dense(
    units = 10,
    activation = "relu"
  ) |>
  layer_dense(
    units = 1,
    activation = "sigmoid"
  )

summary(model)


############################################################
# Steg 6: Valfri extra kontroll
#
# Visar information om den Python-installation som
# används av R och Keras.
############################################################

library(reticulate)

py_config()


############################################################
# Felsökning
#
# Använd bara instruktionerna nedan om installationen
# eller testet ovan ger ett felmeddelande.
############################################################


############################################################
# Felsökningssteg 1: Starta om och försök igen
#
# Detta gäller både Windows och Mac.
#
# 1. Kontrollera att datorn är ansluten till internet.
#
# 2. Starta om R genom att välja:
#
#    Session -> Restart R
#
# 3. Kör därefter följande kod:
############################################################

library(keras3)

install_keras(backend = "tensorflow")


############################################################
# Felsökningssteg 2: Kontrollera datorn och programmen
#
# Kontrollera följande:
#
# - Att den senaste versionen av R är installerad.
#
# - Att den senaste versionen av RStudio är installerad.
#
# - Att det finns tillräckligt med ledigt diskutrymme.
#
# - Att antivirusprogram eller brandvägg inte blockerar
#   installationen.
#
# Windows:
#
# Om felmeddelandet innehåller "Permission denied" eller
# "Access is denied", stäng RStudio. Högerklicka sedan på
# RStudio och välj "Kör som administratör". Försök därefter
# med installationen igen.
#
# Mac:
#
# Om macOS visar en säkerhetsfråga, tillåt RStudio eller
# Python att köra. På en Mac med Apple Silicon, exempelvis
# M1, M2, M3, M4 eller M5, ska den vanliga installationen
# ovan användas. Installera inte TensorFlow separat via
# terminalen.
#
# Visa den installerade R-versionen:
############################################################

R.version.string


############################################################
# Felsökningssteg 3: Felsök med Copilot
#
# Kopiera hela felmeddelandet från konsolen och klistra in
# det i Copilot.
#
# Beskriv också följande:
#
# - Att du arbetar i RStudio.
#
# - Att du försöker installera keras3 och TensorFlow.
#
# - Om du använder Windows eller Mac.
#
# - Vilket av stegen ovan som gav fel.
#
# Du kan exempelvis skriva:
#
# "Jag försöker installera keras3 och TensorFlow i RStudio
# på en Windows- eller Mac-dator. Följande fel uppstår när
# jag kör install_keras(backend = 'tensorflow'):
#
# [Klistra in hela felmeddelandet här]
#
# Förklara felet och hjälp mig att felsöka det stegvis,
# med endast ett eller två steg åt gången."
############################################################


############################################################
# Felsökningssteg 4: Kontakta läraren
#
# Om problemet inte går att lösa med Copilot:
#
# 1. Kör kommandona nedan.
#
# 2. Kopiera hela utskriften.
#
# 3. Kopiera även hela felmeddelandet från konsolen.
#
# 4. Skicka informationen till läraren.
#
# Beskriv även:
#
# - Om du använder Windows eller Mac.
#
# - Vilket steg i installationen som gav fel.
#
# - Vad du redan har provat.
#
# Skicka inte bara en skärmbild med texten
# "det fungerar inte".
############################################################

sessionInfo()

library(reticulate)

py_config()