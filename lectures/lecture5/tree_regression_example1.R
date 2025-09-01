
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Skatta trädmodeller för trees data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


source("https://raw.githubusercontent.com/STIMALiU/732G57_ML/refs/heads/main/labs/lm_diagnostics.R")
#install.packages("tree")
library(tree)

#-------------------------------------------------------------------------------
# Undersöker trees data
#-------------------------------------------------------------------------------
data("trees")
head(trees)
?tress

library(plotly)

# Använd inbyggda trees-datan
plot(trees[,c(3,2,1)])

# Skapa 3D scatter plot
plot_ly(data = trees,
        x = ~Girth,
        y = ~Height,
        z = ~Volume,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5, color = ~Volume, colorscale = "Viridis", showscale = TRUE)) %>%
  layout(title = "3D Scatter Plot of Trees Dataset",
         scene = list(
           xaxis = list(title = "Girth"),
           yaxis = list(title = "Height"),
           zaxis = list(title = "Volume")
         ))
# Kör ni plotten ovan så öppnas en interaktiv plot i tabben "Viewer" till vänster
# Ni kan interagera med plotten på olika sätt, tex rotera i olika riktningar
# Färgen på punkterna visar värdet på Volume

# rotera och undersök data: vi ser att det verkar finnas ett samband mellan
# både Height och Girth med Volume


#-------------------------------------------------------------------------------
# Linjär regression för trees data
#-------------------------------------------------------------------------------
lm_model<-lm(formula = Volume~.,data = trees)

lm_pred<-predict(lm_model)
summary(lm_model)
plot(trees$Volume,lm_pred)
# Tränings MSE:
mean((lm_pred-trees$Volume)^2)
lm_diagnostics(lm_obj = lm_model)

# hur funkar modellen?

#-------------------------------------------------------------------------------
# skapar en 3D-plot med data + regressionsytan (som ett lutande plan)

# Skapa grid för Girth och Height
girth_seq <- seq(min(trees$Girth), max(trees$Girth), length.out = 30)
height_seq <- seq(min(trees$Height), max(trees$Height), length.out = 30)
grid_X <- expand.grid(Girth = girth_seq, Height = height_seq)

grid_all<-grid_X
# Prediktera Volume från modellen för all x-punkter i grid_X
grid_all$Volume <- predict(lm_model, newdata = grid)

# Omforma till matris för surface plot
volume_matrix <- matrix(grid_all$Volume, nrow = length(girth_seq), 
                        ncol = length(height_seq),byrow = TRUE)

# Skapa 3D scatter plot
scatter <- plot_ly(data = trees,
                   x = ~Girth, y = ~Height, z = ~Volume,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 5, color = ~Volume, colorscale = "Viridis", showscale = TRUE),
                   name = "Observed Data")

# Lägg till regressionsplan
surface <- add_trace(scatter,
                     x = girth_seq, y = height_seq, z = volume_matrix,
                     type = "surface", opacity = 0.5, colorscale = "Reds",
                     name = "Regression Plane")

# Anpassa layout
surface <- layout(surface,
                  title = "3D_trees: Volume as a function of Girth and Height",
                  scene = list(
                    xaxis = list(title = "Girth"),
                    yaxis = list(title = "Height"),
                    zaxis = list(title = "Volume")
                  ))

# Visa plotten
surface


# vi ser att den linjära modellen funkar rätt ok
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Modell A: enkel trädmodell
#-------------------------------------------------------------------------------
library(tree)
# skattar en trädmodell för Volume 
# använder defualt-värden för att stoppa utökningen av trädet
A<-tree(formula = Volume~.,data = trees)

# kollar på det skattade trädet
plot(A)
text(A, pretty = 0)
# bara 4 noder, bara Girth används för att dela upp noderna

A_pred<-predict(A)
plot(trees$Volume,A_pred)
# Tränings MSE:
mean((A_pred-trees$Volume)^2)
# tydligt sämre än den linjära modellen

#-------------------------------------------------------------------------------
# skapar en 3D-plot med data + ytan för anpassade värden för modell A

# Ladda nödvändiga paket
library(plotly)
library(rpart)


# Skapa en 3D scatter plot
scatter <- plot_ly(data = trees,
                   x = ~Girth, y = ~Height, z = ~Volume,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 5, color = ~Volume, colorscale = "Viridis", showscale = TRUE),
                   name = "Observed Data")

# Skapa grid för Girth och Height
girth_seq <- seq(min(trees$Girth), max(trees$Girth), length.out = 30)
height_seq <- seq(min(trees$Height), max(trees$Height), length.out = 30)
grid <- expand.grid(Girth = girth_seq, Height = height_seq)

# Prediktera Volume från modellen
grid$Volume <- predict(A, newdata = grid)

# Omforma till matris för surface plot
volume_matrix <- matrix(grid$Volume, nrow = length(girth_seq), 
                        ncol = length(height_seq),byrow = TRUE)

# Lägg till regressionsplan till scatter plot
surface <- add_trace(scatter,
                     x = girth_seq, y = height_seq, z = volume_matrix,
                     type = "surface", opacity = 0.5, colorscale = "Reds",
                     name = "Tree Model Plane")

# Anpassa layout
surface <- layout(surface,
                  title = "3D_trees: Volume as a function of Girth and Height (tree model A)",
                  scene = list(
                    xaxis = list(title = "Girth"),
                    yaxis = list(title = "Height"),
                    zaxis = list(title = "Volume")
                  ))

# Visa plotten
surface
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Modell B: enkel trädmodell
#-------------------------------------------------------------------------------

# ändrar stopp-kriterierna för att få en mer flexibel modell
?tree.control
B<-tree(formula = Volume~.,data = trees,control = tree.control(nobs = 31,minsize = 3))
plot(B)
text(B, pretty = 0)
# 7 noder
B_pred<-predict(B)
plot(trees$Volume,B_pred)
# tränings MSE
mean((B_pred-trees$Volume)^2)
# bättre än den linjära modellen!

#-------------------------------------------------------------------------------
# skapar en 3D-plot med data + ytan för anpassade värden för modell B

# Ladda nödvändiga paket
library(plotly)
library(rpart)


# Skapa en 3D scatter plot
scatter <- plot_ly(data = trees,
                   x = ~Girth, y = ~Height, z = ~Volume,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 5, color = ~Volume, colorscale = "Viridis", showscale = TRUE),
                   name = "Observed Data")

# Skapa grid för Girth och Height
girth_seq <- seq(min(trees$Girth), max(trees$Girth), length.out = 30)
height_seq <- seq(min(trees$Height), max(trees$Height), length.out = 30)
grid <- expand.grid(Girth = girth_seq, Height = height_seq)

# Prediktera Volume från modellen
grid$Volume <- predict(B, newdata = grid)

# Omforma till matris för surface plot
volume_matrix <- matrix(grid$Volume, nrow = length(girth_seq), 
                        ncol = length(height_seq),byrow = TRUE)

# Lägg till regressionsplan till scatter plot
surface <- add_trace(scatter,
                     x = girth_seq, y = height_seq, z = volume_matrix,
                     type = "surface", opacity = 0.5, colorscale = "Reds",
                     name = "Tree Model Plane")

# Anpassa layout
surface <- layout(surface,
                  title = "3D_trees: Volume as a function of Girth and Height (tree model B)",
                  scene = list(
                    xaxis = list(title = "Girth"),
                    yaxis = list(title = "Height"),
                    zaxis = list(title = "Volume")
                  ))

# Visa plotten
surface

# bättre anpassning än modell A och den linjära modellen

#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Modell C: träd från paketet rpart
#-------------------------------------------------------------------------------
# testar med rpart

library(rpart)
library(rpart.plot)

C <- rpart(
  formula = Volume ~ .,
  data = trees,
  ,
  # Anger föroreningsmått
  parms = list(split = "mse"), 
  
  control = list(
    ## Stopkriterier
    # Anger att antalet observationer som krävs för att en förgrening ska ske
    minsplit = 5,
    # Anger maxdjupet av träder, där 0 är rotnoden
    maxdepth = 6, 
    # Anger den minsta tillåtna förbättringen som måste ske för att en förgrening ska ske
    cp = 0,
    # Två inställningar som inte används mer i detalj
    maxcompete = 0,
    maxsurrogate = 0,
    
    ## Trädanpassning
    # Anger antalet korsvalideringar som ska ske medan modellens tränas, intern validering
    xval = 0, 
    # Tillåter att förgreningar har surrogatregler som kan användas vid saknade värden
    # Ska vara 2 om saknade värden finns i datamaterialet
    usesurrogate = 0
  )
)

rpart.plot(C)
y_hat<-predict(C)
# snyggare träd!

mean((y_hat-trees$Volume)^2)
# ungefär lika bra som den linjära modellen

plot(trees$Volume,predict(C))


#-------------------------------------------------------------------------------
# skapar en 3D-plot med data + ytan för anpassade värden för modell C

# Ladda nödvändiga paket
library(plotly)
library(rpart)


# Skapa en 3D scatter plot
scatter <- plot_ly(data = trees,
                   x = ~Girth, y = ~Height, z = ~Volume,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 5, color = ~Volume, colorscale = "Viridis", showscale = TRUE),
                   name = "Observed Data")

# Skapa grid för Girth och Height
girth_seq <- seq(min(trees$Girth), max(trees$Girth), length.out = 30)
height_seq <- seq(min(trees$Height), max(trees$Height), length.out = 30)
grid <- expand.grid(Girth = girth_seq, Height = height_seq)

# Prediktera Volume från modellen
grid$Volume <- predict(C, newdata = grid)

# Omforma till matris för surface plot
volume_matrix <- matrix(grid$Volume, nrow = length(girth_seq), 
                        ncol = length(height_seq),byrow = TRUE)

# Lägg till regressionsplan till scatter plot
surface <- add_trace(scatter,
                     x = girth_seq, y = height_seq, z = volume_matrix,
                     type = "surface", opacity = 0.5, colorscale = "Reds",
                     name = "Tree Model Plane")

# Anpassa layout
surface <- layout(surface,
                  title = "3D_trees: Volume as a function of Girth and Height (rpart Model)",
                  scene = list(
                    xaxis = list(title = "Girth"),
                    yaxis = list(title = "Height"),
                    zaxis = list(title = "Volume")
                  ))

# Visa plotten
surface
# Notera att den anpassade ytan har "trappsteg" i både Girth- och Height-led,
# rotera för att se anpassningen från olika vinklar

# Hur blev anpassningen?

#-------------------------------------------------------------------------------


# vilken modell tycker du är bäst?








