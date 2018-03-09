# Mushroom Decision Tree

## How to run
### เปลี่ยน absolute path บรรทัดที่ 10 ที่ parameter file
`data <- read.csv(file="/Users/golf/Desktop/R/mushrooms.csv",header = TRUE)`

### Visualize All Information Gain 
Run this on console 
`Attribute_Selection(data)`
เพื่อดู graph ของ Information Gain 

### Display Mushroom Tree
Run this on console
`plot(mushroom_tree)`

### Display Rpart Tree Model
Run this on console
`rpart.plot(fit)`

### Display Predicted From Rpart
Run this on console
`plot(pred_r)`
