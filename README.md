# Environment-Image-Recognition
Implements various machine learning models to predict whether the image was taken outdoors or not.

For further details of this project, see [report](Documents/Image%20Recognition%20Report.pdf) .

Dataset: Photos were taken from http://www.ee.columbia.edu/ln/dvmm/downloads/PIM_PRCG_dataset/

## How to run
1. Save and unzip the images folder found on columbia.edu onto your computer.
2. Load and run partition scripts, ensure you update the folder path in the R scripts to your directory.
3. Verify the .txt files are saved in your current working directory (there should be 4 files).
4. Run the model scripts, ensure once again the folder path in the R scripts is updated to your directory.
5. Accuracy results will be printed and plotted on your screen.

Models Scripts:

* SupportVectorMachine.R
* PenalizedLogisticRegression3x3.R
* PenalizedLogisticRegression.R
* ConvolutionalNeuralNetwork.R

Pixel Summarization/Partition Scripts

* Partitions/3x3MeanGrid.R
* Partitions/3x3MedianGrid.R
* Partitions/MeanPixels.R
* Partitions/MedianPixels.R
