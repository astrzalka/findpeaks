// where data should be saved
folder = getDirectory("Choose directory");

// number of lines in ROI Manager
ile = roiManager("count");
// name of analyzed picture
nazwa = getTitle();
// checking number of channels
getDimensions(w, h, channels, slices, frames);
// getting pixel size
getPixelSize (unit, pixelWidth, pixelHeight);
// loop saving the profile for each channel
for(k=1; k<=channels; k+=1){
// clean results from previus runs
	run("Clear Results");
	wiersz = 0;
// loop analyzing all ROIs
	for(i=0; i < ile; i+=1){
// choose ROI
		roiManager("Select", i);
// get fluorescence profile from the rigth channel
		Stack.setChannel(k);
		wynik = getProfile();
// loop creating result table
		
		for (j=0; j<wynik.length; j++){
 			setResult("Length", wiersz, pixelWidth * j);
 			setResult("Value", wiersz, wynik[j]);
			updateResults();
			wiersz += 1;
		}
	}
// save result to txt
saveAs("Measurements", folder+"Values"+nazwa+"_C"+k+".txt");
}
// save ROI
roiManager("Save", folder+"RoiSet"+nazwa+".zip");

// list of all open images
images = getList("image.titles");

// save  last ROI as tiff stack
ile = roiManager("count");

	// choose the first image
	selectWindow(images[0]);

	// choose the first ROI - first image in new stack
	roiManager("Select", 0);
	first = getValue("Frame");
	print(first);

	// choose last ROI - last image
	roiManager("Select", ile-1);	
	last = getValue("Frame");
	print(last);
	
	// make rectangle around hypha, enlarge it by 2 microns and duplicate to new stack
	run("To Bounding Box");
	run("Enlarge...", "enlarge=2");
	run("Duplicate...", "duplicate frames="+first+"-"+last);
	

images = getList("image.titles");

// save all new images
for(i=1; i < lengthOf(images); i+=1){

	// zaczynamy od 1 żeby pominąć wyjściowy obraz - checmy zapisać tylko te wygenerowane w pętli wyżej
	//print(images[i]);
	save(folder+"image"+images[i]+".tiff");
	selectWindow(images[i]);
	close();
	
}