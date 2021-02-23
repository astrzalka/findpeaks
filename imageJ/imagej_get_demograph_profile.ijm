// gdzie mają być zapisane wyniki
folder = getDirectory("Wybierz folder");

// ile linii jest w ROI manager
ile = roiManager("count");
// nazwa analizowanego pliku
nazwa = getTitle();
// sprawdź ile kanałów ma plik
getDimensions(w, h, channels, slices, frames);
// jaką wielkość ma jeden piksel
getPixelSize (unit, pixelWidth, pixelHeight);
// pętla zapisująca profil dla każdego kanału
for(k=1; k<=channels; k+=1){
// usuwamy poprzednie wyniki
	run("Clear Results");
	wiersz = 0;
// pętla analizująca wszystkie ROI
	for(i=0; i < ile; i+=1){
// wybieramy odpowiednie ROI
		roiManager("Select", i);
// robimy wykres fluorescencji
		// ustawiamy 
		Stack.setChannel(k);
		wynik = getProfile();
// pętla tworząca kolumnę z wynikami, każda linia to nowa kolumna
		
		for (j=0; j<wynik.length; j++){
 			setResult("Length", wiersz, pixelWidth * j);
 			setResult("Value", wiersz, wynik[j]);
			updateResults();
			wiersz += 1;
		}
	}
// zapisujemy wynik w pliku txt
saveAs("Measurements", folder+"Values"+nazwa+"_C"+k+".txt");
}
// zapisujemy zestaw ROI
roiManager("Save", folder+"RoiSet"+nazwa+".zip");

// lista wszystkich otawrtych obrazów
images = getList("image.titles");

// zapisuje każde analizowane ROI jako osobny tiff do folderu wybranego przez użytkownika 
ile = roiManager("count");
for(i=0; i < ile; i+=1){
	// wybieramy wyjściowy obraz
	selectWindow(images[0]);
	// wybieramy odpowiednie ROI i zapisujemy na którym jest slice
	roiManager("Select", i);
	slice = getSliceNumber();
	
	print(floor((slice/2) + 1));
	// Robimy kwadrat dokoła strzępki i duplikujemy, powiększamy go o 2 mikrony
	run("To Bounding Box");
	run("Enlarge...", "enlarge=2");
	run("Duplicate...", "duplicate slices="+floor((slice/2) + 1));
	

}

// lista wszystkich otawrtych obrazów
images = getList("image.titles");

for(i=1; i < lengthOf(images); i+=1){

	// zaczynamy od 1 żeby pominąć wyjściowy obraz - checmy zapisać tylko te wygenerowane w pętli wyżej
	//print(images[i]);
	save(folder+"image"+images[i]+".tiff");
	selectWindow(images[i]);
	close();
	
}