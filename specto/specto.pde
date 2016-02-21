

void setup (){
 int cellW = 500; //cell width
 int cellH = 2;  //cell height 
 int freqBins = 200; //number of frequency bins
 int timeBins = 1;  //numer of time bins
 size(cellW*timeBins, cellH*freqBins);
 background(255);
 String[] lines = loadStrings("mono1000.txt");
//iterate through each time bin
for(int i =0;i<lines.length;i++){
  String myLine = lines[i];
  String[] num = split(myLine, ',');
  println(num.length);
  //iterate through each frequency bin
  for(int j = 0;j<freqBins;j++){
    println(j);
    if(num[j] != null){
    float c = magToColor(Float.parseFloat(num[j])); //color of cell
    println(c);
    stroke(c);
    fill(c);
    rect(i*cellW,height - j*cellH,cellW,cellH); 
    }
  }
 }
}

float magToColor(float f){
 return f/200; 
}
