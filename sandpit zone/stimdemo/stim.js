function shuffle(a) { //credit SO
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}

var stimobj = function(optionlist, stimid){
    //an option is {
    //width, (0-1 proporiton of  max)
    //height,(0-1 proporiton of  max)
    //role,   (one of Targ,Comp,Decoy for target, competitor, decoy) 
    //shape_type (one of 0,1,2 . Match-status should be (0,0,0) (0,0,1) (0,1,0) (1,0,0) or (0,1,2) mapping to shapes is randomized at draw time.
    //}
    
    this.drawme=function(targdiv){
	var size=200;//stim max size. height, width are given in 0-1 then scaled by this.
	var canvassize = 300; //used for both width & height
	
	function drawTriangle(canvas,mywidth,myheight,apexdisplacement){
	    //	var canvas = document.getElementById('mycanvas');
	    if (canvas.getContext) {
		var ctx = canvas.getContext('2d');
		//this aligns the centers, so you can judge sizes by checking y-of-base. Bad.
		//TODO Either keep centering and change to triangle format, or keep line and add jitter to baseline.
		var minx =canvas.width/2.0-mywidth/2.0;
		var miny =canvas.height/2.0+myheight/2.0;
		var midx = canvas.width/2.0+apexdisplacement;

		ctx.beginPath();
		ctx.moveTo(minx,miny);
		ctx.lineTo(midx, miny-myheight);
		ctx.lineTo(minx+mywidth, miny);
		ctx.fill();
	    }
	}//end centerTriangle

	//write three canvases to the targdiv (and later, probably buttons too. Maybe in a triangle shape rather than a linear one?)
	document.getElementById(targdiv).innerHTML=
	    "<canvas id='leftcanvas"+stimid+"' width='"+canvassize+"' height='"+canvassize+"' style='border:1px solid black'></canvas>"+
	    "<canvas id='middlecanvas"+stimid+"' width='"+canvassize+"' height='"+canvassize+"' style='border:1px solid black'></canvas>"+
	    "<canvas id='rightcanvas"+stimid+"' width='"+canvassize+"' height='"+canvassize+"' style='border:1px solid black'></canvas>";

	//do the randomization of screen location
	optionlist = shuffle(optionlist)
	//do the randomization of shape mapping
	var shapetype_settings = shuffle([-size/8,0,size/3]) //apex displacement for left, center, and right flavour triangles. Indexed into by option 'shape' attribute. Depends on max-size, but not individual stim width&height. hmmm.

	drawTriangle(document.getElementById('leftcanvas'+stimid),optionlist[0].width*size,optionlist[0].height*size,shapetype_settings[optionlist[0].shape])
	drawTriangle(document.getElementById('middlecanvas'+stimid),optionlist[1].width*size,optionlist[1].height*size,shapetype_settings[optionlist[1].shape])
	drawTriangle(document.getElementById('rightcanvas'+stimid),optionlist[2].width*size,optionlist[2].height*size,shapetype_settings[optionlist[2].shape])

    }//end drawme

}//end stimobj


//convenience functions for generating trial types
function allEqualTrial(width,height,shapes){
    return [{width:width,height:height,role:"Filler=",shape:shapes[0]},
	    {width:width,height:height,role:"Filler=",shape:shapes[1]},
	    {width:width,height:height,role:"Filler=",shape:shapes[2]}];
}
function oneWinnerTrial(width,height,shapes){
    return [{width:width,height:height,role:"Filler=",shape:shapes[0]},
	    {width:width*1.3,height:height*1.3,role:"FillerWinner",shape:shapes[1]},
	    {width:width,height:height,role:"Filler=",shape:shapes[2]}];
}

//sandpit area main()

var demoTarg={width:1,height:.9,role:"Filler=",shape:"0"}
var demoComp={width:0.9,height:1,role:"Filler=",shape:"0"}
var demoDecoy={width:0.9,height:1,role:"Filler=",shape:"0"}

var bob = new stimobj([demoTarg,demoComp,demoDecoy],"demo");
bob.drawme("uberdiv");
