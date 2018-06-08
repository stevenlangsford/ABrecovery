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

function rnorm(mu,sigma) {
    var u = 0, v = 0;
    while(u === 0) u = Math.random(); //Converting [0,1) to (0,1)
    while(v === 0) v = Math.random();
    return (Math.sqrt( -2.0 * Math.log( u ) ) * Math.cos( 2.0 * Math.PI * v ))*sigma+mu;
}

var stimobj = function(optionlist, stimid){
    //an option is {
    //width, (0-1 proporiton of  max)
    //height,(0-1 proporiton of  max)
    //role,   (one of Targ,Comp,Decoy for target, competitor, decoy) 
    //shape_type (one of 0,1,2 . Match-status should be (0,0,0) (0,0,1) (0,1,0) (1,0,0) or (0,1,2) mapping to shapes is randomized at draw time.
    //}
    this.options = optionlist; //makes options visible to inspection via the console.
    
    this.drawme=function(targdiv){
	var size=200;//stim max size. height, width are given in 0-1 then scaled by this.
	var canvassize = 201; //used for both width & height
	
	function drawTriangle(canvas,mywidth,myheight,apexdisplacement){//apexdisplacement is as proportion of mywidth (preserves similarity)
	    //	var canvas = document.getElementById('mycanvas');
	    if (canvas.getContext) {
		var ctx = canvas.getContext('2d');
		//this aligns the centers, so you can judge sizes by checking y-of-base. Bad.
		//TODO Either keep centering and change to triangle format, or keep line and add jitter to baseline.
		var minx =canvas.width/2.0-mywidth/2.0;
		var miny =canvas.height/2.0+myheight/2.0;
		var midx = minx+apexdisplacement*mywidth;

		ctx.beginPath();
		ctx.moveTo(minx,miny);
		ctx.lineTo(midx, miny-myheight);
		ctx.lineTo(minx+mywidth, miny);
		ctx.fill();
	    }
	}//end centerTriangle

	//write three canvases to the targdiv (and later, probably buttons too. Maybe in a triangle shape rather than a linear one?)
	document.getElementById(targdiv).innerHTML="<table style='border:solid 3px black'>"+//haha, tables. Oh dear.
	"<tr><td colspan='2' align='center'>"+
	    "<canvas id='leftcanvas"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+
	    "</td></tr>"+
	    "<tr>"+
	    "<td>"+"<canvas id='middlecanvas"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+"</td>"+
	    "<td>"+"<canvas id='rightcanvas"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+"</td>"+
	    "<tr>"+
	    "</table>";

	//do the randomization of screen location
	optionlist = shuffle(optionlist)
	//do the randomization of shape mapping
	var shapetype_settings = shuffle([.25,.5,.75]) //apex location as proportion of basewidth: left, center, and right leaning triangles. Indexed into by option 'shape' attribute. Depends on max-size, but not individual stim width&height. hmmm.

	drawTriangle(document.getElementById('leftcanvas'+stimid),optionlist[0].width*size,optionlist[0].height*size,shapetype_settings[optionlist[0].shape])
	drawTriangle(document.getElementById('middlecanvas'+stimid),optionlist[1].width*size,optionlist[1].height*size,shapetype_settings[optionlist[1].shape])
	drawTriangle(document.getElementById('rightcanvas'+stimid),optionlist[2].width*size,optionlist[2].height*size,shapetype_settings[optionlist[2].shape])

    }//end drawme

}//end stimobj


//convenience functions for generating trial types
function randomOption(){
//    return {width:rnorm(80,2),height:rnorm(50,2),role:"Filler",shape:0}//height and width seeds taken from trueblood 2013
    return {width:rnorm(.5,.2),height:(.5,.2),role:"Filler",shape:0}//height and width seeds taken from trueblood 2013
}
function cloneOption(anoption){
    return {width:anoption.width, height:anoption.height,role:anoption.role,shape:anoption.shape}
}
function displaceClone(anoption,delta_width,delta_height){ //deltas multiply originals, so expressed as "proportion change". Creates decoys or good options.
    return {width:anoption.width*delta_width, height:anoption.height*delta_height,role:anoption.role,shape:anoption.shape}
}

function mirrorClone(anoption){ //flips width and height to create a distant equal-value competitor

    //    return {width:-anoption.height, height:anoption.width,role:anoption.role,shape:anoption.shape}
} //actually what you want is not the mirror but rotation by 90. Not the same thing at all.

function similaritySlide(anoption, delta){ //height multiplied by delta, width by 1/delta, preserves area, ie slide along equivalue line
    return {width:anoption.width*(1/delta), height:anoption.height*delta,role:anoption.role,shape:anoption.shape}
}

function getShapeFlavours(atriad){ //atrial is a list of three options. Returns a list of triads.
    function setShapes(o1,o2,o3,s1,s2,s3){
	var newo1 = cloneOption(o1);
	var newo2 = cloneOption(o2);
	var newo3 = cloneOption(o3);
	newo1.shape=s1;
	newo2.shape=s2;
	newo3.shape=s3;
	return [newo1,newo2,newo3];
    }
    var option1=atriad[0]; var option2=atriad[1]; option3=atriad[2];
    return [
	setShapes(option1,option2,option3, 0,0,0), //all match
	setShapes(option1,option2,option3, 0,0,1), //odd one out: for each role.
	setShapes(option1,option2,option3, 0,1,0),
	setShapes(option1,option2,option3, 1,0,0),
	setShapes(option1,option2,option3, 0,1,2)  //no match
    ]
}

//trial types
function oneWinnerTrial(){
    var init = randomOption();
    init.role='A'
    var comp = mirrorClone(init);//similaritySlide(init,1.1);
    comp.role='B'
    var winner = Math.random()<.5 ? displaceClone(init,1.2,1.2) : displaceClone(comp,1.2,1.2);
    winner.role='winner'
    return [init,comp,winner];
}

function attractionDecoyTrial(){
    var init = randomOption();
    init.role='Targ'
    var comp = mirrorClone(init);//similaritySlide(init,1.2);
    comp.role='Comp'
    var winner = displaceClone(init,.8,.8)
    winner.role='attractiondecoy'
    return [init,comp,winner];
}


//sandpit area main()

var attractionDecoy = getShapeFlavours(attractionDecoyTrial());
document.write("<h2>Attraction decoy</h2>")
for(i=0;i<attractionDecoy.length;i++){
    document.write("<div id='attraction"+i+"'></div>"+
		   "<div>"+attractionDecoy[i][0].role+" "+attractionDecoy[i][0].shape+"</div>"+
		   "<div>"+attractionDecoy[i][1].role+" "+attractionDecoy[i][1].shape+"</div>"+
		   "<div>"+attractionDecoy[i][2].role+" "+attractionDecoy[i][2].shape+"</div><hr>");
    new stimobj(attractionDecoy[i],"attractionDecoy"+i).drawme("attraction"+i);
}


var singlewinner = getShapeFlavours(oneWinnerTrial());
document.write("<h2>Single winner</h2>")
for(i=0;i<singlewinner.length;i++){
    document.write("<div id='singlewinner"+i+"'></div>"+
		   "<div>"+singlewinner[i][0].role+" "+singlewinner[i][0].shape+"</div>"+
		   "<div>"+singlewinner[i][1].role+" "+singlewinner[i][1].shape+"</div>"+
		   "<div>"+singlewinner[i][2].role+" "+singlewinner[i][2].shape+"</div><hr>");
    new stimobj(singlewinner[i],"singlewinner"+i).drawme("singlewinner"+i);
}
