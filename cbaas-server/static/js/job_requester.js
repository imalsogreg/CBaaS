"use strict";


function job_request() {

    var expParts = funAndArgs( $('#expr-text').value() );

}

function exprFunAndArg(s) {
    var i = s.indexOf(' ');
    fName = s.substr(0,i);
    fArg  = s.substr(i+1);
    return [fName, fArg];
}


function callFun(){
    var fName = $('#fun-name')[0].value;
    var fArg  = $('#fun-arg')[0].value;
    if (functions[fName]) {
        var workersForFn = functions[fName];
        var url = 'api1/callfun?worker-id=' + workersForFn[0];

        // if (mybrowserid != undefined) {
        //     url = url + '&browser-id=' + mybrowserid;
        // }
        var argVar = {tag:'VText',
                      contents: fArg
                     };
        $.ajax(url,
               {'success':renderTicketNumber,
                'failure':renderError,
                'headers':{'Accept':'application/json',
                           'Content-Type':'application/json'},
                'data':JSON.stringify({'function':fName, 'arg': argVar}),
                'dataType':'json',
                'method':'Post'});
    }
}

function renderTicketNumber(d,tStatus,jqxhr){
    console.log('renderResult:');
}

function renderError(e) {
    console.log('error');
    console.log(e);
}

var loadedpic = undefined;

function updatePicLoaded(){
    var placeholder_url =  "https://placeholdit.imgix.net/~text?txtsize=33&txt=350%C3%97150&w=350&h=150";
    var picurl = $('#pic-fun-arg')[0].value;
    var tmpimg = $('#tmpimg');
    var tmpcanvas = $('#imgcanvas');
    $.ajax(picurl,
           {'success': function(d) {
               tmpimg[0].crossOrigin = "Anonymous";
               tmpimg.attr('src',picurl);
           },
           'error': function(d){
               tmpimg.attr('src',placeholder_url);
           }
           });
}

function picLoaded(){
    var tmpimg = $('#tmpimg');
    var tmpcanvas = $('#imgcanvas');
    tmpimg[0].crossOrigin = "Anonymous";
    var w = tmpimg[0].naturalWidth;
    var h = tmpimg[0].naturalHeight;
    console.log(w);
    console.log(h);
    var ctx = tmpcanvas[0].getContext('2d');
    ctx.canvas.width = w;
    ctx.canvas.height = h;
    ctx.drawImage(tmpimg[0],0,0);
    console.log("DRAWING RECT!");
    var occWid = parseInt( $('#occlude')[0].value );
    ctx.fillStyle = "#00AAAA";
    ctx.fillRect(5,5,occWid,400);
}

var d; // for debugging

function callPicFun(){
    var fName = $('#pic-fun-name')[0].value;
    var canv  = $('#imgcanvas');
    console.log(fName);
    console.log(functions[fName]);
    if (functions[fName]) {
        var workersForFn = functions[fName];
        // var pixData = getImgBytes();
        // var fArg  = {};
        // fArg = urlDataToVal(pixdata);
        // fArg.tag = "VImage";
        // fArg.contents = pixData.substring(pixData.indexOf(',')+1);
        var fArg = urlDataToVal( getImgBytes() );
        console.log(fArg);
        var url = 'api1/callfun?worker-id=' + workersForFn[0];
        // if (mybrowserid != undefined) {
        //     url = url + '&browser-id=' + mybrowserid;
        // }
        d = JSON.stringify({'function':fName,'arg':fArg});
        var dl = d.length;
        $.ajax(url,
               {'success':function(d){myjob = d; console.log(myjob);},
                'failure':renderError,
                'headers':{'Accept':'application/json',
                           'Content-Type':'application/json'
                          },
                'data':JSON.stringify({'function':fName, 'arg': fArg}),
                'dataType':'json',
                'method':'Post'});
    }
}

function urlDataToVal(urlBytes){
    var pixdata = urlBytes.substring(urlBytes.indexOf(',')+1);
    return {'tag':'VImage',
            'contents':{'tag':'ModelImage',
                        'contents': pixdata
                       }
           };
}

function getImgBytes(){
    var tmpimg = $('#tmpimg');
    var tmpcanvas = $('#imgcanvas');
    tmpimg[0].crossOrigin = "Anonymous";
    var ctx = tmpcanvas[0].getContext('2d');
    return (tmpcanvas[0].toDataURL('image/png'));
}
