"use strict";

var myjobs    = {};

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
    console.log(fName);
    console.log(functions[fName]);
    if (functions[fName]) {
        var workersForFn = functions[fName];
        var fArg  = $('#fun-args')[0].value;
        var url = 'api1/callfun?worker-id=' + workersForFn[0];
        if (mybrowserid != undefined) {
            url = url + '&browser-id=' + mybrowserid;
        }
        $.ajax(url,
               {'success':renderResult,
                'failure':renderError,
                'headers':{'Accept':'application/json',
                           'Content-Type':'application/json'},
                'data':JSON.stringify({'function':fName, 'arg': fArg}),
                'dataType':'json',
                'method':'Post'});
    }
}

function renderResult(d,tStatus,jqxhr){
    console.log('success');
    console.log(d);
    $('body').append(d);
}

function renderError(e) {
    console.log('error');
    console.log(e);
}
