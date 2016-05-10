"use strict";

// expose the websockets connection for debugging
var w = undefined;
var myjob = undefined;

var mybrowserid = undefined;
var workers   = {};
var functions = {};

$('document').ready(function (){
    var loc = window.location, new_uri;
    if (loc.protocol === "https:") {
      new_uri = "wss:";
    } else {
      new_uri = "ws:";
    }
    new_uri += "//" + loc.host;
    new_uri += loc.pathname + "api1/browserupdates",
    w = new WebSocket(new_uri);
    w.onclose = function(m) {
        console.log("CLOSED: " + m);
    }
    w.onerror = function(event){
        console.log("ERROR");
    }
    w.onopen = function(event){
        console.log("OPEN");
    }
    w.onmessage = function (m) {
        console.log(m);
        var msg = JSON.parse(m.data);
        var thisFunc = msg.contents[1].function;
        if (msg.tag == "WorkerJoined") {
            workers[msg.contents[0]] = msg.contents[1];
            if(functions[thisFunc]) {
                functions[thisFunc].push(msg.contents[0]);
            } else {
                functions[thisFunc] = [msg.contents[0]];
            }
        } else if (msg.tag === "WorkerLeft") {
            thisFunc = workers[msg.contents].function;
            delete workers[msg.contents];
            functions[thisFunc] =
                 functions[thisFunc].filter(function(i) {
                    return (i != msg.contents);
                });
        } else if (msg.tag === "SetBrowserID") {
            mybrowserid = msg.contents;
        } else if (msg.tag == "JobFinished") {
            console.log('JobFinished');
            console.log(msg.contents);
            fetchAndPrintResult(msg.contents);
        }
        listWorkers();
        console.log('Finished onmessage handler');
    };

});

function fetchAndPrintResult(resultID) {
    if(resultID == myjob){
        console.log('JOB MATCH - RUN AJAX');
        var url = 'api1/jobresult?job-id=' + resultID;
        console.log('URL: ' + url);
        $.ajax(url,
               {'success': function(r) {
                   var res = r;
                   console.log("FOUND RESULT");
                   console.log(res);
                   $('body').append("<br/>" + res.value.contents);
               },'error': function(e) {
                   console.log('ERROR');
                   console.log(e);
               },'failure':function(e) {console.log('FAILURE: ' + e);
               },'headers':{'Accept':'application/json',
                            'Content-Type':'application/json'
                           },
                'method':'Get',
                'dataType':'json',
                'data':'{}'
               }
              );
    } else {
        console.log("This is someone else's job: " + resultID);
    }
}

var t;

function listWorkers(){
    var workersList = $('#workers');
    t = workersList[0];
    while (workersList[0].firstChild){
        workersList[0].removeChild(workersList[0].firstChild);
    };
    for (var k in workers) {
        if (workers.hasOwnProperty(k)) {
            makeItem(k, workers[k]);
        }
    }
};

function makeItem(i,w){
    var workerTable = $('#workers');
    var r = document.createElement('tr');
    var dI = document.createElement('td');
    dI.innerHTML = "<span>" + i + "</span>";
    r.appendChild(dI);
    for (var k in w) {
        if (w.hasOwnProperty(k)) {
            var aTd = document.createElement('td');
            aTd.innerHTML = w[k];
            r.appendChild(aTd);
        }
    };
    workerTable[0].appendChild(r);
};
