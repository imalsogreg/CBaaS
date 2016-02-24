"use strict";

// expose the websockets connection for debugging
var w = undefined;

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
    new_uri += loc.pathname + "api1/browse"
    w = new WebSocket(new_uri);
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
            $('body').append("<br/>" + msg.contents[1].value.contents);
        }
        listWorkers();
    };

});

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
