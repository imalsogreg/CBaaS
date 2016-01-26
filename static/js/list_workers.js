"use strict";

// expose the websockets connection for debugging
var w = undefined;

var mybrowserid = undefined;
var workers   = {};
var functions = {};

$('document').ready(function (){
    w = new WebSocket('ws://localhost:9160/browser');
    w.onmessage = function (m) {
        console.log(m.data);
        var msg = JSON.parse(m.data);
        // console.log(msg);
        var thisFunc = msg.contents[1].function;
        console.log(thisFunc);
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
                filter( functions[thisFunc].filter(function(i) {
                    return (i != msg.contents);
                }));
        } else if (msg.tag === "SetBrowserID") {
            mybrowserid = msg.contents;
        } else if (msg.tag == "JobFinished") {
            console.log(msg.contents);
            $('body').append("<br/>" + msg.contents[1].value);
        }
        console.log(workers);
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
