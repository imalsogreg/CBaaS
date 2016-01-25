var w = undefined;

var workers = {};

$('document').ready(function (){
    w = new WebSocket('ws://localhost:9160/browser');
    w.onmessage = function (m) {
        console.log(m.data);
        var msg = JSON.parse(m.data);
        // console.log(msg);
        if (msg.tag == "WorkerJoined") {
            workers[msg.contents[0]] = msg.contents[1];
        } else if (msg.tag === "WorkerLeft") {
            console.log(msg.contents);
            console.log( typeof(msg.contents) );
            console.log( "DELETING KEY " + msg.contents);
            delete workers[msg.contents];
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
    // cs.forEach(function (n) {
    //     workersList.removeChild(n);
    // });
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
