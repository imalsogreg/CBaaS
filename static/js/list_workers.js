var w = undefined;

$('document').ready(function (){
    w = new WebSocket('ws://localhost:9160/browser');
    w.onmessage = function (m) { console.log(m.data);};

});

// function listWorkers(){
//     var workersList = $('#workers');
//     $.ajax('/')
// }
