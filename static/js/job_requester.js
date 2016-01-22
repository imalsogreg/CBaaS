funs = [];

$('document').ready(function (){

    $.ajax('workers', {'success': function(dat){
        console.log(dat);
    }, 'failure': function(e) {
        console.log(e);
    }});

});

function job_request() {

    var expParts = funAndArgs( $('#expr-text').value() );

}

function funAndArg(s) {
    var i = s.indexOf(' ');
    fName = s.substr(0,i);
    fArg  = s.substr(i+1);
    return [fName, fArg];
}
