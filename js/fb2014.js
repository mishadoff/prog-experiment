var fs=require('fs');
var hljs=require('highlight.js');

var dir='D:/temp/code/';
var lang;
var langEntry
var data={};

fs.readdir(dir, function(err, files){
    if (err) throw err;
    var c=files.length;
    var i=0;
    files.forEach(function(file){
	i++;
        fs.readFile(dir+file,'utf-8',function(err, content){
            if (err) throw err;
            langEntry = hljs.highlightAuto(content);
	    lang = langEntry.language;
	    if (lang in data) {
		data[lang]++;
	    } else {
		data[lang] = 1;
	    }
	    console.log(langEntry);
	    if (i === c) {
		console.log(data);
	    }
        });
    });
});
