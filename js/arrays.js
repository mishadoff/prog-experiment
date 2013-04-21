// Arrays demo

//create array
var a = [1, 2, 8, 9, 4];
println("<br><b>Create</b>");
println("var a = " + pprint(a));

// concat arrays
var b = [9, 3, 6];
println("<br><b>Concat</b>");
println("var a = " + pprint(a));
println("var b = " + pprint(b));
println("a.concat(b) = " + pprint(a.concat(b)));

//interleave
println("<br><b>Join</b>");
println("var a = " + pprint(a));
println("a.join('-') = " + a.join('-'));

// push to array
println("<br><b>Push</b>");
println("var a = " + pprint(a));
a.push(666);
println("a.push(666) = " + pprint(a));

// pop from array
println("<br><b>Pop</b>");
println("var a = " + pprint(a));
var c = a.pop();
println("var c = a.pop()");
println("c = " + c);
println("a = " + pprint(a));


//interleave
println("<br><b>Reverse</b>");
println("var a = " + pprint(a));
println("a.reverse() = " + pprint(a.reverse()));

//shift
println("<br><b>Shift</b>");
println("var a = " + pprint(a));
var c = a.shift();
println("var c = a.shift()");
println("c = " + c);
println("a = " + pprint(a));

//shift
println("<br><b>Slice</b>");
println("var a = " + pprint(a));
var c = a.slice(1, 3);
println("var c = a.slice(1, 3)");
println("c = " + pprint(c));
println("a = " + pprint(a));

// sorting
println("<br><b>Sort</b>");
var a = [4, 42, 16, 15, 23, 8];
println("var a = " + pprint(a));
a.sort();
println("a.sort() = " + pprint(a));
a.sort(function(a, b) { return a - b;});
println("a.sort(function(a, b) { return a - b;}); = " + pprint(a));

//sorting
println("<br><b>Splice</b>");
var a = [4, 42, 16, 15, 23, 8];
println("var a = " + pprint(a));
var c = a.splice(2, 2, [33]);
println("var c = a.splice(2, 2, [33])");
println("c = " + pprint(c));
println("a = " + pprint(a));

// unshift
println("<br><b>Unshift</b>");
println("var a = " + pprint(a));
var c = a.unshift('@');
println("var c = a.unshift('@')");
println("c = " + c);
println("a = " + pprint(a));


// pretty print for array
function pprint(arr) {
    return "[" + arr.join(", ") + "]";
};

function println(s) {
    document.getElementById("result").innerHTML += (s + "<br>"); 
};
