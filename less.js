var path = require('path');
var rw = require('rw');
var less = require('less');

var print_error = function (e) {
  less.writeError(e);
  process.exit(2);
}

try {
  var opt = JSON.parse(rw.readFileSync('/dev/stdin', 'utf8'));

  var print_tree = function (e, tree) {
    if (e)
      print_error(e);
    var css = tree.toCSS(opt.css);
    rw.writeFileSync('/dev/stdout', css, 'utf8');
  }

  var parse_data = function (e, data) {
    if (e)
      print_error(e);
    new(less.Parser)(opt.parser).parse(data, print_tree)
  }

  rw.readFile(path.resolve(process.cwd(), opt.src), 'utf8', parse_data);
} catch (e) {
  print_error(e);
}
