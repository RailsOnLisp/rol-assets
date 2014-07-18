var path = require('path');
var fs = require('fs');
var less = require('less');

var print_error = function (e) {
  less.writeError(e);
  process.exit(2);
}

try {
  process.stdin.setEncoding('utf-8');
  process.stdin.on('readable', function () {
    var opt = JSON.parse(process.stdin.read());

    var print_tree = function (e, tree) {
      if (e)
	print_error(e);
      var css = tree.toCSS(opt.css);
      process.stdout.write(css);
    }

    var parse_data = function (e, data) {
      if (e)
	print_error(e);
      new(less.Parser)(opt.parser).parse(data, print_tree)
    }

    fs.readFile(path.resolve(process.cwd(), opt.src), 'utf8', parse_data);
  });
} catch (e) {
  print_error(e);
}
