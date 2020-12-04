var fs = require ('fs');

var pips_per_die = 4
var die_rx = /^([0-9]+)[Dd](\+([1-3]))?$/;

function dice_to_cost (stat) {
    var match = die_rx.exec (stat);
    if (match != null) {
	dice = parseInt (match[0], 10);
	if (match[3] != null) { 
	    pips = parseInt (match[3], 10);
	} else {
	    pips = 0;
	}
	return (dice * 4) + pips;
    } else {
	throw 'Invalid Stat: ' + stat
    }
}

function cost_to_dice (cost) {
    var dice = Math.floor (cost / pips_per_die);
    var pips = cost % pips_per_die;
    var s = dice.toString () + 'D';
    if (pips != 0)
	s = s + '+' + pips.toString ();
    return s;
}

var args = process.argv.slice(2);
console.log ('args: ', args);

var characters_file = fs.readFileSync (args[0]);

console.log ('Characters_file: %s', characters_file);
var characters = JSON.parse (characters_file);

console.log ('Characters: %s', characters);

for (var i = 0; i < characters.length; i++) {
    var character = characters[i];
    console.log ('Character %d: %s', i, JSON.stringify (character));
    
}


var cost = dice_to_cost ('3D+3');
console.log ('cost: %d', cost);
console.log ('dice: %s', cost_to_dice (cost));
