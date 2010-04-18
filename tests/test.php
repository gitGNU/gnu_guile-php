Some text before php tag...

<?php

function testFunc1()
{
	$a = 101;

	print "from testFunc1\n";
	print "\n";
	print $a;
	print "\n";
}

function testFunc2 ( ) { print "from testFunc2\n"; }

function testFunc3(
) {
							print "from testFunc3\n";
}

#comment1
/* comment2 */
//comment 3

$x = 1;
$y = 2;

print $x;
print $y;

print "\n";
print "\Z";
print "testing";

print "\n-----------\n";

testFunc1();
testFunc2();
testFunc3();

?>

Some text after php tag..........
