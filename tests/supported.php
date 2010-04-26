Some text before php tag...

<?php

## PHP for GNU Guile

# Copyright (C) 2010 Jon Herron

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA


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

function testFunc4($paramA, $paramB, $paramC) {
  print "Inside testFunc4....\n";
  print "---------\n";
  print $paramA;
  print "\n";
  print $paramB;
  print "\n";
  print $paramC;
  print "---------\n";
}

#comment1
/* comment2 */
//comment 3

$a = null;
$x = 11111;
$y = 2;
$z = "some string var value";

print $a;
print $x;
print $y;
print $z;

print "\n";
print "\Z";
print "testing";

print "\n-----------\n";

testFunc1();
testFunc2();
testFunc3();
testFunc4(1, "testing", 555); 

if ($x == 10) {
  testFunc1();
} else 
{
  testFunc4("from", "else", "statement");
}

if($y == 2) {
  testFunc2();
} else {
  testFunc3();
}

if($y == 2)
{
  testFunc4(90,12,41);
}

if(2 == 2)
  print "one line if statement...\n";

if(3 == 2)
  print "weird...\n";
else
  print "one line else...\n";

if(1 == 1) {
  print "multi\n";
  print "line\n";
  print "if\n";
}
#else
#  print "single line else after multi line if\n";

?>

Some text after php tag..........
