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

	return $a;
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
else
  print "single line else after multi line if\n";


function testReturn()
{
	return "return value from testReturn\n";
}

$testFuncRes = testReturn();

print "testReturn returned: ";
print $testFuncRes;
print "\n";

?>

Some text after php tag..........


<?php

$r = 5;

print ++$r;
print "\n";
print --$r;
print "\n";
print $r;
print "\n";

if($r < 10)
  print "r is less than 10\n";

if($r > 1)
  print "r is greater than 1\n";

if($r < 0)
  print "this is not correct!\n";

if($r <= 5)
  print "r is <= 5\n";

if($r >= 5)
  print "r is >= 5\n";

if($r <= 3)
  print "not right\n";

if($r >= 100)
  print "really wrong\n";

?>

<h1>After another php block..</h1>

<?php if(2 == 1) { ?> ok..... <?php } else { ?> hrm.... <?php } ?>

<hr />
<p><?php echo "testing echo....\n"; ?></p>
<h2><?= $r ?></h2>
<h3><%= ++$r %></h3>

<script language="php">

for($loopVar = 0; $loopVar < 10; ++$loopVar)
{
	print $loopVar;
	print "\n";
}

print "loopVar is: ";
print $loopVar;
print "\n\n";

while($loopVar > 0)
{
	print --$loopVar;
	print "\n";
}

print "loopVar is: ";
print $loopVar;
print "\n\n";

do {
   print "should only see this line once...\n";
} while($loopVar > 0);

</script>

Switch statements....

<?php

$i = 3;
$x = 5;

switch($i) {}
switch($i) { default: print 'pointless but works'; }

switch($i)
{
case 1:
case 2:
     break;

case 3: print '$i matches 3'; print "\n"; break; 
default: print "shouldn't see this printed out\n";
}

switch($x)
{
case 1:
case 2:
case 3:
case 4:
default:
print '$x is not 1 - 4, its: ';
print $x;
print "\n";
}

?>