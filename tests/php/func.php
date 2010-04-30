Testing function calls...

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

print "\n\n**************\n\n";

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

testFunc1();
testFunc4(1, "testing", 555); 


?>
