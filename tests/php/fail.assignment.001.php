<?php 
$a = 'k'; 
$a .= $a;
$a = 'j' . $a;
$b = $a . 'r';
print $a . $b; //jkkjkkr 
?>

