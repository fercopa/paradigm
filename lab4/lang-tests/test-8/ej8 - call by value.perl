$x=50;
$y=70;
&interchange ($x, $y);
print "x:$x, y:$y\n";

sub interchange{
    ($x1, $y1) = @_;
    $z=$x1;
    $x1=$y1;
    $y1=$z;

    $_[0] = $x1;
    $_[1] = $y1;
    print "interchange = x1:$x1, y1:$y1\n";
}
