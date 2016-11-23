$x=50;
$y=70;
&interchange (*x, *y);      # Pasaje por referencia
print "x:$x, y:$y\n";

sub interchange{
    (*x1, *y1) = @_;
    $z=$x1;
    $x1=$y1;
    $y1=$z;
    print "interchange = x1:$x1, y1:$y1\n";
}
