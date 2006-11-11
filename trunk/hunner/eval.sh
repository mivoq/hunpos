awk '
BEGIN{FS="\t"}
{
	error = 0;
	n++ ;
	if ($1 != $4) {v++ ; error+=1}
	if ($2 != $4) {m++; error +=2}
	a[error]+=1 

} END{
print "vit hiba: " v " maxent hiba " m " " n

	print "            vit nem \t igen  ";
	print "maxent nem:  " a[0] "\t\t" a[1]
	print "maxent igen: "a[2] "\t\t" a[3]
	 
}
'