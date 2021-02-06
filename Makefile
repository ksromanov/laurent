laurent: Laurent.dcl Laurent.icl FieldGF2.dcl FieldGF2.icl FieldGF3.dcl FieldGF3.icl FieldGF127.dcl FieldGF127.icl main.icl
	clm -I /usr/lib64/clean/Platform/ -I /usr/lib64/clean/Gast/ -h 6000M main -o laurent

clean:
	rm -f laurent
	rm -rf "Clean System Files"
