#!/bin/bash
currDir=$(basename $(pwd))
if [ "$currDir" == "Tests" ]
then
	cd ..
fi

make || { echo 'Build failed!' ; exit 1; }

cd Tests
for path in ./*; do
	[ -d "${path}" ] || continue # if not a directory, skip
	dirname="$(basename "${path}")"
	
	inPath=$(ls "$dirname"/in.*)
	correctPath=$(ls "$dirname"/correct.*)
	correctName=$(basename "$correctPath")
	extension="${correctName##*.}"
	outPath="$dirname/out.$extension"

	echo "Testing: ../wrangell $dirname/run.wl $inPath $outPath"
	../wrangell "$dirname/run.wl" "$inPath" "$outPath"

	diff $correctPath $outPath > /tmp/thediff 2>&1
   	if [ $? != 0 ]
   	then
    	cat /tmp/thediff
   	fi

   	rm $outPath


done
