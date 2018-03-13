#!/usr/local/bin/bash
wget -r -np -k http://dmccooey.com/polyhedra

for filename in `find dmccooey.com/polyhedra/*.html -type f`; do
    wget "www."${filename::-5}".txt" -P dmccooey.com/polyhedra/
	rm $filename
done

for filename in `find dmccooey.com/polyhedra/*.js -type f`; do
    rm $filename
done

echo "The txt were successfully downloaded"