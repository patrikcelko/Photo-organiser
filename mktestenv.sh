#!/bin/sh

chmod -f +rwx testenv/second/secret testenv/second/blocked testenv/second/fourth
rm -rf testenv

mkdir testenv
cd testenv
mkdir -p first/third/ second/fourth second/secret second/blocked fifth
mkdir -p no-touchy no-touchy/go-away

touch -d "2014-03-04 15:55+00" first/pic1.jpg
touch -d "2011-01-02 12:08+00" second/pic2.jpeg
touch -d "2020-03-02 10:12+00" first/pic3.JPG
touch -d "2020-01-01 00:00+00" first/third/pic4.JPEG
touch -d "2020-01-01 00:01+00" first/samename.jpg
touch -d "1999-11-23 12:37+00" second/pic5.jpg
touch -d "2009-12-31 23:59+00" root.jpg
touch second/fourth/pic6.jpg
touch second/secret.jpg
touch second/secret/picX.jpg
touch second/blocked/picY.jpg
touch second/nonimage.txt
touch second/secret.txt
touch no-touchy/.nomedia
touch no-touchy/lure.jpg
touch no-touchy/go-away/invisible.jpg
for i in $(seq 10); do
    touch fifth/img$i.jpg
done
echo "later" > first/third/samename.jpg

chmod -x second/blocked
chmod -w second/fourth
chmod -r second/secret
chmod -r second/secret.txt
chmod -r second/secret.jpg

echo "2010-01-01  00:00  2020-01-01 00:00   old_decade" >> tidy.conf
echo "2020-01-01  00:00  2020-04-01 00:00   new_decade" >> tidy.conf
echo "2020-04-01  00:00  2030-01-01 00:00   new_decade/recent" >> tidy.conf
