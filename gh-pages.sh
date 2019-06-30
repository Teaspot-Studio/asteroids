mkdir .temp || true 
cp front/statics/js/all.min.js .temp/all.min.js 
git checkout gh-pages
mv .temp/all.min.js js/all.min.js
git add js/all.min.js
git commit -m "Update"
git push origin gh-pages
git checkout master
rm -rf .temp || true
