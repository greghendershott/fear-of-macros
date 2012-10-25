# Simply make gh-pages branch a mirror of master branch.
git checkout master && \
git push origin master && \
git checkout gh-pages && \
git merge master && \
git push origin gh-pages && \
git checkout master
